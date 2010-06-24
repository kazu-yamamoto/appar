{-# LANGUAGE OverloadedStrings #-}

{-
This is subset of Parsec.

Parsec 3 provides features which Parsec 2 does not provide:
- Applicative style
- ByteString as input

But Haskell Platform includes Parsec 2, not Parsec 3. Installing
Parsec 3 to Haskell Platform environment makes it mess. So, I have
implemented this.
-}

module Text.Appar.Parser where

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char

----------------------------------------------------------------

data MkParser inp a = P { runParser :: inp -> (Maybe a, inp) }

parse :: MkParser ByteString a -> ByteString -> Maybe a
parse p bs = fst (runParser p bs)

----------------------------------------------------------------

class Eq inp => Input inp where
    car :: inp -> Char
    cdr :: inp -> inp
    nil :: inp
    isNil :: inp -> Bool

----------------------------------------------------------------

instance Functor (MkParser inp) where
    f `fmap` p = return f <*> p

instance Applicative (MkParser inp) where
    pure  = return
    (<*>) = ap

instance Alternative (MkParser inp) where
    empty = mzero
    (<|>) = mplus

instance Monad (MkParser inp) where
    return a = P $ \bs -> (Just a, bs)
    p >>= f  = P $ \bs -> case runParser p bs of
        (Nothing, bs') -> (Nothing, bs')
        (Just a,  bs') -> runParser (f a) bs'
    fail _   = P $ \bs -> (Nothing, bs)

instance MonadPlus (MkParser inp) where
    mzero       = P $ \bs -> (Nothing, bs)
    p `mplus` q = P $ \bs -> case runParser p bs of
        (Nothing, bs') -> runParser q bs'
        (Just a,  bs') -> (Just a, bs')

----------------------------------------------------------------

satisfy :: Input inp => (Char -> Bool) -> MkParser inp Char
satisfy predicate = P sat
  where
    sat bs
      | isNil bs    = (Nothing, nil)
      | predicate b = (Just b,  bs')
      | otherwise   = (Nothing, bs)
      where
        b = car bs
        bs' = cdr bs

try :: MkParser inp a -> MkParser inp a
try p = P $ \bs -> case runParser p bs of
        (Nothing, _  ) -> (Nothing, bs)
        (Just a,  bs') -> (Just a,  bs')

----------------------------------------------------------------

char :: Input inp => Char -> MkParser inp Char
char c = satisfy (c ==)

string :: Input inp => String -> MkParser inp String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

----------------------------------------------------------------

oneOf :: Input inp => String -> MkParser inp Char
oneOf cs = satisfy (`elem` cs)

noneOf :: Input inp => String -> MkParser inp Char
noneOf cs = satisfy (`notElem` cs)

alphaNum :: Input inp => MkParser inp Char
alphaNum = satisfy isAlphaNum

digit :: Input inp => MkParser inp Char
digit = satisfy isDigit

hexDigit :: Input inp => MkParser inp Char
hexDigit = satisfy isHexDigit

space :: Input inp => MkParser inp Char
space = satisfy isSpace

----------------------------------------------------------------

choice :: [MkParser inp a] -> MkParser inp a
choice = foldr (<|>) mzero

option :: a -> MkParser inp a -> MkParser inp a
option x p = p <|> pure x

skipMany :: MkParser inp a -> MkParser inp ()
skipMany p = () <$ many p

skipSome :: MkParser inp a -> MkParser inp ()
skipSome p = () <$ some p

sepBy1 :: MkParser inp a -> MkParser inp b -> MkParser inp [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

manyTill :: MkParser inp a -> MkParser inp b -> MkParser inp [a]
manyTill p end = scan
  where
    scan = [] <$ end <|> (:) <$> p <*> scan
