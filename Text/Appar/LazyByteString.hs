{-|
Simple 'Applicative' parser whose input is lazy 'ByteString'.
The usage is the same as parsec.

Parsec 3 provides features which Parsec 2 does not provide:

* 'Applicative' style

* 'ByteString' as input

But Haskell Platform includes Parsec 2, not Parsec 3. Installing
Parsec 3 to Haskell Platform environment makes it mess. So, this library
was implemented.

-}

module Text.Appar.LazyByteString (
  -- * Documentation
  -- ** Parser type
    Parser
  , module Text.Appar.Parser
  ) where

import Text.Appar.Parser
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as S hiding (ByteString)

instance Input ByteString where
    car   = S.head
    cdr   = S.tail
    nil   = S.empty
    isNil = S.null

{-|
  Parser synonym for strict 'ByteString'.
-}
type Parser = MkParser ByteString
