{-# LANGUAGE TypeSynonymInstances #-}

module Text.Appar.String (
    Parser
  , module Text.Appar.Parser
  ) where

import Text.Appar.Parser

instance Input String where
    car = head
    cdr = tail
    isNil = null
    nil = ""

type Parser = MkParser String
