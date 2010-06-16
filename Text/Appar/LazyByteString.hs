module Text.Appar.LazyByteString (
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

type Parser = MkParser ByteString
