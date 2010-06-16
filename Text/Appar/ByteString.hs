module Text.Appar.ByteString (
    Parser
  , module Text.Appar.Parser
  ) where

import Text.Appar.Parser
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S hiding (ByteString)

instance Input ByteString where
    car   = S.head
    cdr   = S.tail
    nil   = S.empty
    isNil = S.null

type Parser = MkParser ByteString
