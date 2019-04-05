{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module LamCalc.Untyped.Parser.Source where

import           Data.String     (IsString)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Megaparsec (Stream (..))

class (Stream s, Token s ~ Char, IsString (Tokens s), IsString s, Semigroup s, Eq s) =>
      Source s
  where
  fromChar :: Char -> s
  toString :: s -> String

instance Source String where
  fromChar = (: [])
  toString = id

instance Source Text where
  fromChar = T.singleton
  toString = T.unpack
