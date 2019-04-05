{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LamCalc.Untyped.Naive.Ident where

import           Data.Text (Text)

class Eq a =>
      Ident a
  where
  alter :: a -> a

instance Ident String where
  alter = (<> "'")

instance Ident Text where
  alter = (<> "'")
