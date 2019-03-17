{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module LamCalc.Untyped.Parser.Expr
  ( Expr(..)
  , ExprF(..)
  , VarName
  ) where

import Data.Functor.Foldable.TH
import Data.Text (Text)

type VarName = Text

data Expr
  = Var VarName
  | Lam [VarName]
        Expr
  | App Expr
        Expr
  deriving (Eq, Show)

makeBaseFunctor ''Expr