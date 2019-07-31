{-# LANGUAGE DeriveFunctor #-}

module LamCalc.Untyped.DeBruijn.Maybe.Expr where

data Index a
  = Zero
  | Next !a
  deriving (Eq, Functor, Show)

data Expr a
  = Var !a
  | Lam !(Expr (Index a))
  | App !(Expr a)
        !(Expr a)
  deriving (Eq, Functor, Show)
