module LamCalc.Untyped.DeBruijn.Expr where

data Expr a
  = Free !a
  | Bound !Int
  | Lam (Expr a)
  | App (Expr a)
        (Expr a)
  deriving (Eq, Show)
