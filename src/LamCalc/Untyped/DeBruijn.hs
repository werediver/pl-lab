{-# LANGUAGE ScopedTypeVariables #-}

module LamCalc.Untyped.DeBruijn where

import           Data.List                     (elemIndex)
import           LamCalc.Untyped.DeBruijn.Expr
import qualified LamCalc.Untyped.Parser.Expr   as P

desugar ::
     forall a. Eq a
  => P.Expr a
  -> Expr a
desugar e = desugar' e []
  where
    desugar' :: P.Expr a -> [a] -> Expr a
    desugar' e bs =
      case e of
        P.Var x           -> maybe (Free x) Bound (elemIndex x bs)
        P.Lam (x:xs) body -> Lam $ desugar' (P.Lam xs body) (x : bs)
        P.Lam [] body     -> desugar' body bs
        P.App f x         -> App (desugar' f bs) (desugar' x bs)
        P.Let x e body    -> App (Lam $ desugar' body (x : bs)) (desugar' e bs)
