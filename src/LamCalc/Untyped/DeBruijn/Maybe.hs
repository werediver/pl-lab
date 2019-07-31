{-# LANGUAGE LambdaCase #-}

module LamCalc.Untyped.DeBruijn.Maybe where

import           LamCalc.Untyped.DeBruijn.Maybe.Expr
import qualified LamCalc.Untyped.Parser.Expr         as P

desugar :: Eq a => P.Expr a -> Expr a
desugar =
  \case
    P.Var x -> Var x
    P.Lam (x:xs) body -> Lam $ desugar (index (P.Lam xs body) x)
    P.Lam [] body -> desugar body
    P.App f x -> desugar f `App` desugar x
    P.Let x e' body -> Lam (desugar (index body x)) `App` desugar e'
  where
    index :: Eq a => P.Expr a -> a -> P.Expr (Index a)
    index e b =
      case e of
        P.Var x
          | x == b -> P.Var Zero
          | otherwise -> Next <$> e
        P.Lam xs body
          | b `elem` xs -> Next <$> e
          | otherwise -> P.Lam (Next <$> xs) (index body b)
        P.App f x -> index f b `P.App` index x b
        P.Let x e' body
          | x == b -> Next <$> e
          | otherwise -> P.Let (Next x) (index e' b) (index body b)

instantiate :: Eq a => Expr (Index a) -> Expr a -> Expr a
instantiate e arg =
  case e of
    Var Zero     -> arg
    Var (Next x) -> Var x
    Lam body     -> Lam $ instantiate' body (Next Zero) (Next <$> arg)
    App f x      -> instantiate f arg `App` instantiate x arg
  where
    instantiate' ::
         Eq a => Expr (Index (Index a)) -> Index (Index a) -> Expr (Index a) -> Expr (Index a)
    instantiate' e x arg =
      case e of
        Var x'
          | x' == x -> arg
          | Next x'' <- x' -> Var x''
        Var Zero -> Var Zero
        Lam body -> Lam $ instantiate' body (Next x) (Next <$> arg)
        App f x' -> instantiate' f x arg `App` instantiate' x' x arg

abstract :: Expr a -> Expr (Index a)
abstract = undefined

whnf :: Eq a => Expr a -> Expr a
whnf =
  \case
    e@(Var _) -> e
    e@(Lam _) -> e
    App f x ->
      case whnf f of
        Lam body -> whnf $ instantiate body x
        f'       -> f' `App` x

nf :: Eq a => Expr a -> Expr a
nf =
  \case
    e@(Var _) -> e
    Lam body -> Lam $ nf body
    App f x ->
      case whnf f of
        Lam body -> nf $ instantiate body x
        f'       -> nf f' `App` nf x
