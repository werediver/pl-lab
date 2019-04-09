{-# LANGUAGE LambdaCase #-}

module LamCalc.Untyped.Naive where

import           Data.List                   (union, (\\))
import           LamCalc.Untyped.Naive.Expr
import           LamCalc.Untyped.Naive.Ident
import qualified LamCalc.Untyped.Parser.Expr as P

desugar :: P.Expr a -> Expr a
desugar =
  \case
    P.App f x -> App (desugar f) (desugar x)
    P.Var x -> Var x
    P.Lam (x:xs) body -> Lam x (desugar $ P.Lam xs body)
    P.Lam [] e -> desugar e
    P.Let x e body -> App (Lam x (desugar body)) (desugar e)

freeVars :: Ident a => Expr a -> [a]
freeVars (Var x)      = [x]
freeVars (Lam x body) = freeVars body \\ [x]
freeVars (App f x)    = freeVars f `union` freeVars x

subst :: Ident a => a -> Expr a -> Expr a -> Expr a
subst targetVar substitution = subst'
  where
    subst' e@(Var x)
      | x == targetVar = substitution
      | otherwise = e
    subst' e@(Lam x body)
      | x == targetVar = e
      | x `elem` substitutionFreeVars =
        let newVarName = safeVarName x (substitutionFreeVars `union` freeVars body)
            e' = subst x (Var newVarName) body
         in Lam newVarName (subst' e')
      | otherwise = Lam x (subst' body)
    subst' (App f x) = App (subst' f) (subst' x)
    safeVarName x blackList =
      let x' = alter x
       in if x' `elem` blackList
            then safeVarName x' blackList
            else x'
    substitutionFreeVars = freeVars substitution

alphaEq :: Ident a => Expr a -> Expr a -> Bool
alphaEq (Var x) (Var x')            = x == x'
alphaEq (Lam x body) (Lam x' body') = alphaEq body (subst x' (Var x) body')
alphaEq (App f x) (App f' x')       = alphaEq f f' && alphaEq x x'
alphaEq _ _                         = False

whnf :: Ident a => Expr a -> Expr a
whnf e@(Var _) = e
whnf e@(Lam _ _) = e
whnf (App f x) =
  case whnf f of
    Lam x' body' -> whnf (subst x' x body')
    f'           -> App f' x

nf :: Ident a => Expr a -> Expr a
nf e@(Var _) = e
nf (Lam x body') = Lam x (nf body')
nf (App f x) =
  case whnf f of
    Lam x' body' -> nf (subst x' x body')
    f'           -> App (nf f') (nf x)

betaEq :: Ident a => Expr a -> Expr a -> Bool
betaEq x y = nf x `alphaEq` nf y
