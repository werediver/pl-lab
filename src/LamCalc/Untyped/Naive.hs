{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LamCalc.Untyped.Naive where

import           Data.List                   (union, (\\))
import           LamCalc.Untyped.Naive.Expr
import qualified LamCalc.Untyped.Parser.Expr as P

desugar :: P.Expr -> Expr
desugar =
  \case
    P.App f x -> App (desugar f) (desugar x)
    P.Var x -> Var x
    P.Lam (x:xs) e -> Lam x (desugar $ P.Lam xs e)
    P.Lam [] e -> desugar e

freeVars :: Expr -> [VarName]
freeVars (Var x)   = [x]
freeVars (Lam x e) = freeVars e \\ [x]
freeVars (App f x) = freeVars f `union` freeVars x

subst :: VarName -> Expr -> Expr -> Expr
subst targetVar substitution = subst'
  where
    subst' e@(Var x)
      | x == targetVar = substitution
      | otherwise = e
    subst' e@(Lam x e')
      | x == targetVar = e
      | x `elem` substitutionFreeVars =
        let newVarName = safeVarName x (substitutionFreeVars `union` freeVars e')
            e'' = subst x (Var newVarName) e'
         in Lam newVarName (subst' e'')
      | otherwise = Lam x (subst' e')
    subst' (App f x) = App (subst' f) (subst' x)
    safeVarName x blackList =
      let x' = x <> "'"
       in if x' `elem` blackList
            then safeVarName x' blackList
            else x'
    substitutionFreeVars = freeVars substitution

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var x')      = x == x'
alphaEq (Lam x e) (Lam x' e') = alphaEq e (subst x' (Var x) e')
alphaEq (App f x) (App f' x') = alphaEq f f' && alphaEq x x'
alphaEq _ _                   = False

whnf :: Expr -> Expr
whnf e@(Var _) = e
whnf e@(Lam _ _) = e
whnf (App f x) =
  case whnf f of
    Lam x' e' -> whnf (subst x' x e')
    e'        -> e'

nf :: Expr -> Expr
nf e@(Var _) = e
nf (Lam x e') = Lam x (nf e')
nf (App f x) =
  case whnf f of
    Lam x' e' -> nf (subst x' x e')
    f'        -> App (nf f') (nf x)

betaEq :: Expr -> Expr -> Bool
betaEq x y = nf x `alphaEq` nf y
