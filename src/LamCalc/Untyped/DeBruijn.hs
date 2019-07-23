{-# LANGUAGE LambdaCase          #-}
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
        P.Lam (x:xs) body -> Lam $ Scope $ desugar' (P.Lam xs body) (x : bs)
        P.Lam [] body     -> desugar' body bs
        P.App f x         -> App (desugar' f bs) (desugar' x bs)
        P.Let x e body    -> App (Lam $ Scope $ desugar' body (x : bs)) (desugar' e bs)

push :: Expr a -> Expr a
push = push' 0
  where
    push' lamDepth =
      \case
        e@(Free _) -> e
        e@(Bound n)
          | n >= lamDepth -> Bound (n + 1)
          | otherwise -> e
        Lam (Scope body) -> Lam $ Scope $ push' (lamDepth + 1) body
        App f x -> App (push' lamDepth f) (push' lamDepth x)

pull :: Expr a -> Expr a
pull = pull' 0
  where
    pull' lamDepth =
      \case
        e@(Free _) -> e
        e@(Bound n)
          | n > lamDepth -> Bound (n - 1)
          | otherwise -> e
        Lam (Scope body) -> Lam $ Scope $ pull' (lamDepth + 1) body
        App f x -> App (pull' lamDepth f) (pull' lamDepth x)

instantiate :: Expr a -> Scope Expr a -> Expr a
instantiate e (Scope body) = pull $ substBound 0 e body
  where
    substBound :: Int -> Expr a -> Expr a -> Expr a
    substBound target e = substBound'
      where
        substBound' body =
          case body of
            Free _ -> body
            Bound n
              | n == target -> e
              | otherwise -> body
            Lam (Scope body') -> Lam $ Scope $ substBound (target + 1) (push e) body'
            App f x -> App (substBound' f) (substBound' x)

abstract :: a -> Expr a -> Scope Expr a
abstract name body = Scope $ substFree name (Bound 0) (push body)

substFree :: a -> Expr a -> Expr a -> Expr a
substFree = undefined

whnf :: Expr a -> Expr a
whnf e@(Free _) = e
whnf e@(Bound _) = e
whnf e@(Lam _) = e
whnf (App f x) =
  case whnf f of
    Lam body -> whnf $ instantiate x body
    f'       -> App f' x

nf :: Expr a -> Expr a
nf e@(Free _) = e
nf e@(Bound _) = e
nf (Lam (Scope body)) = Lam $ Scope $ nf body
nf (App f x) =
  case whnf f of
    Lam body -> nf $ instantiate x body
    f'       -> App (nf f') (nf x)

betaEq :: Expr a -> Expr a -> Bool
betaEq x y = nf x `betaEq` nf y
