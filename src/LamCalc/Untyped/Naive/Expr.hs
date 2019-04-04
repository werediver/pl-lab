{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module LamCalc.Untyped.Naive.Expr
  ( Expr(..)
  , ExprF(..)
  ) where

import           Data.Functor.Foldable.TH
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..), parens, (<+>))

data Expr a
  = Var !a
  | Lam !a
        (Expr a)
  | App (Expr a)
        (Expr a)
  deriving (Eq, Show)

makeBaseFunctor ''Expr

instance Pretty a => Pretty (Expr a) where
  pretty = pretty' Trailing
    where
      pretty' :: ExprPos -> Expr a -> Doc b
      pretty' pos =
        \case
          Var x -> pretty x
          Lam x e ->
            (case pos of
               Inner    -> parens
               Trailing -> id) $
            pretty 'λ' <> pretty x <> pretty '.' <+> pretty e
          App f x -> pretty' Inner f <+> prettyR x
        where
          prettyR e =
            case e of
              App _ _ -> parens $ pretty e
              _       -> pretty' pos e

data ExprPos
  = Inner
  | Trailing
