{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module LamCalc.Untyped.DeBruijn.Expr where

import           Data.Functor.Foldable.TH
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..), parens, (<+>))

newtype Scope f a =
  Scope (f a)
  deriving (Eq, Show)

data Expr a
  = Free !a
  | Bound !Int
  | Lam (Scope Expr a)
  | App (Expr a)
        (Expr a)
  deriving (Eq, Show)

makeBaseFunctor ''Expr

instance Pretty a => Pretty (Expr a) where
  pretty = pretty' Trailing []
    where
      pretty' :: ExprPos -> [String] -> Expr a -> Doc b
      pretty' pos bs =
        \case
          Free x -> pretty x
          Bound n -> pretty (bs !! n)
          Lam (Scope body) ->
            (case pos of
               Inner    -> parens
               Trailing -> id) $
            pretty 'λ' <> pretty x <> pretty '.' <+> pretty' Trailing (x : bs) body
            where x = lamParam (length bs)
                  lamParam depth =
                    "__" <> [az !! (depth `mod` length az)] <>
                    let order = depth `div` length az
                     in if order > 0
                          then show order
                          else mempty
                  az = ['a' .. 'z']
          App f x -> pretty' Inner bs f <+> prettyR bs x
        where
          prettyR bs e =
            case e of
              App _ _ -> parens $ pretty' Trailing bs e
              _       -> pretty' pos bs e

data ExprPos
  = Inner
  | Trailing
