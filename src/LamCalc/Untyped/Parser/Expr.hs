{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module LamCalc.Untyped.Parser.Expr
  ( Expr(..)
  , ExprF(..)
  , VarName
  ) where

import           Data.Functor.Foldable.TH
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

type VarName = Text

data Expr
  = Var VarName
  | Lam [VarName]
        Expr
  | App Expr
        Expr
  | Let VarName
        Expr
        Expr
  deriving (Eq, Show)

makeBaseFunctor ''Expr

instance Pretty Expr where
  pretty = pretty' Trailing
    where
      pretty' :: ExprPos -> Expr -> Doc a
      pretty' pos =
        \case
          Var x -> pretty x
          Lam x e ->
            (case pos of
               Inner    -> parens
               Trailing -> id) $
            pretty 'λ' <> hsep (pretty <$> x) <> pretty '.' <+> pretty e
          App f x -> pretty' Inner f <+> prettyR x
          Let x e e' ->
            pretty "let" <+>
            pretty x <+> pretty '=' <+> pretty e <> line <+> pretty "in" <+> align (pretty e')
        where
          prettyR e =
            case e of
              App _ _ -> parens $ pretty e
              _       -> pretty' pos e

data ExprPos
  = Inner
  | Trailing
