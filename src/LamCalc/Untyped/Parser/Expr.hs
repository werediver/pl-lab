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
  pretty = pretty' isolated
    where
      pretty' :: ExprPos -> Expr -> Doc a
      pretty' pos =
        \case
          Var x -> pretty x
          Lam x e ->
            (if isTrailing pos
               then id
               else parens) $
            pretty 'λ' <> hsep (pretty <$> x) <> pretty '.' <+> pretty' isolated e
          App f x ->
            (if isLeading pos
               then id
               else parens) $
            pretty' (nonTrailing pos) f <+> pretty' (nonLeading pos) x
          Let x e e' ->
            (if isIsolated pos
               then id
               else parens) $
            pretty "let" <+>
            pretty x <+>
            pretty '=' <+>
            pretty' isolated e <> line <+> pretty "in" <+> align (pretty' isolated e')

data ExprPos = ExprPos
  { isLeading  :: Bool
  , isTrailing :: Bool
  }

isIsolated :: ExprPos -> Bool
isIsolated pos = isLeading pos && isTrailing pos

isolated :: ExprPos
isolated = ExprPos {isLeading = True, isTrailing = False}

nonTrailing :: ExprPos -> ExprPos
nonTrailing pos = ExprPos {isLeading = isLeading pos, isTrailing = False}

nonLeading :: ExprPos -> ExprPos
nonLeading pos = ExprPos {isLeading = False, isTrailing = isTrailing pos}
