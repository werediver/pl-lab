{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module LamCalc.Untyped.Parser.Expr
  ( Expr(..)
  , ExprF(..)
  ) where

import           Data.Functor.Foldable.TH
import           Data.Text.Prettyprint.Doc

data Expr a
  = Var !a
  | Lam ![a]
        (Expr a)
  | App (Expr a)
        (Expr a)
  | Let !a
        (Expr a)
        (Expr a)
  deriving (Eq, Functor, Show)

makeBaseFunctor ''Expr

instance Pretty a => Pretty (Expr a) where
  pretty = pretty' isolated
    where
      pretty' :: ExprPos -> Expr a -> Doc b
      pretty' pos =
        \case
          Var x -> pretty x
          Lam x body ->
            (if isTrailing pos
               then id
               else parens) $
            pretty 'λ' <> hsep (pretty <$> x) <> pretty '.' <+> pretty' isolated body
          App f x ->
            (if isLeading pos
               then id
               else parens) $
            pretty' (nonTrailing pos) f <+> pretty' (nonLeading pos) x
          Let x e body ->
            (if isIsolated pos
               then id
               else parens) $
            pretty "let" <+>
            pretty x <+>
            pretty '=' <+>
            pretty' isolated e <> line <+> pretty "in" <+> align (pretty' isolated body)

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
