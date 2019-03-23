{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LamCalc.Untyped.ParserQuickSpec
  ( spec
  ) where

import           Control.Applicative                   (liftA2)
import           Data.Char
import           Data.Functor.Foldable
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           LamCalc.Untyped.Parser
import           LamCalc.Untyped.Parser.Expr
import           Test.Hspec
import           Test.QuickCheck
import           Text.Megaparsec                       (parseMaybe)

data ExprPos
  = Inner
  | Trailing

instance Pretty Expr where
  pretty = pretty' Trailing
    where
      pretty' :: ExprPos -> Expr -> Doc a
      pretty' pos =
        \case
          Var varName -> pretty varName
          Lam varNames e' ->
            (case pos of
               Inner    -> parens
               Trailing -> id) $
            pretty 'λ' <> hsep (pretty <$> varNames) <> pretty '.' <+> pretty e'
          App f x -> pretty' Inner f <+> prettyR x
        where
          prettyR e =
            case e of
              App _ _ -> parens $ pretty e
              _       -> pretty' pos e

instance Arbitrary Expr where
  arbitrary = sized tree
    where
      tree 0 = Var <$> varName
      tree n = oneof [liftA2 Lam (listOf1 varName) subtree, liftA2 App subtree subtree]
        where
          subtree = tree $ n `div` 2
  shrink =
    para $ \case
      VarF _ -> []
      LamF varNames (e', alts) ->
        (case varNames of
           _:_:_ -> [Lam (init varNames) e']
           _     -> []) <>
        (Lam varNames <$> alts) <>
        [e', Var (head varNames)]
      AppF (lhs, altsL) (rhs, altsR) ->
        (App lhs <$> altsR) <> ((`App` rhs) <$> altsL) <> [lhs, rhs, Var "__shrunkApp"]

varName :: Gen Text
varName = do
  head <- frequency [(4, charThat isLower), (1, pure '_')]
  body <- resize 3 $ listOf $ frequency [(4, charThat isAlphaNum), (1, pure '_')]
  tail <- resize 2 $ listOf (pure '\'')
  return $ T.pack $ head : body <> tail
  where
    charThat = (choose ('\x00', '\x7f') `suchThat`)

serialize :: Pretty a => a -> Text
serialize = renderStrict . layoutPretty defaultLayoutOptions . pretty

sut :: Text -> Maybe Expr
sut = parseMaybe expr

spec :: Spec
spec =
  describe "expr" $
  it "parses arbitrary pretty-printed expressions" $ property $ \x -> (sut . serialize) x == Just x
