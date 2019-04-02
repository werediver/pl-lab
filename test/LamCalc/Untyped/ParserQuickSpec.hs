{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LamCalc.Untyped.ParserQuickSpec
  ( spec
  ) where

import           Control.Applicative                   (liftA2, liftA3)
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

instance Arbitrary Expr where
  arbitrary = sized tree
    where
      tree 0 = Var <$> varName
      tree n =
        oneof
          [ liftA2 Lam (listOf1 varName) subtree
          , liftA2 App subtree subtree
          , liftA3 Let varName subtree subtree
          ]
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
      LetF x (def, defAlts) (body, bodyAlts) ->
        ((\alt -> Let x alt body) <$> defAlts) <> (Let x def <$> bodyAlts) <>
        [def, body, Var "__shrunkLet"]

varName :: Gen Text
varName = T.singleton <$> choose ('a', 'z')

serialize :: Pretty a => a -> Text
serialize = renderStrict . layoutPretty defaultLayoutOptions . pretty

sut :: Text -> Maybe Expr
sut = parseMaybe expr

spec :: Spec
spec =
  describe "expr" $
  it "parses arbitrary pretty-printed expressions" $ property $ \x -> (sut . serialize) x == Just x
