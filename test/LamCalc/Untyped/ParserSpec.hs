{-# LANGUAGE OverloadedStrings #-}

module LamCalc.Untyped.ParserSpec
  ( spec
  ) where

import           Data.Text                   (Text)
import           LamCalc.Untyped.Parser
import           LamCalc.Untyped.Parser.Expr
import           Test.Hspec
import           Text.Megaparsec             (parseMaybe)

sut :: Text -> Maybe Expr
sut = parseMaybe expr

spec :: Spec
spec =
  describe "expr" $ do
    it "parses a variable" $ sut "x" `shouldBe` (Just $ Var "x")
    it "parses I combinator" $ sut "\\x. x" `shouldBe` (Just $ Lam ["x"] (Var "x"))
    it "parses K combinator" $ sut "\\x. \\y. x" `shouldBe` (Just $ Lam ["x"] $ Lam ["y"] $ Var "x")
    it "parses K combinator (multi-param)" $
      sut "\\x y. x" `shouldBe` (Just $ Lam ["x", "y"] (Var "x"))
    it "parses S combinator" $
      sut "\\x. \\y. \\z. x z (y z)" `shouldBe`
      (Just $
       Lam ["x"] $ Lam ["y"] $ Lam ["z"] $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))
    it "parses S combinator (multi-param)" $
      sut "\\x y z. x z (y z)" `shouldBe`
      (Just $ Lam ["x", "y", "z"] $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))
