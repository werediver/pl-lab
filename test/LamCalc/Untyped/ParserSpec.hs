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
    it "parses a parenthised variable" $ sut "(x)" `shouldBe` (Just $ Var "x")
    it "parses an abstraction" $ sut "\\x.x" `shouldBe` (Just $ Lam ["x"] (Var "x"))
    it "parses a parenthised abstraction with parenthised body" $
      sut "(\\x.(x))" `shouldBe` (Just $ Lam ["x"] (Var "x"))
    it "parses a multi-param abstraction" $
      sut "\\x y.x" `shouldBe` (Just $ Lam ["x", "y"] (Var "x"))
    it "parses a var-to-var application" $ sut "f x" `shouldBe` (Just $ App (Var "f") (Var "x"))
    it "parses a lam-to-lam application" $
      sut "\\x.x \\y.y" `shouldBe` (Just $ App (Lam ["x"] (Var "x")) (Lam ["y"] (Var "y")))
