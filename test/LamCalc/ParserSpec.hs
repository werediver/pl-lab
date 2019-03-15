{-# LANGUAGE OverloadedStrings #-}

module LamCalc.ParserSpec
  ( spec
  ) where

import Data.Text (Text)
import LamCalc.Parser
import Test.Hspec
import Text.Megaparsec (parseMaybe)

sut :: Text -> Maybe Expr
sut = parseMaybe expr

spec :: Spec
spec =
  describe "expr" $ do
    it "parses a variable" $ sut "x" `shouldBe` (Just $ Fix $ VarF "x")
    it "parses a parenthised variable" $
      sut "(x)" `shouldBe` (Just $ Fix $ VarF "x")
    it "parses an abstraction" $
      sut "\\x.x" `shouldBe` (Just $ Fix $ LamF ["x"] (Fix $ VarF "x"))
    it "parses a parenthised abstraction with parenthised body" $
      sut "(\\x.(x))" `shouldBe` (Just $ Fix $ LamF ["x"] (Fix $ VarF "x"))
    it "parses a multi-param abstraction" $
      sut "\\x y.x" `shouldBe` (Just $ Fix $ LamF ["x", "y"] (Fix $ VarF "x"))
    it "parses a var-to-var application" $
      sut "f x" `shouldBe` (Just $ Fix $ AppF (Fix $ VarF "f") (Fix $ VarF "x"))
    it "parses a lam-to-lam application" $
      sut "\\x.x \\y.y" `shouldBe`
      (Just $
       Fix $
       AppF
         (Fix $ LamF ["x"] (Fix $ VarF "x"))
         (Fix $ LamF ["y"] (Fix $ VarF "y")))
