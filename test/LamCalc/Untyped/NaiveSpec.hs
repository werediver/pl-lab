{-# LANGUAGE OverloadedStrings #-}

module LamCalc.Untyped.NaiveSpec where

import           LamCalc.Untyped.Naive      (whnf)
import           LamCalc.Untyped.Naive.Expr
import           Test.Hspec

spec :: Spec
spec =
  describe "whnf" $ do
    it "keeps non-reducible applications untouched" $
      whnf (App (App (Var "a") (Var "b")) (Var "c")) `shouldBe`
      App (App (Var "a") (Var "b")) (Var "c")
    it "performs β-reduction" $ whnf (App (Lam "a" (Var "a")) (Var "b")) `shouldBe` Var "b"
    it "performs β-reduction on a nested redex" $
      whnf (App (App (Lam "a" (Var "a")) (Var "b")) (Var "c")) `shouldBe` App (Var "b") (Var "c")
