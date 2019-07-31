{-# LANGUAGE TypeApplications #-}

module LamCalc.Untyped.DeBruijnMaybeSpec where

import           LamCalc.Untyped.DeBruijn.Maybe      (nf, whnf)
import           LamCalc.Untyped.DeBruijn.Maybe.Expr
import           Test.Hspec

spec :: Spec
spec =
  describe "DeBruijn" $ do
    describe "whnf" $ do
      it "keeps non-reducible applications untouched" $
        whnf (Var "a" `App` Var "b" `App` Var "c") `shouldBe` Var "a" `App` Var "b" `App`
        Var "c"
      it "performs β-reduction" $ whnf (i `App` Var "b") `shouldBe` Var "b"
      it "performs β-reduction on a nested redex" $
        whnf (i `App` Var "b" `App` Var "c") `shouldBe` App (Var "b") (Var "c")
      it "reduces SKK to WHNF" $
        whnf @String (s `App` k `App` k) `shouldBe` Lam (k `App` b0 `App` (k `App` b0))
    describe "nf" $ do
      it "reduces SKK to NF" $ nf @String (s `App` k `App` k) `shouldBe` i
      it "pushes a bare bound variable operand down during substitution" $
        nf @String (Lam (k `App` b0)) `shouldBe` k
      it "pushes bound variables in an operand down during substitution" $
        nf (Lam (k `App` (Var (Next "a") `App` b0))) `shouldBe` Lam (Lam (Var (Next (Next"a")) `App` b1))

i :: Expr a
i = Lam b0

k :: Expr a
k = Lam $ Lam b1

s :: Expr a
s = Lam $ Lam $ Lam $ b2 `App` b0 `App` (b1 `App` b0)

b0 :: Expr (Index a)
b0 = Var Zero

b1 :: Expr (Index (Index a))
b1 = Var $ Next Zero

b2 :: Expr (Index (Index (Index a)))
b2 = Var $ Next $ Next Zero
