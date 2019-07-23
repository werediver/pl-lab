{-# LANGUAGE TypeApplications #-}

module LamCalc.Untyped.DeBruijnSpec where

import           LamCalc.Untyped.DeBruijn      (nf, whnf)
import           LamCalc.Untyped.DeBruijn.Expr
import           Test.Hspec

spec :: Spec
spec =
  describe "DeBruijn" $ do
    describe "whnf" $ do
      it "keeps non-reducible applications untouched" $
        whnf (Free "a" `App` Free "b" `App` Free "c") `shouldBe` Free "a" `App` Free "b" `App`
        Free "c"
      it "performs β-reduction" $ whnf (i `App` Free "b") `shouldBe` Free "b"
      it "performs β-reduction on a nested redex" $
        whnf (i `App` Free "b" `App` Free "c") `shouldBe` App (Free "b") (Free "c")
      it "reduces SKK to WHNF" $
        whnf @String (s `App` k `App` k) `shouldBe` lam (k `App` b0 `App` (k `App` b0))
    describe "nf" $ do
      it "reduces SKK to NF" $ nf @String (s `App` k `App` k) `shouldBe` i
      it "pushes a bare bound variable operand down during substitution" $
        nf @String (lam (k `App` b0)) `shouldBe` k
      it "pushes bound variables in an operand down during substitution" $
        nf (lam (k `App` (Free "a" `App` b0))) `shouldBe` lam (Lam (Scope (Free "a" `App` b1)))

lam :: Expr a -> Expr a
lam = Lam . Scope

i :: Expr a
i = Lam $ Scope b0

k :: Expr a
k = Lam $ Scope $ Lam $ Scope b1

s :: Expr a
s = Lam $ Scope $ Lam $ Scope $ Lam $ Scope $ b2 `App` b0 `App` (b1 `App` b0)

b0 :: Expr a
b0 = Bound 0

b1 :: Expr a
b1 = Bound 1

b2 :: Expr a
b2 = Bound 2
