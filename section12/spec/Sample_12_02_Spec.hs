module Sample_12_02 where

import Test.Hspec
import Test.QuickCheck

main = hspec $ do
  describe "Monoidとしてのリスト" $ do
    it "空のリストは左単位元" $ do
      [] ++ [0.5, 2.5] `shouldBe` [0.5,2.5]
    it "空のリストは右単位元" $ do
      [1,2,3] ++ [] `shouldBe` [1,2,3]
    it "結合則が成り立つ" $ do
      property $ \xs ys zs -> ((xs :: String) ++ ys) ++ zs `shouldBe` xs ++ (ys ++ zs)