module Sample_12_03 where

import Test.Hspec
import Test.QuickCheck

import Data.Monoid

main = hspec $ do
  describe "リストはモノイド" $ do
    describe "mapend" $ do
      it "2つのリストを結合する" $ do
        [1,2,3] `mappend` [4,5,6] `shouldBe` [1,2,3,4,5,6]
      it "3つのリストを結合する" $ do
        ("one" `mappend` "two") `mappend` "three" `shouldBe` "onetwothree"
      it "3つのリストを結合する" $ do
        "one" `mappend` ("two" `mappend` "three") `shouldBe` "onetwothree"
      it "memptyは単位元" $ do
        "peng" `mappend` mempty `shouldBe` "peng"
    describe "mconcat" $ do
      it "二重のリストを渡すとその要素を連結した平らなリストを返す" $ do
        mconcat [[1,2],[3,6],[9]] `shouldBe` [1,2,3,6,9]
    describe "mempty" $ do
      it "空のリストである" $ do
        (mempty :: [Int]) `shouldBe` []
  describe "Product" $ do
    it "mappendは掛け算" $ do
      Product 3 `mappend` Product 9 `shouldBe` Product 27
    it "memptyはProduct 1に相当" $ do
      Product 2 `mappend` mempty `shouldBe` Product 2
    it "mconcatにProduct値のリストを渡すとすべての積をとれる" $ do
      (getProduct . mconcat . map Product $ [3,4,2]) `shouldBe` 24
  describe "Sum" $ do
    it "mappendは足し算" $ do
      (getSum $ Sum 2 `mappend` Sum 3) `shouldBe` 5
    it "memptyはSum 0に相当" $ do
      (getSum $ mempty `mappend` Sum 3) `shouldBe` 3
    it "mconcatで総和をとれる" $ do
      (getSum . mconcat . map Sum $ [1,2,3]) `shouldBe` 6
  describe "Any" $ do
    it "mappendはorに相当" $ do
      Any True `mappend` Any False `shouldBe` Any True
    it "memptyはFalseに相当" $ do
      mempty `mappend` Any False `shouldBe` Any False
    it "mconcatでリストの中身がいづれか1つでもTrueであればTrueを返すことができる" $ do
      (mconcat . map Any $ [False, False, True, False]) `shouldBe` Any True
  describe "All" $ do
    it "mappendはandに相当" $ do
      All True `mappend` All False `shouldBe` All False
    it "memptyはTrueに相当" $ do
      mempty `mappend` All True `shouldBe` All True
    it "mconcatでリストの中身がいづれか1つでもFalseであればFalseを返すことができる" $ do
      (mconcat . map All $ [True, False, True, True]) `shouldBe` All False
    it "mconcatでリストの中身がすべてTrueであればTrueを返すことができる" $ do
      (mconcat . map All $ [True, True, True]) `shouldBe` All True
  describe "Ordering Monoid" $ do
    describe "mappend" $ do
      it "LTとGTを渡すとLTを返す" $ do
        LT `mappend` GT `shouldBe` LT
      it "GTとLTを渡すとGTを返す" $ do
        GT `mappend` LT `shouldBe` GT
      it "memptyとLTを渡すとLTを返す" $ do
        mempty `mappend` LT `shouldBe` LT
      it "memptyとGTを渡すとLTを返す" $ do
        mempty `mappend` GT `shouldBe` GT