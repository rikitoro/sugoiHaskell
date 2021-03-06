module Sample_12_03_Spec where

import Test.Hspec
import Test.QuickCheck

import Data.Monoid

import Sample_12_03

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
  describe "lengthCompare" $ do
    it "文字列の長さを比較する" $ do
      lengthCompare "zen" "ants" `shouldBe` LT
    it "文字列の長さが同じ時は辞書順で比較する" $ do
      lengthCompare "zen" "ant" `shouldBe` GT

  describe "lengthCompare1" $ do
    it "文字列の長さを比較する" $ do
      lengthCompare1 "zen" "ants" `shouldBe` LT
    it "文字列の長さが同じ時は母音の数を比較する" $ do
      lengthCompare1 "zen" "ana" `shouldBe` LT
    it "母音の数も等しい場合は辞書順を比較する" $ do
      lengthCompare1 "zen" "ann" `shouldBe` GT

  describe "Maybe Monoid" $ do
    describe "mappend" $ do
      it "NothingとJust値を渡すとJust値を返す" $ do
        Nothing `mappend` Just "andy" `shouldBe` Just "andy"
      it "Just値同士を渡すとJustの中身をmappendしてJustに包んで返す" $ do
        Just (Sum 3) `mappend` Just (Sum 4) `shouldBe` Just (Sum 7)

  describe "First Monoid" $ do
    it "最初に現れるJust値を取得できる" $ do
      (getFirst . mconcat . map First $ [Nothing, Nothing, Just 9, Just 10, Nothing]) `shouldBe`
        Just 9
    describe "Last Monoid" $ do
    it "最後に現れるJust値を取得できる" $ do
      (getLast . mconcat . map Last $ [Nothing, Nothing, Just 9, Just 10, Nothing]) `shouldBe`
        Just 10