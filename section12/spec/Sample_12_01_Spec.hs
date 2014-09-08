module Sample_12_01_Spec where

import Test.Hspec
import Control.Exception (evaluate)
import Sample_12_01

main = hspec $ do
  describe "CharList" $ do
    it "値コンストラクタに同じ文字列を渡したCharList値は等しい" $ do
      CharList "benny" `shouldBe` CharList "benny"
    it "値コンストラクタに異なる文字列を渡したCharList値は異なる" $ do
      CharList "benny" /= CharList "oisters"
  describe "Pair" $ do
    describe "fmap" $ do
      it "関数とPair値を渡すとPairは初めの要素に関数を適用したPairを返す" $ do
        (fmap (*100) $ Pair (2,3)) `shouldBe` Pair (200,3)
      it "関数とPair値を渡すとPairは初めの要素に関数を適用したPairを返す" $ do
        (fmap reverse $ Pair ("london calling",3)) `shouldBe` Pair ("gnillac nodnol",3)
  describe "CoolBool" $ do
    describe "helloMe" $ do
      it "undefinedを渡すと\"hello\"を返す" $ do
        helloMe undefined `shouldBe` "hello"
  describe "CoolBool'" $ do
    describe "helloMe'" $ do
      it "undefinedを渡すとエラーを投げる" $ do
        evaluate (helloMe' undefined) `shouldThrow` anyErrorCall
