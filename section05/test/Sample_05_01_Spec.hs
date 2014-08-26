module Sample_05_01_Spec where

import Test.Hspec
import Control.Exception (evaluate)

import Sample_05_01

main :: IO ()
main = hspec $ do
  describe "multThree" $ do
    context "第一引数を部分適応する: multTwoWithNine = multThree 9" $ do
      let multTwoWithNine = multThree 9
      it "3と3を渡すと81を返す" $ do
        multTwoWithNine 3 3 `shouldBe` 81
      it "2つ数を渡すと9と2つの数を掛けた値を返す" $ do
        multTwoWithNine 2 3 `shouldBe` 54

  describe "compareWithHundred" $ do
    it "100より小さい値を渡すとGTを返す" $ do
      compareWithHundred 99 `shouldBe` GT
    it "100を渡すとEQを返す" $ do
      compareWithHundred 100 `shouldBe` EQ
    it "100より大きな値を渡すとLTを返す" $ do
      compareWithHundred 101 `shouldBe` LT

  describe "divideByTen" $ do
    it "値を渡すと10で割った結果を返す" $ do
      divideByTen 200 `shouldBe` 20.0

  describe "isUpperAlphanum" $ do
    it "大文字のアルファベットを渡すとTrueを返す" $ do
      isUpperAlphanum 'C' `shouldBe` True
    it "小文字のアルファベットを渡すとFalseを返す" $ do
      isUpperAlphanum 'c' `shouldBe` False


