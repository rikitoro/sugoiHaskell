module Sample_06_02_Spec where

import Test.Hspec
import Test.QuickCheck

import Data.List
import Sample_06_02

main :: IO ()
main = hspec $ do
  describe "wordsNums" $ do
    it "渡された文字列中の単語とその頻出回数を返す" $ do
      wordsNums "boom bip bip boom boom pow" `shouldBe` [("bip",2),("boom",3),("pow",1)]
  describe "wordsNums'" $ do
    it "wordsNumsと同じ働きをする" $ do
      property $ \str -> wordsNums' str === (wordsNums str :: [(String, Int)])
  describe "needle `isIn` haystack" $ do
    it "リストhaystackの中にリストneedleの並びがあればTrueを返す" $ do
      "art" `isIn` "party" `shouldBe` True
    it "リストhaystackの中にリストneedleの並びがなければFalseを返す" $ do
      [1,2] `isIn` [1,3,5] `shouldBe` False
    it "isInfixOfと同じ働きをする" $ do 
      property $ \needle haystack -> needle `isIn` haystack == (needle :: String) `isInfixOf` (haystack :: String)

  describe "encode" $ do
    it "offsetと文字列を渡すと文字コードをoffsetずらすシーザー暗号を生成する" $ do
      encode 1 "to party hard" `shouldBe` "up!qbsuz!ibse"
  describe "decode" $ do
    it "encodeと同じoffsetでデコードすると元の文字列に戻る" $ do
      (decode 10 . encode 10 $ "gloval society piyo") `shouldBe` "gloval society piyo"

  describe "foldl" $ do
    it "(+),0, replicate 10000000 1 を渡すと10000000を返すはず" $ do
      pending
      -- foldl (+) 0 (replicate 100000000 1) `shouldBe` 100000000

  describe "foldl'" $ do
    it "(+), 0, replicate 10000000, 1を渡すと10000000を返す" $ do
      foldl' (+) 0 (replicate 10000000 1) `shouldBe` 10000000

  describe "digitSum" $ do
    it "数を渡すと書く桁の数の合計を返す" $ do
      digitSum 1234567890 `shouldBe` 45

  describe "firstTo40" $ do
    it "各桁の合計が40になる最初の自然数を返す" $ do
      firstTo40 `shouldBe` Just 49999

  describe "firstTo" $ do
    it "数を渡すと、各桁の合計がその数になる最初の自然数を返す" $ do
      firstTo 27 `shouldBe` Just 999

