module RecursiveSpec where

import Test.Hspec
import Recursive
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "maximun'" $ do
    it "リストを渡すと最大の要素を返す" $
      maximum' [1,3,5,4,2] `shouldBe` 5

    it "要素が1つだけのリストを渡すとその要素を返す" $
      maximum' [10] `shouldBe` 10

    it "空のリストを渡すとエラーを投げる" $
      evaluate (maximum' [] :: Int) `shouldThrow` errorCall "maximun of empty list!"

  describe "replicate' n x" $ do
    it "nが1以上の場合、xをn回繰り返したリストを返す" $ do 
      replicate 3 "hoge" `shouldBe` ["hoge", "hoge", "hoge"]

    it "nが0以下の時は空のリストを返す" $ do
      replicate 0 "hoge" `shouldBe` []


  describe "take' n list" $ do
    it "nが0以下の時は空のリストを返す" $ do
      take' 0 [1,2,3] `shouldBe` [] 

    it "listの先頭n要素を返す" $ do
      take' 2 [1,2,3] `shouldBe` [1,2]

    it "listが空の時は空のリストを返す" $ do
      take' 2 ([] :: [String]) `shouldBe` []

  describe "reverse'" $ do
    it "文字列を渡すと逆順にした文字列を返す" $ do
      reverse' "reverse" `shouldBe` "esrever"

  describe "repeat'" $ do
    it "要素を渡すとその要素からなる無限リストを作る(take' 5ではじめの5要素だけチェックする)" $ do
      take' 5 (repeat' "tea") `shouldBe` ["tea","tea","tea","tea","tea"]

  describe "zip'" $ do
    it "2つのリストを渡すとそれらを綴じ合わせたリストを返す" $ do
      zip  [1,2,3] ['a','b'] `shouldBe` [(1,'a'), (2,'b')]

  describe "elem'" $ do
    it "要素とリストを渡すと要素がリストに含まれない場合はFalseを返す" $ do
      1 `elem'` [2,4,6] `shouldBe` False

    it "要素とリストを渡すと要素がリストに含まれる場合はTrueを返す" $ do
      4 `elem'` [2,4,6] `shouldBe` True


