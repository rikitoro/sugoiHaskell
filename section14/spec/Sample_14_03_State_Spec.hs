module Sample_14_03_State_Spec where

import Test.Hspec
import Control.Monad.State

import Sample_14_03_State

main = hspec $ do
  describe "stackManip" $ do
    it "スタックを渡すとトップ要素と残りのスタックのタプルを返す" $ do
      runState stackManip [5,8,2,1] `shouldBe` (5,[8,2,1])
  describe "stackStuff" $ do
    it "先頭が5のスタックを渡すとそのままそっともとにもどす" $ do
      runState stackStuff [5, 0, 2] `shouldBe` ((),[5, 0, 2])
    it "先頭が5以外のスタックであれば代わりに3と8を積む" $ do
      runState stackStuff [4, 0, 2] `shouldBe` ((),[8, 3, 0, 2])
  describe "moreStack" $ do
    it "stackManipを使った結果が100ならstackStaffを実行する" $ do
      runState moreStack [100, 4, 0, 2] `shouldBe` ((),[8, 3, 0, 2])
    it "stackManipを使った結果が100ならstackStaffを実行する" $ do
      runState moreStack [100, 5, 0, 2] `shouldBe` ((), [5, 0, 2])
    it "stackManipを使った結果が100でないならなにもしない" $ do
      runState moreStack [101, 4, 0, 2] `shouldBe` ((), [4, 0, 2])
