module Sample_05_02_Spec where

import Test.Hspec
import Control.Exception (evaluate)

import Sample_05_02

main :: IO ()
main = hspec $ do
  describe "applyTwice" $ do
    it "(+3)と数を渡すと数に6を足した値を返す" $ do
      applyTwice (+3) 10 `shouldBe` 16
    it "(++\" HAHA\")と文字列を渡すと文字列に HAHA HAHAを追加した文字列を返す" $ do
      applyTwice (++" HAHA") "HEY" `shouldBe` "HEY HAHA HAHA"
    it "(\"HAHA \"++)と文字列を渡すとHAHA HAHAと文字列を繋げた文字列を返す" $ do
      applyTwice ("HAHA "++) "HEY" `shouldBe` "HAHA HAHA HEY"
    it "(3:)と数リストを渡すと配列の先頭に2つ3を追加した数リストを返す" $ do
      applyTwice (3:) [1] `shouldBe` [3,3,1]

  describe "zipWith'" $ do
    it "(+)と2つの数リストを渡すと2つの数リストの要素を足しあわせた値を要素とする数リストを返す" $ do
      zipWith' (+) [4,2,5,6] [2,6,2,3] `shouldBe` [6,8,7,9]
    it "(++)と文字列からなるリストを2つ渡すと対応する文字列を連結したリストを返す" $ do
      zipWith' (++) ["foo ", "bar ", "baz "] ["FT", "HP", "ALD"] `shouldBe` ["foo FT", "bar HP", "baz ALD"]
    it "(*)と有限のリストと無限リストを渡すと有限のリストが終わるところまで積を計算したリストを返す" $ do
      zipWith' (*) (replicate 5 2) [1..] `shouldBe` [2,4,6,8,10]
    it "zipWith' (*)とネストした2つの配列を渡すこともできる" $ do
      zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]] `shouldBe` [[3,4,6],[9,20,30],[10,12,12]]

  describe "flip'" $ do
    it "zipと数リストと文字列を渡すと文字列と数リストをzipしたリストを返す" $ do
      flip' zip [1..5] "Hello" `shouldBe` [('H',1),('e',2),('l',3),('l',4),('o',5)]
    it "zipWith' (flip' div) [2,2..] [10,8,6,4,2]" $ do
      zipWith' (flip' div) [2,2..] [10,8,6,4,2] `shouldBe` [5,4,3,2,1]

