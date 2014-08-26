module RecursiveSpec where

import Test.Hspec
import Control.Exception (evaluate)

import Recursive


main :: IO ()
main = hspec $ do
  describe "maximun'" $ do
    it "リストを渡すと最大の要素を返す" $
      maximum' [1,3,5,4,2] `shouldBe` 5

    it "要素が1つだけのリストを渡すとその要素を返す" $
      maximum' [10] `shouldBe` 10

    it "空のリストを渡すとエラーを投げる" $
      maximum' [] `shouldThrow` error
