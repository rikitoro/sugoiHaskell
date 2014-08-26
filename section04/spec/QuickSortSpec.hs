module QuickSortSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import QuickSort

main :: IO ()
main = hspec $ do
  describe "quicksort" $ do
    it "空のリストを渡すと空のリストを返す" $
      quicksort ([] :: [Int]) `shouldBe` []

    it "リストを渡すとソートしたリストを返す" $
      quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9] `shouldBe` [1,2,2,3,3,4,4,5,6,7,8,9,10] 
