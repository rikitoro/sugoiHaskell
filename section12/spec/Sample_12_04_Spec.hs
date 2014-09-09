module Sample_12_04_Spec where

import Test.Hspec

import qualified Data.Foldable as F
import Data.Monoid

import Sample_12_04

main = hspec $ do
  describe "F.foldl" $ do
    it "(+) 0 tree を渡すとtreeのすべての要素を加算した結果を返す" $ do
      F.foldl (+) 0 testTree `shouldBe` 42
    it "(*) 1 tree を渡すとtreeのすべての要素の積を返す" $ do
      F.foldl (*) 1 testTree `shouldBe` 5 * 3 * 1 * 6 * 9 * 8 * 10
  describe "F.foldMap" $ do
    it "treeの要素に対して述語化してAnyに包んで返す関数を渡してみる" $ do
      (getAny $ F.foldMap (\x -> Any $ x == 3) testTree) `shouldBe` True
    it "treeの要素に対して述語化してAnyに包んで返す関数を渡してみる" $ do
      (getAny $ F.foldMap (\x -> Any $ x > 15) testTree) `shouldBe` False
    it "treeをリスト化する" $ do
      F.foldMap (\x -> [x]) testTree `shouldBe` [1, 3, 6, 5, 8, 9, 10]