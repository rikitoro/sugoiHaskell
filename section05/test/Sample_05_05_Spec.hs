module Sample_05_05_Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Sample_05_05

main = hspec $ do
  describe "sum'" $ do
    it "リストを渡すと要素の和を返す" $ do
      sum' [3,5,2,1] `shouldBe` 11

  describe "sum''" $ do
    it "リストを渡すとsum'にリストを渡したものと同じ結果を返す" $ do
      property $ \xs -> sum'' xs == (sum' xs :: Int)

  describe "map'" $ do
    it "mapをfoldrで定義" $ do
      map' (^2) [1..10] `shouldBe` map (^2) [1..10]

  describe "map''" $ do
    it "mapをfoldlで定義" $ do
      map'' (^3) [1..10] `shouldBe` map (^3) [1..10]

  describe "elem'" $ do
    it "elemをfoldrで定義" $ do
      elem' 8 [2,4..10] `shouldBe` elem 8 [2,4..10]

  describe "elem''" $ do
    it "elemをfoldlで定義" $ do
      elem'' 9 [2,4..10] `shouldBe` elem 9 [2,4..10]

  describe "maximum'" $ do
    it "空のリストを渡すとエラーを投げる" $ do
      evaluate (maximum' [] :: Int) `shouldThrow` anyErrorCall
    it "数リストを渡すと最大要素を返す" $ do
      maximum' [7,4,6,1,9,2] `shouldBe` 9

  describe "maximum''" $ do
    it "空のリストを渡すとエラーを投げる" $ do
      evaluate (maximum'' [] :: Int) `shouldThrow` anyErrorCall
    it "数リストを渡すと最大要素を返す" $ do
      maximum'' [7,4,6,1,9,2] `shouldBe` 9

  describe "sum'''" $ do
    it "空のリストを渡すとエラーを投げる" $ do
      evaluate (sum''' [] :: Int) `shouldThrow` anyErrorCall
    it "空でないリストを渡すとsumにリストを渡したものと同じ結果を返す" $ do
      property $ \x xs -> sum''' (x:xs) == (sum (x:xs) :: Int)

  describe "reverse'" $ do
    it "reverseをfoldl,lambda式で定義" $ do
      property $ \xs -> reverse' xs == (reverse xs :: [Int])

  describe "reverse''" $ do
    it "reverseをfoldl, flip, (:)で定義" $ do
      property $ \xs -> reverse'' xs == (reverse xs :: [Char])

  describe "reverse'''" $ do
    it "reverseをfoldrで定義" $ do
      property $ \xs -> reverse''' xs == (reverse xs :: [Int])

  describe "product'" $ do
    it "リストを渡すと要素の積を返す" $ do
      product' [1..5] `shouldBe` 120

  describe "filter'" $ do 
    it "filterをfoldrで定義" $ do
      property $ \xs -> filter' even xs == (filter even xs :: [Int])

  describe "and'" $ do
    it "Falseの無限リストを渡すとFalseを返す" $ do
      and' (repeat False) `shouldBe` False

  describe "scanl" $ do
    it "foldlのアキュムレータの中間状態のリストを返す" $ do
      scanl (+) 0 [1,2,3,4] `shouldBe` [0, 1, 3, 6, 10]

  describe "scanr" $ do
    it "foldrのアキュムレータの中間状態のリストを返す" $ do
      scanr (+) 0 [1,2,3,4] `shouldBe` [10,9,7,4,0]

  describe "scanl1" $ do
    it "foldl1のアキュムレータの中間状態のリストを返す" $ do
      scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1] `shouldBe` [3,4,5,5,7,9,9,9]

  describe "sqrtSums" $ do
    it "自然数の平方根を小さいものから足していったとき1000を超えるのは何個目？" $ do
      sqrtSums `shouldBe` 131