module Sample_05_04_Spec where

import Test.Hspec

import Sample_05_04

main :: IO ()
main = hspec $ do
  describe "numLongChains'" $ do
    it "66を返す" $ do
      numLongChains' `shouldBe` 66
  describe "lambda式を利用" $ do
    it "mapにlambda式を渡す" $ do
      map (\x -> x + 3) [1..5] `shouldBe` map (+3) [1..5]
    it "zipWithにlambda式を渡す" $ do
      zipWith (\x y -> 10 * x + y) [1..4] [2..5] `shouldBe` [12,23,34,45]
    it "lambda式でもパターンマッチができる" $ do
      map (\(a,b) -> 10 * a + b) [(1,2),(3,4),(5,6)] `shouldBe` [12,34,56]

  describe "flip'" $ do
    it "flip' (++)に文字列を2つ渡すと順番を入れ替えて結合する" $ do
      zipWith (flip' (++)) ["love you", "love me"] ["i ", "you "] `shouldBe` ["i love you", "you love me"]
    it "subtractを渡す" $ do
      map (flip' subtract 20) [1,2,3,4] `shouldBe` [19,18,17,16]