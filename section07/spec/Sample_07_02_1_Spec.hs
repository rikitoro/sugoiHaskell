module Sample_07_02_1_Spec where

import Test.Hspec
import Test.QuickCheck
import Sample_07_02_1

main :: IO ()
main = hspec $ do
  describe "area" $ do
    it "Circleを渡すと円の面積を計算する" $ do
      (area $ Circle 1.1 2.2 3.3) `shouldBe` pi * 3.3 ^ 2
    it "Rectangleを渡すと長方形の面積を計算する" $ do
      (area $ Rectangle 1 2 4 7) `shouldBe` 15 

  describe "Circle" $ do
    it "同心円のサークルを作る" $ do
      map (Circle 10 20) [4,5,6,6] `shouldBe` [Circle 10 20 4,Circle 10 20 5,Circle 10 20 6,Circle 10 20 6]