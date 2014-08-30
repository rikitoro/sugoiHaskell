module Sample_07_02_2_Spec where

import Test.Hspec
import Sample_07_02_2

main :: IO ()
main = hspec $ do
  describe "area" $ do
    it "Circleを渡すと円の面積を求める" $ do
      (area $ Circle (Point 10 30) 2) `shouldBe` pi * 4
    it "Rectangleを渡すと長方形の面積を求める" $ do
      (area $ Rectangle (Point 1 2) (Point 5 10)) `shouldBe` 32
  describe "nudge" $ do
    it "Circleを平行移動する" $ do
      nudge (Circle (Point 1 2) 10) 2 3 `shouldBe` Circle (Point 3 5) 10
    it "Rectangleを平行移動する" $ do
      nudge (Rectangle (Point 1 2) (Point 5 7)) 1 (-1) 
        `shouldBe` Rectangle (Point 2 1) (Point 6 6)

  describe "baseRect" $ do
    it "原点で幅と高さを指定した長方形をつくりnudgeで平行移動させる" $ do
      nudge (baseRect 10 20) 2 3 
        `shouldBe` Rectangle (Point 2 3) (Point 12 23)
  describe "baseCircle" $ do
    it "原点で半径を指定した円を作りnudgeで平行移動させる" $ do
      nudge (baseCircle 7) 2 3 `shouldBe` Circle (Point 2 3) 7 


 