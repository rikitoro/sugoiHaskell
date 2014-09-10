module Sample_13_04_Spec where

import Test.Hspec
import Sample_13_04

main = hspec $ do
  describe "landLeft" $ do
    it "左側に2羽鳥をとまらせる" $ do
      landLeft 2 (0, 0) `shouldBe` (2, 0)
    it "左側から1羽鳥が飛び立つ" $ do
      landLeft (-1) (2, 2) `shouldBe` (1, 2)
  describe "landRight" $ do
    it "右側に2羽鳥をとまらせる" $ do
      landRight 2 (3, 4) `shouldBe` (3, 6)
  describe "-:でlandLef,landRightを接続する" $ do
    it "空のポールに右に1羽、左に1羽、最後に左に2羽鳥が止まる" $ do
      (0,0) -: landRight 1 -: landLeft 1 -: landLeft 2 `shouldBe` (3, 1)
  
