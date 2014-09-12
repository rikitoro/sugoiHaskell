module Sample_13_06_MoveKnight_Spec where

import Test.Hspec
import Sample_13_06_MoveKnight

main = hspec $ do
  describe "moveKnight" $ do
    it "(6, 2)からは6ヶ所へ進める" $ do
      moveKnight (6,2) `shouldBe` [(8,1), (8,3), (4,1), (4,3), (7,4), (5,4)]
    it "(8, 1)からは2ヶ所へ進める" $ do
      moveKnight (8,1) `shouldBe` [(6,2), (7,3)]
  describe "canReachIn3" $ do 
    it "(6, 2)から(6, 1)へ3手で進める" $ do
      (6,2) `canReachIn3` (6,1) `shouldBe` True
    it "(6, 2)から(7, 3)へ3手では進めない" $ do
      (6,2) `canReachIn3` (7,3) `shouldBe` False
