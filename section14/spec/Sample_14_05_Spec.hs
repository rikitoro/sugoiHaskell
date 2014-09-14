module Sample_14_05_Spec where

import Test.Hspec
import Sample_14_05
import Control.Monad.Writer
import Control.Monad.State

main = hspec $ do
  describe "liftM" $ do
    it "Just値に適用" $ do
      liftM (*3) (Just 8) `shouldBe` Just 24
    it "リストに適用" $ do
      liftM (*3) [1..3] `shouldBe` [3,6,9]
    it "Writer Monadに適用" $ do
      (runWriter $ liftM not $ writer (True, "chickpeas"))
        `shouldBe` (False, "chickpeas")
    it "State Monadに適用" $ do
      (runState (liftM (+100) pop) [1,2,3,4])
       `shouldBe` (101,[2,3,4])
  describe "ap" $ do
    it "Justにした関数とJust値に適用" $ do
      Just (+3) `ap` Just 4 `shouldBe` Just 7
    it "関数とリストとリストに適用" $ do
      [(+1), (+2), (+3)] `ap` [10,11] `shouldBe` [11,12,12,13,13,14]
  describe "join" $ do
    it "ネストしたJustを平らにする" $ do
      join (Just (Just 9)) `shouldBe` Just 9
    it "Just NothingをNothingにする" $ do
      join (Just Nothing) `shouldBe` (Nothing :: Maybe String)
    it "Nothingはそのまま" $ do
      join Nothing `shouldBe` (Nothing :: Maybe String)