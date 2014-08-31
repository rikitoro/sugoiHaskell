module Sample_07_08_Spec where

import Test.Hspec 
import Sample_07_08

main = hspec $ do
  describe "TrafficLight" $ do
    describe "Eq型クラスのインスタンスとして" $ do
      it "RedとRedは等価である" $ do
        Red == Red `shouldBe` True
      it "RedとGreenは等価ではない" $ do
        Red /= Green `shouldBe` True
      it "elemを使用できる" $ do
        Red `elem` [Red, Yellow] `shouldBe` True
    describe "Show型クラスのインスタンスとして" $ do
      it "showで文字列に変換できる" $ do
        show [Red, Yellow, Green] `shouldBe` "[Red light,Yellow light,Green light]"