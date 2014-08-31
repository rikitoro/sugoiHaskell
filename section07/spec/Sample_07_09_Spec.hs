module Sample_07_09_Spec where

import Test.Hspec

import Sample_07_09

import Sample_07_07
import Sample_07_08


main = hspec $ do
  describe "YesNo型クラス" $ do
    describe "yesno" $ do
      it "0 :: Intを渡すとFalse" $ do
        (yesno $ length []) `shouldBe` False
      it "文字列を渡すとTrue" $ do
        yesno "haha" `shouldBe` True 
      it "空の文字列を渡すとFalse" $ do
        yesno "" `shouldBe` False
      it "Just値を渡すとTrue" $ do
        (yesno $ Just 0) `shouldBe` True
      it "Nothingを渡すとFalse" $ do
        yesno Nothing `shouldBe` False
      it "Falseを渡すとFalse" $ do
        yesno False `shouldBe` False
      it "EmptyTreeを渡すとFalse" $ do
        yesno EmptyTree `shouldBe` False
      it "singleton 0を渡すとTrue" $ do
        (yesno $ singleton 0) `shouldBe` True
      it "空でないリストを渡すとTrue" $ do
        yesno [0,0] `shouldBe` True
      it "空のリストを渡すとFalse" $ do
        yesno [] `shouldBe` False

    describe "yesnoIf" $ do
      it "空のリストを渡すとnoResultを返す" $ do
        yesnoIf [] "YEAH!" "NO!" `shouldBe` "NO!"
      it "空でないリストを渡すとyesResultを返す" $ do
        yesnoIf [1,2,3] "YEAH!" "NO!" `shouldBe` "YEAH!"
      it "Trueを渡すとyesResultを返す" $ do
        yesnoIf True "YEAH!" "NO!" `shouldBe` "YEAH!"
      it "Just値を渡すとyesResutを返す" $ do
        yesnoIf (Just 10) "YEAH!" "NO!" `shouldBe` "YEAH!"
      it "Nothingを渡すとnoResultを返す" $ do
        yesnoIf Nothing "YEAH!" "NO!" `shouldBe` "NO!"






