module Sample_14_02_Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative
import Sample_14_02

main = hspec $ do
  describe "Functorとしての関数" $ do
    let f = (*5)
    let g = (+2)
    it "fmap f gは合成関数となる" $ do
      (fmap f g $ 8) `shouldBe` 50
  describe "Applicative Functorとしての関数" $ do
    let f = (+) <$> (*2) <*> (+10)
    it "applicative stypeで関数を構成" $ do
      f 3 `shouldBe` 19
  describe "addStuff" $ do
    it "Readerモナドとして働き関数を構成" $ do
      addStuff 3 `shouldBe` 19
    it "addStuff'と等価" $ do
      property $ \x -> addStuff x `shouldBe` addStuff' x