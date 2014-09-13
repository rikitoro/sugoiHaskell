module Sample_14_01_Writer_Spec where

import Test.Hspec
import Control.Monad.Writer 
import Sample_14_01_Writer

main = hspec $ do
  describe "runWriter" $ do
    it "3という数に対してStringモノイドを組み合わせて渡す" $ do
      runWriter (return 3 :: Writer String Int) `shouldBe` (3, "")
    it "3という数に対してSumモノイドを組み合わせて渡す" $ do
      runWriter (return 3 :: Writer (Sum Int) Int) `shouldBe` (3, Sum 0)
    it "3という数に対してProductモノイドを組み合わせて渡す" $ do
      runWriter (return 3 :: Writer (Product Int) Int) `shouldBe` (3, Product 1)
  describe "multWithLog" $ do
    it "3*5を計算してログ付きで返す" $ do
      runWriter multWithLog `shouldBe` (15, ["Got number: 3", "Got number: 5"])
  describe "multWithLog'" $ do
    it "3*5を計算して通常のログとtellで追加したログをつけて返す" $ do
      runWriter multWithLog' `shouldBe` (15, ["Got number: 3", "Got number: 5","Gonna multiply these two"])

