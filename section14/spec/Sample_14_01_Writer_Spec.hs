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
  describe "gcd'" $ do
    it "数を2つ渡すとその最大公約数を返す" $ do
      gcd' 8 3 `shouldBe` 1
  describe "gcd''" $ do
    it "返されるタプルの最初の成分は最大公約数である" $ do
      (fst . runWriter $ gcd'' 8 3) `shouldBe` 1
    it "返されるタプルの第2成分は計算ログである" $ do
      (snd . runWriter $ gcd'' 8 3) `shouldBe` ["8 mod 3 = 2", "3 mod 2 = 1", "2 mod 1 = 0","Finished with 1"]
  describe "gcdReverse" $ do
    it "返されるタプルの最初の成分は最大公約数である" $ do
      (fst . runWriter $  gcdReverse 8 3) `shouldBe` 1
    it "返されるタプルの第2成分は逆順の計算ログである" $ do
      (snd . runWriter $ gcdReverse 8 3) `shouldBe` [ "Finished with 1","2 mod 1 = 0","3 mod 2 = 1","8 mod 3 = 2"]
