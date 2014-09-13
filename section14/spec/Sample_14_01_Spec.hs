module Sample_14_01_Spec where

import Test.Hspec
import Data.Monoid
import Sample_14_01

main = hspec $ do
  describe "isBigGang" $ do
    it "9以下の数を渡すと大きなGangではない旨を文脈付きで返す" $ do
      isBigGang 3 `shouldBe` (False, "Compared gang size to 9")
    it "9より大きな数を渡すと大きなGangではない旨を文脈付きで返す" $ do
      isBigGang 10 `shouldBe` (True, "Compared gang size to 9")
  describe "applyLog" $ do
    it "9以下のログ付きの値とisBigGangを渡すと新たなログを追加されたFalse値が返る" $ do
      (3, "Smallish gang.") `applyLog` isBigGang
        `shouldBe` (False, "Smallish gang.Compared gang size to 9")
    it "9より大きなのログ付きの値とisBigGangを渡すと新たなログを追加されたTrue値が返る" $ do
      (10, "A freaking platoon.") `applyLog` isBigGang
        `shouldBe` (True, "A freaking platoon.Compared gang size to 9")
    it "第2引数に値を受け取ってログ付きの値を返すラムダ関数を渡す" $ do
      ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
        `shouldBe` (5, "Got outlaw name.Applied length.")
    it "第2引数に値を受け取ってログ付きの値を返すラムダ関数を渡す" $ do
      ("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
        `shouldBe` (7, "Got outlaw name.Applied length.")
    it "10ドルのbeansにドリンクを頼むとミルクが出されて計35ドルになる" $ do
      ("beans", Sum 10) `applyLog` addDrink `shouldBe` ("milk", Sum 35)
    it "25ドルのjerkyにドリンクを頼むとwhiskeyが出されて計124ドルになる" $ do
      ("jerky", Sum 25) `applyLog` addDrink `shouldBe` ("whiskey", Sum 124)
    it "5ドルのdogmeatにドリンクを頼むとbeerが出されて計35ドルになる" $ do
      ("dogmeat", Sum 5) `applyLog` addDrink `shouldBe` ("beer", Sum 35)
    it "さらにドリンクを頼むともう一杯beerが出されて計65ドルになる" $ do
      ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink `shouldBe` ("beer", Sum 65)
