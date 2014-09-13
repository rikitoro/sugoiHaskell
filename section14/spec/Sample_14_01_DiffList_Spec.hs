module Sample_14_01_DiffList_Spec where

import Test.Hspec
import Sample_14_01_DiffList
import Control.Monad.Writer


main = hspec $ do
  describe "DiffList" $ do
    it "fromDiffList, toDiffList, mappendを組み合わせてリストを構築できる" $ do
      fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
        `shouldBe` [1,2,3,4,1,2,3] 
  describe "gcd'" $ do
    it "返り値のタプルの第2成分は計算ログの逆順である" $ do
      (fromDiffList . snd . runWriter $ gcd' 110 34)
        `shouldBe` ["Finished with 2", "8 mod 2 = 0", "34 mod 8 = 2", "110 mod 34 = 8" ]
  describe "finalCountDown" $ do
    it "数を渡すとその数から逆順にログをつけながらカウントダウンする" $ do
      (fromDiffList . snd . runWriter $ finalCountDown 500000)
        `shouldBe` map show [0..500000]
  describe "finalCountDown'" $ do
    it "数を渡すとその数から逆順にログをつけながらカウントダウンする。でもおそい・・・" $ do
      (snd . runWriter $ finalCountDown' 5000)
        `shouldBe` map show [0..5000]