module Sample_14_03_Spec where

import Test.Hspec
import Sample_14_03


main = hspec $ do
  describe "stackManip" $ do
    it "スタックを渡すとトップ要素と残りのスタックのタプルを返す" $ do
      stackManip [5,8,2,1] `shouldBe` (5,[8,2,1])
