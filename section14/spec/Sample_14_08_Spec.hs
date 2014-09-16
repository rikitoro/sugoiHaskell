module Sample_14_08_Spec where

import Test.Hspec
import Data.Ratio
import Sample_14_08


main = hspec $ do
  describe "fmap" $ do
    it "関数とProb値を渡すと値に関数を適応し確率はそのままのProb値を返す" $ do
      fmap negate (Prob [(3, 1%2), (5, 1%4), (9, 1%4)])
        `shouldBe` Prob [(-3, 1%2), (-5, 1%4), (-9, 1%4)]
  describe "flatten" $ do
    it "入れ子になったProbリストを平らにする" $ do
      let thisSituation = Prob [(Prob [('a', 1%2), ('b', 1%2)], 1%4), (Prob [('c', 1%2), ('d', 1%2)], 3%4)]
      flatten thisSituation 
        `shouldBe` Prob [('a', 1%8), ('b', 1%8), ('c', 3%8), ('d', 3%8)]
  describe "flipThree" $ do
    it "イカサマコインが紛れ込んだ時の確率分布を求める" $ do
      getProb flipThree
        `shouldBe` [(False, 1%40), (False, 9%40), (False, 1%40), (False, 9%40),
        (False, 1%40), (False, 9%40), (False, 1%40), (True, 9%40)]