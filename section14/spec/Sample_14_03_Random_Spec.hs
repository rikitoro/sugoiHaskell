module Sample_14_03_Random_Spec where

import Test.Hspec
import Sample_14_03_Random
import System.Random
import Control.Monad.State

main = hspec $ do
  describe "threeCoins" $ do
    it "乱数ジェネレータを渡すと3枚コインを振る" $ do
      (fst . runState threeCoins $ mkStdGen 33 )
        `shouldBe` (True,False,True)