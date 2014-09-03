module Randoms_Spec where

import Test.Hspec 
import Test.QuickCheck
import System.Random
import Randoms

main = hspec $ do
  describe "randoms'" $ do
    it "同じ乱数ジェネレータに対してrandomsと同じ乱数列を返す" $ do
      property $ \n -> 
        let gen = mkStdGen n 
        in (take 10 $ randoms' gen :: [Int]) == (take 10 $ randoms gen)