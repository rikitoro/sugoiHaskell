module ThreeCoins_Spec where

import Test.Hspec 
import ThreeCoins
import System.Random

main = hspec $ do
  describe "ThreeCoins" $ do
    it "(mkStdGen 20)をわたす" $ do
      threeCoins (mkStdGen 20) `shouldBe` (True, False, False)
