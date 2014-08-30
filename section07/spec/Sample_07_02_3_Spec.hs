module Sample_07_02_3 where

import Test.Hspec 
import Shapes



main :: IO ()
main = hspec $ do
  describe "Point" $ do
    it "Pointの値コンストラクタは呼び出せない" $ do 
      pending --Point 1 2 `shouldBe` Point 1 2
