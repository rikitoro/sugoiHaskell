module Sample_14_04_Spec where

import Test.Hspec

main = hspec $ do
  describe "Either Monad" $ do
    it "Left値と成功する関数をバインドするとLeft値を返す" $ do
      (Left "boom" >>= \x -> return (x+1)) `shouldBe` Left "boom"
    it "Left値とLeft値を返す関数をバインドすると初めのLeft値を返す" $ do
      (Left "boom" >>= \x -> Left "no way !") `shouldBe` (Left "boom" :: Either String Int) 
    it "Right値とLeft値を返す関数をバインドするとLeft値を返す" $ do
      (Right 100 >>= \x -> Left "no way!") `shouldBe` (Left "no way!" :: Either String Int)
    it "Right値と成功する関数をバインドするとRight値を返す" $ do
      (Right 3 >>= \x -> return (x + 100)) `shouldBe` (Right 103 :: Either String Int)