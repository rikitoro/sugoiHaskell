module Sample_13_03_Spec where

import Test.Hspec

main = hspec $ do
  describe "Maybe Monad" $ do
    it "returnはJustと等価" $ do
      (return "WHAT" :: Maybe String) `shouldBe` Just "WHAT"
    it "Just値と関数をバインダで結合する" $ do
      (Just 9 >>= ((\x -> return (x*10)) :: Int -> Maybe Int)) `shouldBe` Just 90
    it "Nothing値と関数をバインダで結合する" $ do
      (Nothing >>= ((\x -> return (x*10)) :: Int -> Maybe Int)) `shouldBe` Nothing
