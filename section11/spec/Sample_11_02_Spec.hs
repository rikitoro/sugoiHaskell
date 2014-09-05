module Sample_11_02_Spec where

import Test.Hspec
import Sample_11_02

main = hspec $ do
  describe "ファンクター則第一法則" $ do
    it "Just値に対して成り立つ" $ do
      fmap id (Just 3) `shouldBe` Just 3
    it "Listに対して成り立つ" $ do
      fmap id (Just [1..5]) `shouldBe` Just [1..5]
    it "空のリストにたいして成り立つ" $ do
      fmap id [] `shouldBe` ([] :: [Int])
    it "Nothingに対して成り立つ" $ do
      fmap id Nothing `shouldBe` (Nothing :: Maybe Int)

  describe "CMaybe" $ do
    describe "fmap" $ do
      it "CJust 0値に適用する" $ do
        (show . fmap (++"ha") $ CJust 0 "ho") `shouldBe` "CJust 1 \"hoha\""
      it "CNothing値に適用する" $ do
        (show . fmap (++"ha") $ CNothing) `shouldBe` "CNothing" 
      it "ファンクター則を満たさない反例" $ do
        fmap id (CJust 0 "haha") /= id (CJust 0 "haha") 