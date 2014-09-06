module Sample_11_04_Spce where

import Test.Hspec
import Sample_11_04
import Control.Applicative

main = hspec $ do
  describe "liftA2" $ do
    it "Just 4からJust [4]を生成する" $ do
      fmap (\x -> [x]) (Just 4) `shouldBe` Just [4]
    it "Just 3とJust [4]からJust [3,4]を生成する" $ do
      liftA2 (:) (Just 3) (Just [4]) `shouldBe` Just [3,4]
    it "アプリカティブスタイルでJust 3とJust [4]からJust [3,4]を生成する" $ do
      (:) <$> Just 3 <*> Just [4] `shouldBe` Just [3,4]
    