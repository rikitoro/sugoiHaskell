module Sample_14_07_Spec where

import Test.Hspec
import Sample_14_07

main = hspec $ do
  describe "." $ do
    it "関数を合成して新たな関数を構成する" $ do
      let f = (+1) . (*100)
      f 4 `shouldBe` 401
    it "関数のリストを畳み込む" $ do
      let f = foldr (.) id [(+8), (*100), (+1)]
      f 1 `shouldBe` 208
  describe "<=<" $ do
    it "モナディク関数を合成して新たなモナディック関数を構成する" $ do
      let g = (\x -> return (x+1)) <=< (\x -> return (x*100))
      (Just 4 >>= g) `shouldBe` Just 401