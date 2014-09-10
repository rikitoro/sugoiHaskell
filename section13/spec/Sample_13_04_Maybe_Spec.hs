module Sample_13_04_Maybe_Spec where

import Test.Hspec
import Sample_13_04_Maybe

main = hspec $ do
  describe "landLeft" $ do
    it "2羽鳥が止まってもバランスがとれていれば大丈夫" $ do
      landLeft 2 (0,0) `shouldBe` Just (2, 0)
    it "一気に10羽止まるとバランスを崩す" $ do
      landLeft 10 (0, 3) `shouldBe` Nothing
  describe "landRight" $ do
    it "2羽鳥が止まってバランスを崩した" $ do
      landRight 2 (0,2) `shouldBe` Nothing
  describe "bind >>=" $ do
    it "右に2羽、左に2羽、右に2羽ならバランスを崩さない" $ do
      (return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2)
        `shouldBe` Just (2, 4)
    it "左に1羽、右に4羽、左から−1羽、右から-2羽だと途中で落ちる" $ do
      (return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2))
        `shouldBe` Nothing
    it "bananaがあると滑って落ちる" $ do
      (return (0,0) >>= landLeft 1 >>= banana >>= landRight 1)
        `shouldBe` Nothing
    it ">> Nothingで落とす" $ do
      (return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1)
        `shouldBe` Nothing



