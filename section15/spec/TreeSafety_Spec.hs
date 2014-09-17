module TreeSafety_Spec where

import Test.Hspec
import TreeSafety

main = hspec $ do
  describe "goLeft" $ do
    it "成功した場合" $ do
      goLeft (Node 'A' Empty Empty, []) `shouldBe` Just (Empty, [LeftCrumb 'A' Empty])
    it "空の木の左部分木を取ろうとすると失敗しNothingを返す" $ do
      goLeft (Empty, []) `shouldBe` (Nothing :: Maybe (Zipper Char))
  describe ">>=" $ do
    let coolTree = Node 1 Empty (Node 3 Empty Empty)
    it "goRightで右部分木に移動" $ do
      (return (coolTree,[]) >>= goRight) 
        `shouldBe` Just (Node 3 Empty Empty, [RightCrumb 1 Empty])
    it "更にgoRightで右に移動" $ do
      (return (coolTree,[]) >>= goRight >>= goRight)
        `shouldBe` Just (Empty, [RightCrumb 3 Empty, RightCrumb 1 Empty])
    it "更にgoRightで右に移動しようとすると失敗してNothingを返す" $ do
      (return (coolTree,[]) >>= goRight >>= goRight >>= goRight)
        `shouldBe` Nothing
