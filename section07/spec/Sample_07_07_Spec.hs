module Sample_07_07_Spec where

import Test.Hspec 
import Sample_07_07

main = hspec $ do
  describe "List" $ do
    it "Empty" $ do
      show (Empty :: List Int) `shouldBe` "Empty"
    it "5 `Cons` Empty" $ do
      (show $ 5 `Cons` Empty) `shouldBe` "Cons 5 Empty"
    it "4 `Cons` (5 `Cons` Empty)" $ do
      (show $ 4 `Cons` (5 `Cons` Empty)) `shouldBe` "Cons 4 (Cons 5 Empty)"
    it "3 `Cons` (4 `Cons` (5 `Cons` Empty))" $ do
      (show $ 3 `Cons` (4 `Cons` (5 `Cons` Empty))) `shouldBe` "Cons 3 (Cons 4 (Cons 5 Empty))"

  describe "List'" $ do
    it "Empty'" $ do
      show (Empty' :: List' Int) `shouldBe` "Empty'"
    it ":-:で要素とList'を結合する" $ do
      let a = 3 :-: 2 :-: 1 :-: Empty'
      (show $ 100 :-: a) `shouldBe` "100 :-: (" ++ show a ++ ")"
    it "^++で2つのList'を結合する" $ do
      let a = 3 :-: 4 :-: 5 :-: Empty'
      let b = 6 :-: 7 :-: Empty'
      a ^++ b `shouldBe` 3 :-: 4 :-: 5 :-: 6 :-: 7 :-: Empty'

  describe "Tree" $ do
    let nums = [8, 6, 4, 1, 7, 3, 5]
    let numsTree = foldr treeInsert EmptyTree nums
    it "show numsTree" $ do
      show numsTree 
        `shouldBe` "Node 5 "
          ++ "(Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) "
          ++ "(Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))"
    describe "treeElem" $ do
      it "要素が木に含まれるときはTrueを返す" $ do
        6 `treeElem` numsTree `shouldBe` True
      it "要素が木に含まれないときはFalseを返す" $ do
        10 `treeElem` numsTree `shouldBe` False