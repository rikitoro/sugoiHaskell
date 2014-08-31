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