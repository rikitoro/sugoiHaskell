module Tree_Spec where

import Test.Hspec
import Tree

main = hspec $ do
  describe "changeToP" $ do
    let directions = [R, L]
    let newTree = changeToP directions freeTree
    it "directionsで指定された箇所はPに変更されている" $ do
      elemAt directions newTree `shouldBe` 'P'
    it "他の箇所は元と同じ要素である" $ do
      elemAt [L, R, L] newTree `shouldBe` 'S'