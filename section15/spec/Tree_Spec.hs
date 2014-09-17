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
  describe "Breadcrumbs'" $ do
    it "(freeTree, [])にgoRight', goLeft'を順に適用すると部分木とパンくずリスト[L,R]を返す" $ do
      (freeTree, []) -: goRight' -: goLeft'
        `shouldBe` (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty), [L,R])
  describe "Zipper" $ do
    let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')
    it "goLeft, goRightでTreeを進みmodifyで要素を変更できる" $ do
      elemAt [L,R] (fst $ topMost newFocus) `shouldBe` 'P'
    let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')
    it "さらにgoUpしてその要素を変更する" $ do
      elemAt [L] (fst $ topMost newFocus2) `shouldBe` 'X'
    it "他の要素は変更されていない" $ do
      elemAt [L, L] (fst $ topMost newFocus2) `shouldBe` 'L'
    describe "attach" $ do
      let farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
      let newFocus3 = farLeft -: attach (Node 'Z' Empty Empty)
      it "freeTreeの一番左端に新しい木を取り付ける" $ do
        elemAt [L,L,L,L] (fst $ topMost newFocus3) `shouldBe` 'Z'
      it "他の箇所は変更されない" $ do
        elemAt [L,L,L] (fst $ topMost newFocus3) `shouldBe` 'N'

