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
  describe "sequenceA" $ do
    it "Justでくるまれた値のリストを渡すと、値のリストをJustに包んで返す" $ do
      sequenceA [Just 3, Just 4, Just 5] `shouldBe` Just [3,4,5] 
    it "Nothingがリストに含まれるとNothingを返す" $ do
      sequenceA [Just 3, Nothing, Just 5] `shouldBe` Nothing
    it "関数のリストを渡すとリストを返す関数を返す" $ do
      sequenceA [(+3), (+2), (+1)] 3 `shouldBe` [6,5,4]
    it "リストのリストを渡すと内部のリストの要素を一つづつとって作れるリストを列挙する" $ do
      sequenceA [[1,2,3],[4,5,6]] `shouldBe`
        [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
    it "空のリストがリストに含まれると空のリストを返す" $ do
      sequenceA [[1,2,3],[4,5,6], [], [7,8]] `shouldBe` []
    it "等価なリスト内包表記で表現する" $ do
      sequenceA [[1,2,3],[4,5,6,7]] `shouldBe` [[x,y]| x <- [1,2,3], y <- [4,5,6,7]]
    it "述語関数のリストを渡すと真理値のリストを返す関数を返す" $ do
      sequenceA [(>4), (<10), odd] 7 `shouldBe` [True, True, True]
    it "更にandに適用する" $ do
      (and $ sequenceA [(>4), (<10), odd] 7) `shouldBe` True

  describe "sequenceA'" $ do
    it "Justでくるまれた値のリストを渡すと、値のリストをJustに包んで返す" $ do
      sequenceA' [Just 3, Just 4, Just 5] `shouldBe` Just [3,4,5] 
    it "Nothingがリストに含まれるとNothingを返す" $ do
      sequenceA' [Just 3, Nothing, Just 5] `shouldBe` Nothing
    it "関数のリストを渡すとリストを返す関数を返す" $ do
      sequenceA' [(+3), (+2), (+1)] 3 `shouldBe` [6,5,4]
    it "リストのリストを渡すと内部のリストの要素を一つづつとって作れるリストを列挙する" $ do
      sequenceA' [[1,2,3],[4,5,6]] `shouldBe`
        [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]      