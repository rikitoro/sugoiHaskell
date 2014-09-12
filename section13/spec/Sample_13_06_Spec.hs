module Sample_13_06_Spec where

import Test.Hspec
import Control.Applicative
import Control.Monad
import Sample_13_06

main = hspec $ do
  describe "リストモナド" $ do
    it "アプリカティブスタイルで適用する" $ do
      (*) <$> [1, 2, 3] <*> [10, 100, 1000] 
        `shouldBe` [10, 100, 1000, 20, 200, 2000, 30, 300, 3000]
    describe ">>=" $ do
      it "リストと値からリストを返す関数とをバインダで結合できる" $ do
        ([3, 4, 5] >>= \x -> [x, -x]) `shouldBe` [3, -3, 4, -4, 5, -5]
      it "最初に空のリストを渡すと空のリストを返す" $ do
        ([] >>= \x -> [x, -x]) `shouldBe` []
      it "途中に空リストを返す関数を渡すと空リストを返す" $ do
        ([3, 4, 5] >>=  \x -> ([] :: [] Int)) `shouldBe` []
      it "バインドのカスケード" $ do
        ([1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch))
          `shouldBe` [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]
    describe "listOfTuples" $ do
      it "バインドのカスケードと等価なdo記法" $ do  
        ([1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch))
          `shouldBe` listOfTuples
      it "等価なリスト内包表記" $ do
        listOfTuples `shouldBe` [(n, ch) | n <- [1, 2], ch <- ['a', 'b']]
  describe "guard" $ do
    it "リスト内包表記で出力する要素を選別できる" $ do
      [x | x <- [1..50], '7' `elem` show x ] `shouldBe` [7, 17, 27, 37, 47]
    it "Maybeモナドの文脈ではTrueを渡すとJust ()を返す" $ do
      guard (5 > 2) `shouldBe` Just ()
    it "リストモナドの文脈ではTrueを渡すと[()]を返す" $do
      guard (5 > 2) `shouldBe` [()]
    it "Maybeモナドの文脈ではFalseを渡すとNothingを返す" $ do
      guard (2 > 5) `shouldBe` Nothing
    it "リストモナドの文脈ではTrueを渡すと[]を返す" $do
      guard (2 > 5) `shouldBe` []
    it "Trueを渡して>>につなぐと計算を続ける" $ do
      (guard (5 > 2) >> return "cool") `shouldBe` ["cool"]
    it "Falseを渡して>>につなぐと計算に失敗する" $ do
      (guard (1 > 2) >> return "cool") `shouldBe` []
    it "guardで解の候補を選別できる" $ do
      ([1..50] >>= (\x -> guard ('7' `elem` show x) >> return x))
        `shouldBe` [7, 17, 27, 37, 47]
    it "等価なdo記法" $ do
      sevensOnly `shouldBe` [7, 17, 27, 37, 47]


