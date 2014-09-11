module Sample_13_06_Spec where

import Test.Hspec
import Control.Applicative
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
