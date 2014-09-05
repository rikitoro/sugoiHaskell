module Sample_11_03_Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative

main = hspec $ do
  describe "fmap" $ do
    let a = fmap (*) [1..4]
    it "関数が入ったファンクタ値に値を適応する" $ do
      fmap ($ 9) a `shouldBe` [9,18,27,36]
  describe "<*>" $ do
    it "Justに包んだ関数とJustに包んだ値を渡すと値に関数を適用した結果をJustに包んで返す" $ do
      Just (+ 3) <*> Just 9 `shouldBe` Just 12
    it "Justに包んだ関数とNothingを渡すとNothingを返す" $ do
      Just (++ "Hahaha") <*> Nothing `shouldBe` Nothing
    it "NothingとJustに包んだ値を渡すとNothingを返す" $ do
      Nothing <*> Just "woot" `shouldBe` (Nothing :: Maybe String)
  describe "アプリカティブスタイル" $ do
    it "2引数関数とJust値2つを渡すと値を関数に適用してJust値にして返す" $ do
      pure (+) <*> Just 3 <*> Just 5 `shouldBe` Just 8
    it "第1引数にNothingを渡すとNothingを返す" $ do
      pure (+) <*> Nothing <*> Just 5 `shouldBe` Nothing
    it "第2引数にNothingを渡すとNothingを返す" $ do
      pure (+) <*> Just 3 <*> Nothing `shouldBe` Nothing
  describe "<$>" $ do
    it "2引数関数とJust値2つを渡すと値を関数に適用してJust値にして返す" $ do
      (++) <$> Just "johntra" <*> Just "volta" `shouldBe` Just "johntravolta"

  describe "リスト[]はアプリカティブファンクタ" $ do
    describe "pure" $ do
      it "値を渡すとリストに入れて返す" $ do
        property $ \str -> pure str `shouldBe` ([str] :: [String])
    describe "<*>" $ do
      it "関数のリストと値のリストを渡すと各関数にそれぞれの値を適用してリストにして返す" $ do
        [ (* 0), (+100), (^2)] <*> [1,2,3] `shouldBe` [0,0,0,101,102,103,1,4,9]  
      it "アプリカティブスタイルをリストで使う" $ do
        (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."]
          `shouldBe` ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
      it "リスト内包表記を置き換えることができる" $ do
        property $ \xs ys -> (*) <$> (xs :: [Int]) <*> (ys :: [Int])
          `shouldBe` [ x*y | x <- xs, y <- ys]
      it "filterと組み合わせて使う" $ do
        (filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]) `shouldBe` [55,80,100,110]
