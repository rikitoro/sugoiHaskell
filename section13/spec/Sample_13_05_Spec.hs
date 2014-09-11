module Sample_13_05_Spec where

import Test.Hspec
import Sample_13_05

main = hspec $ do
  describe ">>=" $ do
    it "Maybeモナド値と値を取ってMaybeモナド値を返すラムダ関数をバインダでカスケードできる" $ do
      (Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))))
        `shouldBe` Just "3!"
    it "初めにNothingを渡すとNothingを返す" $ do
      (Nothing >>= (\x -> Just "!" >>= (\y -> Just (show (x :: Int) ++ y))))
        `shouldBe` Nothing
    it "途中でNothingを返す関数を渡すとNothingを返す" $ do
      (Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ (y :: String)))))
        `shouldBe` Nothing
    it "最後にNothingを返す関数を渡すとNothingを返す" $ do
      (Just 3 >>= (\x -> Just "!" >>= (\y -> (Nothing :: Maybe String))))
        `shouldBe` Nothing

  describe "foo" $ do
    it "バインドを使った記法と等価なdo記法" $ do
      (Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))))
        `shouldBe` foo 

  describe "routine" $ do
    it "ピエールは無事です！" $ do
      routine `shouldBe` Just (3, 3)
  describe "routineF" $ do
    it "あわれピエールは落っこちました" $ do
      routineF `shouldBe` Nothing
  describe "routineB" $ do
    it "途中バナナを踏んでしまったら落っこちる" $ do
      routineB `shouldBe` Nothing

  describe "justH" $ do
    it "パターンマッチは成功しJust 'H'を返す" $ do
      justH `shouldBe` Just 'H'
  describe "wopwop" $ do
    it "パターンマッチに失敗しNothingを返す" $ do
      wopwop `shouldBe` Nothing
      