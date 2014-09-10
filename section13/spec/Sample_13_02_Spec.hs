module Sample_13_02_Spec where

import Test.Hspec
import Sample_13_02

main = hspec $ do
  describe "applyMaybe" $ do
    it "Just 3と\\x-> Just (x+3) を渡すとJust 4を返す" $ do
      Just 3 `applyMaybe` ((\x -> Just (x + 1)) :: Int -> Maybe Int) `shouldBe` Just 4
    it "Nothingと\\x-> Just (x+3) を渡すとNothingを返す" $ do
      Nothing `applyMaybe` ((\x -> Just (x + 1)) :: Int -> Maybe Int) `shouldBe` Nothing
    it "Just \"smile\"と\\x -> Just (x ++ \" :)\")を渡すとスマイルをつけたJust値を返す" $ do
      Just "smile" `applyMaybe` ((\x -> Just (x ++ " :)")) :: String -> Maybe String)
        `shouldBe` Just "smile :)"
    it "Nothingと\\x -> Just (x ++ \" :)\")を渡すとNothingを返す" $ do
      Nothing `applyMaybe` ((\x -> Just (x ++ " :)")) :: String -> Maybe String)
        `shouldBe` Nothing
    context "第2引数の関数のほうがNothingを返す場合があるとき" $ do
      it "第一引数が、関数がJust値を返すような条件を満たすときはJust値を返す" $ do
        Just 3 `applyMaybe` ((\x -> if x > 2 then Just x else Nothing) :: Int -> Maybe Int)
          `shouldBe` Just 3
      it "第一引数が、関数がNothingを返すような条件を満たすときはNothingを返す" $ do
        Just 1 `applyMaybe` ((\x -> if x > 2 then Just x else Nothing) :: Int -> Maybe Int)
          `shouldBe` Nothing
  describe "Maybe Monad" $ do
    it "returnはJustと等価" $ do
      (return "WHAT" :: Maybe String) `shouldBe` Just "WHAT"
    it "Just値と関数をバインダで結合する" $ do
      (Just 9 >>= ((\x -> return (x*10)) :: Int -> Maybe Int)) `shouldBe` Just 90
    it "Nothing値と関数をバインダで結合する" $ do
      (Nothing >>= ((\x -> return (x*10)) :: Int -> Maybe Int)) `shouldBe` Nothing
