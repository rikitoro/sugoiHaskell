module Sample_14_06_Spec where

import Test.Hspec
import Sample_14_06

main = hspec $ do
  describe "readMaybe" $ do
    it "数を表す文字列を渡すとその数をJustにて返す" $ do
      (readMaybe "12" :: Maybe Int) `shouldBe` Just 12
    it "数を表さない文字列を渡すとNothingを返す" $ do
      (readMaybe "GOTO HELL" :: Maybe Int) `shouldBe` Nothing
  describe "foldingFunction" $ do
    it "リストと*を渡すとトップ2つを掛け算したリストをJustに入れて返す" $ do
      foldingFunction [3,2] "*" `shouldBe` Just [6]
    it "リストと-を渡すとトップ2つを引き算したリストをJustに入れて返す" $ do
      foldingFunction [3,2] "-" `shouldBe` Just [-1]
    it "空のリストと*を渡すとNothingを返す" $ do
      foldingFunction [] "*" `shouldBe` Nothing
    it "リストと数を渡すとリストに数をプッシュする" $ do
      foldingFunction [2,3] "1" `shouldBe` Just [1,2,3]
    it "リストと無効な文字列を渡すとNothingを返す" $ do
      foldingFunction [] "1 wawawawa" `shouldBe` Nothing
  describe "solveRPN" $ do
    it "\"1 2 * 4 +\"を渡すとJust 6を返す" $ do
      solveRPN "1 2 * 4 +" `shouldBe` Just 6
    it "\"1 2 * 4 + 5 *\"を渡すとJust 30を返す" $ do
      solveRPN "1 2 * 4 + 5 *" `shouldBe` Just 30
    it "\"1 2 * 4\"を渡すと失敗してNothingを返す" $ do
      solveRPN "1 2 * 4" `shouldBe` Nothing
    it "\"1 8 wharglbllargh\"を渡すと失敗してNothingを返す" $ do
      solveRPN "1 8 wharglbllargh" `shouldBe` Nothing


