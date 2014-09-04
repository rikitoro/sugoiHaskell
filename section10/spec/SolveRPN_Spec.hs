module SolveRPN_Spec where

import Test.Hspec 
import SolveRPN

main = hspec $ do
  describe "solveRPN" $ do
    it "四則演算子が1つの逆ポーランド記法に対して計算ができる" $ do
      solveRPN "3 4 *" `shouldBe` 12
    it "四則演算子が2つの逆ポーランド記法に対して計算ができる" $ do
      solveRPN "3 4 * 2 +" `shouldBe` 14
    it "四則演算子が多くある逆ポーランド記法に対して計算ができる" $ do
      solveRPN "10 4 3 + 2 * -" `shouldBe` (-4)
    it "lnを計算できる" $ do
      solveRPN "10 ln" `shouldBe` log 10
    it "sumで和を総和を計算できる" $ do
      solveRPN "1 2 3 4 sum 2 /" `shouldBe` 5