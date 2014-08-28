module Sample_05_06_Spec where

import Test.Hspec
main :: IO ()
main = hspec $
  describe "$" $ do
    it "sum (filter (>10) (map (*2) [2..10]))と等価な書き方" $ do
      (sum $ filter (> 10) $ map (* 2) [2..10]) `shouldBe` sum (filter (>10) (map (*2) [2..10]))
    it "($ 3)を関数に適応するとその関数に3を適応する" $ do
      map ($ 3) [(4+), (10*), (^2), sqrt] `shouldBe` [7,30,9,sqrt 3]