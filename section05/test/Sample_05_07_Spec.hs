module Sample_05_07_Spec where

import Test.Hspec
import Test.QuickCheck
import Sample_05_07

main :: IO ()
main = hspec $ do
  describe "関数合成" $ do
    it "\\x -> nagate (abs x) を.で構成する" $ do
      property $ \xs -> map (\x -> negate (abs x)) xs == (map (negate . abs) xs :: [Int])
    it "\\xs -> negate (sum (tail xs))を.で構成する" $ do
      map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]] `shouldBe` map (negate . sum . tail) [[1..5],[3..6],[1..7]]
  describe "多引数関数の関数合成" $ do
    it "sum (replicate n (max x y))を.で構成する" $ do
      property $ \n x y -> sum (replicate n (max x y)) ==  (sum . replicate n $ max x y  :: Float)
    it "replicate 2 (product (map (*3) (zipWith max xs ys)))を.で構成する" $ do
      property $ \xs ys -> replicate 2 (product (map (*3) (zipWith max xs ys))) == (replicate 2 . product . map (*3) $ zipWith max xs ys :: [Float])
  describe "oddSquareSumとoddSquareSum'" $ do
    it "両者は等価である" $ do
      oddSquareSum `shouldBe` oddSquareSum'
          