module Sample_11_01_Spec where

import Test.Hspec
import Test.QuickCheck 
import Control.Monad.Instances
import Sample_11_01

main = hspec $ do
  describe "fmap" $ do
    it "関数ファンクタに対するfmapは関数合成である" $ do
      fmap (*3) (+100) 1 `shouldBe` 303
    it "中置表現" $ do
      ((* 3) `fmap` (+ 100) $ 1) `shouldBe` 303
    it ".と等価" $ do
      property $ \n -> ((* 3) `fmap` (+ 100) $ n :: Int) `shouldBe` ((*3) . (+100) $  n)
    it "show . (*3) を渡してみる" $ do
      fmap (show . (*3)) (+100) 1 `shouldBe` "303"
  describe "fmap (++\"!!\")" $ do
    let shout = fmap (++"!!")
    it "文字列のリストを渡すと元気よく!!" $ do
      shout ["ha", "ka", "ta", "no"] `shouldBe` ["ha!!", "ka!!", "ta!!", "no!!"]
  describe "fmap (replicate 3)" $ do
    it "リストを渡すと各要素を3回繰り返したリストからなるリストを返す" $ do
      rep3 [1,2,3,4] `shouldBe` [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
    it "Just nを渡すとnを3回繰り返したリストのJust値が得られる" $ do
      rep3 (Just 4) `shouldBe` Just [4,4,4]
    it "Notingを渡すとNothingを返す" $ do
      rep3 (Nothing :: Maybe [Int]) `shouldBe` Nothing
    it "Left値を渡すとLeft値をそのまま返す" $ do
      rep3 (Left "foo" :: Either String Int) `shouldBe` (Left "foo")
    it "Right値を渡すとその中身を3回繰り返したRight値を返す" $ do
      rep3 (Right 5 :: Either String Int) `shouldBe` (Right [5,5,5])
