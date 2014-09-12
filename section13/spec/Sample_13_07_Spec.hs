module Sample_13_07_Spec where

import Test.Hspec
--import Control.Monad
import Sample_13_07
import Sample_13_04_Maybe

main = hspec $ do
  describe "左恒等性" $ do
    it "Justモナドで成り立つ" $ do
      (return 3 >>= (\x -> Just (x+100000))) `shouldBe` Just 100003
    it "Listモナドで成り立つ" $ do
      (return "WoM" >>= (\x -> [x,x,x])) `shouldBe` (\x -> [x,x,x]) "WoM"
  describe "右恒等性" $ do
    it "Justモナドで成り立つ" $ do
      (Just "Move on up" >>= return) `shouldBe` Just "Move on up" 
    it "Listモナドで成り立つ" $ do
      ([1,2,3,4] >>= return) `shouldBe` [1,2,3,4]
  describe "結合法則" $ do
    it "Maybeモナドで成り立つ" $ do
      (return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2)
        `shouldBe` 
        (return (0,0)  >>= (\x -> landRight 2 x >>= (\y ->landLeft 2 y  >>= (\z -> landRight 2 z))))
  describe "(<=<)" $ do
    let f x = [x, -x]
    let g x = [x*3, x*2]
    let h x = [x+3, x+3]
    it "結合法則" $ do
      ((f <=< g) <=< h) 3 `shouldBe` (f <=< (g <=< h)) 3
    it "左恒等性" $ do
      (f <=< return) 3 `shouldBe` f 3
    it "右恒等性" $ do
      (return <=< g) 3 `shouldBe` g 3

