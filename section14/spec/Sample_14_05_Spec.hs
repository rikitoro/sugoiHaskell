module Sample_14_05_Spec where

import Test.Hspec
import Sample_14_05
import Control.Monad.Writer
import Control.Monad.State

main = hspec $ do
  describe "liftM" $ do
    it "Just値に適用" $ do
      liftM (*3) (Just 8) `shouldBe` Just 24
    it "リストに適用" $ do
      liftM (*3) [1..3] `shouldBe` [3,6,9]
    it "Writer Monadに適用" $ do
      (runWriter $ liftM not $ writer (True, "chickpeas"))
        `shouldBe` (False, "chickpeas")
    it "State Monadに適用" $ do
      (runState (liftM (+100) pop) [1,2,3,4])
       `shouldBe` (101,[2,3,4])
  describe "ap" $ do
    it "Justにした関数とJust値に適用" $ do
      Just (+3) `ap` Just 4 `shouldBe` Just 7
    it "関数とリストとリストに適用" $ do
      [(+1), (+2), (+3)] `ap` [10,11] `shouldBe` [11,12,12,13,13,14]
  describe "join" $ do
    it "ネストしたJustを平らにする" $ do
      join (Just (Just 9)) `shouldBe` Just 9
    it "Just NothingをNothingにする" $ do
      join (Just Nothing) `shouldBe` (Nothing :: Maybe String)
    it "Nothingはそのまま" $ do
      join Nothing `shouldBe` (Nothing :: Maybe String)
    it "ListのListを平らにする" $ do
      join [[1,2,3],[4,5,6]] `shouldBe` [1,2,3,4,5,6]
    it "Writer値のWriter値を平らにする" $ do
      (runWriter $ join (writer (writer (1, "aaa"), "bbb")))
        `shouldBe` (1, "bbbaaa")
    it "Right値のRight値を平らにする" $ do
      (join (Right (Right 9)) :: Either String Int) `shouldBe` Right 9
    it "Left値のRight値を平らにする" $ do 
      (join (Right (Left "error")) :: Either String Int) `shouldBe` Left "error"
    it "Left値はそのまま" $ do
      (join (Left "error") :: Either String Int) `shouldBe` Left "error"
    it "State値のState値を平らにする" $ do
      runState (join (state $ \s -> (push 10, 1:2:s))) [0,0,0]
        `shouldBe` ((),[10,1,2,0,0,0])
  describe "filterM" $ do
    it "モナディックな述語関数とリストを渡すと述語関数でフィルタリングする" $ do
      (fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]) 
        `shouldBe` [1,2,3]
    it "モナディックな述語関数とリストを渡すと述語関数でフィルタリングしたログを返す" $ do
      (snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]) 
        `shouldBe` ["9 is too large", "Keeping 1", "5 is too large", "Keeping 2", "10 is too large", "Keeping 3"]
  describe "powerset" $ do
    it "リストを渡すと冪集合を返す" $ do
      powerset [1,2,3] `shouldBe` [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
  describe "binSmalls" $ do
    it "foldMで9以下の数からなるリストをbinSmallsでたたみ込むと総和をJust値として返す" $ do
      foldM binSmalls 0 [2,8,3,1] `shouldBe` Just 14
    it "foldMで9より大きなの数があるリストをbinSmallsでたたみ込むとNothingを返す" $ do
      foldM binSmalls 0 [2,8,11,3,1] `shouldBe` Nothing
