module Heathrow_Spec where

import Test.Hspec 
import Heathrow

main = hspec $ do
  describe "groupsOf" $ do
    it "3と配列を渡すと配列の要素を3個づつにまとめる" $ do
      groupsOf 3 [1..10] `shouldBe` [[1,2,3],[4,5,6],[7,8,9],[10]]
  describe "roadStep" $ do
    it "初めのセクションの最短経路を求める" $ do
      (roadStep ([],[]) $ Section 50 10 30) `shouldBe` ([(C,30), (B,10)],[(B,10)])
    it "2つ目のセクションまでの最短経路を求める" $ do
      (roadStep ([(C,30), (B,10)],[(B,10)]) $ Section 5 90 20) `shouldBe`
        ([(A, 5),(C,30),(B,10)],[(C,20),(A,5),(C,30),(B,10)])
  describe "optimalPath" $ do
    it "ヒースロー空港からロンドンまでのRoadSystemを渡すと最短経路を求める" $ do
      let roadSystem = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
      optimalPath roadSystem `shouldBe` [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]

