module ListZipper_Spec where

import Test.Hspec
import ListZipper

main = hspec $ do
  describe "ListZipper" $ do
    let xs = [1,2,3,4]
    it "goForwardする" $ do
      (xs,[]) -: goForward `shouldBe` ([2,3,4],[1])
    it "さらにgoForwardする" $ do
      (xs,[]) -: goForward -: goForward `shouldBe` ([3,4],[2,1])
    it "さらにgoForwardする" $ do
      (xs,[]) -: goForward -: goForward -: goForward `shouldBe` ([4],[3,2,1])
    it "goBackでひとつもどす" $ do
      (xs,[]) -: goForward -: goForward -: goForward -: goBack `shouldBe` ([3,4],[2,1])



