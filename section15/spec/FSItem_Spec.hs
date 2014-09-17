module FSItem_Spec where

import Test.Hspec
import FSItem

main = hspec $ do
  describe "ファイルシステムの中を移動する" $ do
    let newFocus1 = (myDisk, []) -: fsTo "pics" -: fsTo "skull.bpm"
    it "fsToでフォルダを下りファイルを参照する" $ do
      fst newFocus1 `shouldBe` File "skull.bpm" "Yikes"
    let newFocus2 = newFocus1 -: fsUp -: fsTo "watermelon.gif"
    it "fsUpで一つ上に上がりfsToで別のファイルを参照する" $ do
      fst newFocus2 `shouldBe` File "watermelon.gif" "smash"
  describe "ファイルシステムの操作" $ do
    let newFocus1 = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp
    it "fsRenameでpicsフォルダの名前をcspiに変更する" $ do
      (fst $ newFocus1 -: fsTo "cspi" -: fsTo "ape.jpg")
        `shouldBe` File "ape.jpg" "bleargh"
    let newFocus2 = newFocus1 -: fsTo "cspi" -: fsNewFile (File "heh.jpg" "lol") -: fsUp
    it "fsNewFileでファイルを追加する" $ do
      (fst $ newFocus2 -: fsTo "cspi" -: fsTo "heh.jpg") 
        `shouldBe` File "heh.jpg" "lol"