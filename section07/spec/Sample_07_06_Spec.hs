module Sample_07_06_Spec where

import Test.Hspec 
import Sample_07_06

main = hspec $ do
  describe "inPhoneBook" $ do
    it "名前と電話番号が電話帳にあればTrue" $ do
      inPhoneBook "patsy" "777-8888" phoneBook `shouldBe` True
    it "電話帳に名前があっても電話番号がなければFalse" $ do
      inPhoneBook "patsy" "666-7777" phoneBook `shouldBe` False

  describe "lockerLookup" $ do
    it "使われていないロッカー番号を渡すと暗証番号を返す" $ do
      lockerLookup 101 lockers `shouldBe` Right "JAH3I"
    it "すでに使われているロッカー番号を渡すとその旨を返す" $ do
      lockerLookup 100 lockers `shouldBe` Left "Locker 100 is already taken!"
    it "存在しないロッカー番号を渡すとその旨を返す" $ do
      lockerLookup 102 lockers `shouldBe` Left "Locker 102 doesn't exit!"