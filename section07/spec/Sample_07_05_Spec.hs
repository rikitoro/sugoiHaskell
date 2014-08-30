module Sample_07_05_Spec where

import Test.Hspec 
import Sample_07_05
import Control.Exception (evaluate)

main = hspec $ do
  describe "Person" $ do
    it "フィールドが異なるPersonオブジェクトは/=である" $ do
      mca /= adRock `shouldBe` True
    it "自分自身は==である" $ do
      mikeD == mikeD `shouldBe` True
    it "フィールドが同じオブジェクトは==である" $ do
      mikeD == Person {firstName = "Michael", age = 43, lastName = "Diamond"} 
        `shouldBe` True

    it "Person型のオブジェクトに対してelemが使用できる" $ do
      let beastieBoys = [mca, adRock, mikeD]
      mikeD `elem` beastieBoys `shouldBe` True

    describe "show" $ do
      it "オブジェクトを文字列で表現する" $ do
        show mikeD `shouldBe` "Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
    describe "read" $ do
      it "文字列をオブジェクトに変換する" $ do
        let mysteryDude = "Person {firstName = \"Adam\", lastName = \"Yauch\", age = 44}"
        read mysteryDude `shouldBe` mca

  describe "Bool" $ do
    it "True `compare` False はGT" $ do
      True `compare` False `shouldBe` GT
    it "True > False が成り立つ" $ do
      True > False `shouldBe` True
    it "True < False は成り立たない" $ do
      True < False `shouldBe` False

  describe "Maybe" $ do
    it "NathingはJust somethingより小さい" $ do
      Nothing < Just 100 `shouldBe` True
    it "2つのJust値は中身を比較する" $ do
      Just 100 > Just 50 `shouldBe` True

  describe "Day" $ do
    describe "Show, Read" $ do
      describe "show" $ do
        it "曜日を文字列に変換する" $ do
          show Thursday `shouldBe` "Thursday"
      describe "read" $ do
        it "文字列を曜日に変換する" $ do
          (read "Saturday" :: Day) `shouldBe` Saturday

    describe "Eq, Ord" $ do
      describe "/=" $ do
        it "Saturday /= Sunday" $ do
          Saturday /= Sunday `shouldBe` True
      describe "==" $ do
        it "Friday == Friday" $ do
          Friday == Friday `shouldBe` True
      describe ">" $ do
        it "SaturdayはFridayより後" $ do
          Saturday > Friday `shouldBe` True

    describe "Bounded" $ do
      describe "minBound" $ do
        it "下限はMonday" $ do
          minBound `shouldBe` Monday
        it "上限はSunday" $ do
          maxBound `shouldBe` Sunday
    describe "Enum" $ do
      it "succは次の曜日を与える" $ do
        succ Monday `shouldBe` Tuesday
      it "succにSundayを渡すとエラーを投げる" $ do
        evaluate (succ Sunday) `shouldThrow` anyErrorCall
      it "[Thursday .. Saturday]" $ do
        [Thursday .. Saturday] `shouldBe` [Thursday, Friday, Saturday]
      it "[minBound .. maxBound]" $ do
        [minBound .. maxBound] `shouldBe` [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]