module Sample_07_04_Spec where

import Test.Hspec
import Sample_07_04

main :: IO ()
main = hspec $ do
  describe "tellCar" $ do
    let stang = Car {company = "Ford", model = "Mustang", year = 1967 }
    it "Car型のデータを渡すとわかりやすい情報を文字列で返す" $ do
      tellCar stang `shouldBe` "This Ford Mustang was made in 1967"

  describe "Vector" $ do
    describe "vplus" $ do
      it "2つのベクトルを加算" $ do
        Vector 3 5 8 `vplus` Vector 9 2 8 `shouldBe` Vector 12 7 16
      it "3つのベクトルを加算" $ do
        Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3 `shouldBe` Vector 12 9 19
    describe "vmult" $ do
      it "ベクトルをスカラー倍する" $ do
        Vector 1.0 2.0 3.0 `vmult` 4.0 `shouldBe` Vector 4.0 8.0 12.0
    describe "dotProd" $ do
      it "ベクトルの内積を取る" $ do
        Vector 1.0 2.0 3.0 `dotProd` Vector 2.0 1.0 4.0 `shouldBe` 16.0
      it "vmultと組み合わせた演算もできる" $ do
        Vector 2 9 3 `vmult` (Vector 4 9 5 `dotProd` Vector 9 2 4) 
          `shouldBe` Vector 148 666 222