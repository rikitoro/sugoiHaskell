module Sample_07_03_Spec where

import Test.Hspec
import Sample_07_03

main = hspec $ do
  describe "Person" $ do
    let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
    it "firstNameでファーストネームを取得できる" $ do
      firstName guy `shouldBe` "Buddy"
    it "ageで年齢を取得できる" $ do
      age guy `shouldBe` 43
    it "flavorで好きなアイスフレーバーを取得できる" $ do
      flavor guy `shouldBe` "Chocolate"

  describe "Car" $ do
    let myCar = Car {year = 1967,company = "Ford", model = "Mustang"}
    it "yearで製造年を取得できる" $ do
      year myCar `shouldBe` 1967
    it "companyでメーカーが取得できる" $ do
      company myCar `shouldBe` "Ford"
    it "modelでモデル名が取得できる" $ do
      model myCar `shouldBe` "Mustang"