module Sample_06_04_Spec where

import Test.Hspec
import Test.QuickCheck

import Geometry
import Geometry.Sphere as Sphere
import Geometry.Cube as Cube
import Geometry.Cuboid as Cuboid

main :: IO ()
main = hspec $ do
  describe "Geometry module" $ do
    it "sphereVolume r は半径rの球の体積を求める" $ do
      sphereVolume 2.0 `shouldBe` (4.0/3.0) * pi * (2.0 ^ 3)
  describe "Sphere module" $ do
    it "volume rは半径rの球の体積を求める(sphereVolume)" $ do
      property $ \r -> Sphere.volume r == sphereVolume r
    it "area rは半径rの球の表面積を求める(sphereArea)" $ do
      property $ \r -> Sphere.area r == sphereArea r
  describe "Cuboid module" $ do
    it "volumeは直方体の体積を求める(cuboidVolume)" $ do
      property $ \a b c -> Cuboid.volume a b c == cuboidVolume a b c
    it "areaは直方体の表面積を求める(cuboidArea)" $ do
      property $ \a b c -> Cuboid.area a b c == cuboidArea a b c
  describe "Cube module" $ do
    it "valumeは立方体の体積を求める(cubeVolume)" $ do
      property $ \a -> Cube.volume a == cubeVolume a
    it "valumeは立方体の表面積を求める(cubeArea)" $ do
      property $ \a -> Cube.area a == cubeArea a



