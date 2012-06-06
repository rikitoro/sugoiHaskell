import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid

sv10 :: Float
sv10 = Sphere.volume 10.0

sa10 :: Float
sa10 = Sphere.area 10.0

cubev10 :: Float 
cubev10 = Cube.volume 10.0

cubea10 :: Float
cubea10 = Cube.area 10.0

cuboidv123 :: Float
cuboidv123 = Cuboid.volume 1 2 3

cuboida123 :: Float
cuboida123 = Cuboid.area 1 2 3