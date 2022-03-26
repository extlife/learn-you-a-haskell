module Ch06.Geometry.Cube
( volume
, area
) where

import qualified Ch06.Geometry.Cuboid as Cuboid

volume :: (Floating a) => a -> a
volume side = Cuboid.volume side side side

area ::  (Floating a) => a -> a
area side = Cuboid.area side side side
