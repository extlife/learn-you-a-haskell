module Ch06.Geometry.Cuboid 
( volume
, area
) where

volume :: (Floating a) => a -> a -> a -> a
volume a b c = rectangleArea a b * c

area :: (Floating a) => a -> a -> a -> a
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 +
    rectangleArea c b * 2

rectangleArea :: (Floating a) => a -> a -> a
rectangleArea a b = a * b
