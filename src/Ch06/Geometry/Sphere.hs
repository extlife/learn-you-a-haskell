module Geometry.Sphere
( volume
, area
) where

volume :: (Floating a) => a -> a
volume radius = 4 / 3 * pi * radius ^ 3

area :: (Floating a) => a -> a
area radius = 4 * pi * radius ^ 2
