module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cubeidArea
, cubeidVolume
)where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cubeidVolume side side side

cubeidVolume :: Float -> Float -> Float -> Float
cubeidVolume a b c = rectangleArea a b * c

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

cubeArea :: Float -> Float
cubeArea side = cubeidArea side side side

cubeidArea :: Float -> Float -> Float -> Float
cubeidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2