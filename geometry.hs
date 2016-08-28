module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cubeidArea
, cubeidVolume
, Point(..)
, Shape(..)
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

data Point = Point Float Float deriving (Show)
--型别与值构造子用了相同的名字

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
--Circle 并非型别，型别应该是 Shape

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


baseCircle :: Float -> Shape
baseCircle r  = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Record Syntax
data Person = Person {
    firstName   :: String
   ,lastName    :: String
   ,age         :: Int
   ,height      :: Float
   ,phoneNumber :: String
   ,flavor      :: String
} deriving (Show)


data Car = Car {
    company :: String
   ,model   :: String
   ,year    :: Int
} deriving (Show, Eq, Read)

tellCar :: Car -> String
tellCar (Car {company=c, model=m, year=y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Car' a b c = Car' {
    company' :: a
   ,model'   :: b
   ,year'    :: c
} deriving (Show)

--型别构造子和值构造子的区分是相当重要的。在声明数据型别时，等号=左端的那个是型别构造子，右端的(中间可能有|分隔)都是值构造子。
--因为在型别声明中只能写型别

tellCar' :: (Show a) => Car' String String a -> String
tellCar' (Car' {company'=c, model'=m, year'=y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


