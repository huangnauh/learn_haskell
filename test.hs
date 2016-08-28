import qualified Data.Map as M
import qualified Data.List as L

doubleMe :: Int -> Int
doubleMe x = x + x
doubleUs :: Int -> Int -> Int
doubleUs x y = x*2 + y*2

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
--addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' [] = error"call head on an empty list"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of []    -> error "call head on an empty list"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a long list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what []  = "empty."
          what [x] = "a singleton list."
          what xs  = "a longer list."


length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

tell :: (Show a) => [a] -> String
tell [] = ""
tell (x:xs) = show x ++ " " ++ tell xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

summ :: (Num a) => [a] -> a
summ = foldl (+) 0

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "underweight"
    | weight / height ^ 2 <= 25.0 = "normal"
    | weight / height ^ 2 <= 30.0 = "fat"
    | otherwise = "whale"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= fat    = "fat"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <=0 = []
    | otherwise = x:replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <=0 = []
take' _ []  = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
--elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger  = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smaller = quicksort (filter (<=x) xs)
        bigger  = quicksort (filter (>x) xs)
    in smaller ++ [x] ++ bigger


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd  n = n:chain (n*3 + 1)

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (L.tails haystack)


findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey key xs


findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

phoneBook =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> M.Map k String
phoneBookToMap xs = M.fromListWith (\acc x -> acc ++ ", " ++ x) xs


data Either' a b = Left' a | Right' b deriving (Show, Eq)

instance Functor (Either' a) where
    fmap f (Left' x) = Left' x
    fmap f (Right' x) = Right' (f x)


data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either' String Code
lockerLookup lockerNumber map =
    case M.lookup lockerNumber map of
        Nothing -> Left' $ "Locker number " ++ show lockerNumber ++ " not exist"
        Just (status, code) -> if status /= Taken
                               then Right' code
                               else Left' $ "Locker " ++ show lockerNumber ++ " is already taken"


lockers :: LockerMap
lockers = M.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

infixr 5 :-:
data List' a = Empty | a :-: (List' a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List' a -> List' a -> List' a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

nums = [8,6,4,1,7,3,5]
numsTree =foldr treeInsert EmptyTree nums

--class Eq' a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool
--    x == y = not (x /= y)
--    x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red   == Red   = True
    Green == Green = True
    Yellow== Yellow= True
    _     == _     = False

instance Show TrafficLight where
    show Red    = "Red Light"
    show Yellow = "Yellow Light"
    show Green  = "Green Light"

class YesNo a where
    yesno :: a -> Bool


instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

yesnoif :: (YesNo y) => y -> a -> a -> a
yesnoif yesnoVal yesnoResult noResult =
    if yesno yesnoVal then yesnoResult else noResult

class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
    fmap f (Barry {yabba=x, dabba=y}) = Barry{yabba= f x, dabba=y}
