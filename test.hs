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