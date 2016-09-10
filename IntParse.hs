import Data.Char (digitToInt)
import Data.Char (toUpper)

asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc []     = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
upperCase [] = []

upperCase' xs = map toUpper xs

--所有可以用 foldr 定义的函数，统称为主递归（primitive recursive）

myFilter f xs = foldr step [] xs
    where step x ys | f x  = x : ys
                    | otherwise = ys

myFilter' f xs = foldl step [] xs
    where step ys x | f x = x : ys
                    | otherwise = ys


foldll f v xs = g xs v
    where
        g []     = id
        g (x:xs) = \v -> g xs (f v x)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)


--seq 函数：它强迫（force）求值传入的第一个参数，然后返回它的第二个参数。
--要正确地产生 seq 的作用，表达式中被求值的第一个必须是 seq
foldl'' _ zero [] = zero
foldl'' step zero (x:xs) =
    let new = step zero x
    in new `seq` foldl' step new xs