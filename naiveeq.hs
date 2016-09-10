{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances #-}
import Data.List
import Data.Char (isSpace)
import Data.Char (isDigit)
data Color = Red | Green | Blue
data Time = Time {hour :: Int,
                  minute :: Int
                 }

instance Show Color where
    show Red   = "Color 1: Red"
    show Green = "Color 2: Green"
    show Blue  = "Color 3: Blue"

instance Read Color where
    readsPrec _ value =
        tryParse [("Red", Red), ("Green", Green), ("Blue",Blue)]
        where tryParse [] = []
              tryParse ((attempt, result):xs) =
                    if (take (length attempt) value) == attempt
                        then [(result, drop (length attempt) value)]
                        else tryParse xs


instance Show Time where
  show (Time hour minute) = (if hour > 9
                             then (show hour)
                             else ("0" ++ show hour))
                            ++ ":" ++
                            (if minute > 9
                             then (show minute)
                             else ("0" ++ show minute))

newTime h m | between 0 23 h && between 0 59 m = Time h m
            | otherwise = error "newTime: hours must be in range 0-23 and minutes 0-59"
     where between low high val = low <= val && val <= high

--instance Read Time where
--    readsPrec _ input =
--        let (hours, rest1) = span isDigit input
--            hour = read hours :: Int
--            (c:rest2) = rest1
--            (mins,rest3) = splitAt 2 rest2
--            minute = read mins :: Int
--        in
--            if c ==':' && all isDigit mins && length mins == 2
--                then [(newTime hour minute, rest3)]
--                else []

instance Read Time where
    readsPrec _ (h1:h2:':':m1:m2:therest) =
        let hour   = read [h1,h2] :: Int
            minute = read [m1,m2] :: Int
        in if all isDigit [h1,h2,m1,m2]
              then [(newTime hour minute,therest)]
              else []
    readsPrec _ _ = []

class Foo a where
    foo :: a -> String

instance Foo a => Foo [a] where
    foo = concat . intersperse ", " . map foo

instance Foo Char where
    foo c = [c]

instance Foo String where
    foo = id




