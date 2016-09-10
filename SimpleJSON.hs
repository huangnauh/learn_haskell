module SimpleJSON
    (
        JValue(..)
    ,   getString
    ,   getInt
    ,   getBool
    ,   getObject
    ,   getArray
    ,   isNull
    ) where

newtype JAry a = JAry {
      fromJAry :: [a]
      } deriving (Eq, Ord, Show)


data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
                deriving (Eq, Show, Ord)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: Integral a => JValue -> Maybe a
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

--getObject :: JValue -> Maybe
getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing


isNull :: JValue -> Bool
isNull v            = v == JNull


