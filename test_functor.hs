--import Control.Applicative

data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
    fmap f CNothing = CNothing
--  break functor laws
    fmap f (CJust counter x) = CJust (counter+1) (f x)


--class (Functor f) => Applicative f where
--    pure  :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> somthing = fmap f something