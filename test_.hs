import qualified Data.Foldable as F
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                    f x `mappend`
                    F.foldMap f r
