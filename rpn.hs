import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x1:x2:xs) "*" = (x1 * x2):xs
          foldingFunction (x1:x2:xs) "+" = (x1 + x2):xs
          foldingFunction (x1:x2:xs) "-" = (x2 - x1):xs
          foldingFunction (x1:x2:xs) "/" = (x2 / x1):xs
          foldingFunction (x1:x2:xs) "^" = (x2 ** x1):xs
          foldingFunction (x:xs) "ln" = log x:xs
          foldingFunction xs "sum" = [sum xs]
          foldingFunction xs numberString = read numberString:xs

liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x))

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
                    [(x, "")] -> Just x
                    _         -> Nothing

foldingFunction' :: [Double] -> String -> Maybe [Double]
foldingFunction' (x:y:ys) "*" = return ((x * y) : ys)
foldingFunction' (x:y:ys) "+" = return ((x + y) : ys)
foldingFunction' (x:y:ys) "-" = return ((y - x) : ys)
foldingFunction' xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN' :: String -> Maybe Double
solveRPN' st = do
    [result] <- foldM foldingFunction' [] (words st)
    return result
