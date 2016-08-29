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
