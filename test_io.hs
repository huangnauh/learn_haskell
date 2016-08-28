import qualified Control.Monad as CM
import qualified Data.Char as C
import System.IO as IO

main1 = do
    line <- getLine
    if null line
    then return ()
    else do
        putStrLn $ reserveWords line
        main1

reserveWords :: String -> String
reserveWords = unwords . map reverse . words

main2 = do
    c <- getChar
    CM.when (c /= ' ') $ do
        putChar c
        main2

main3 = do
    rs <- CM.sequence [getLine, getLine, getLine]
    print rs

main' = do
    colors <- CM.forM [1,2,3,4] (\a -> do
        putStrLn $ "color " ++ show a ++ "?"
        getLine)
    putStrLn "colors:"
    mapM putStrLn colors

main4 = CM.forever $ do
    putStr "input:"
    l <- getLine
    putStrLn $ map C.toUpper l

main5 = do
    contents <- getContents
    putStr(map C.toUpper contents)

main6 = do
    contents <- getContents
    putStr (shortLineOnly contents)

shortLineOnly :: String -> String
shortLineOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result

main7 = interact $ unlines . filter ((<10) . length) . lines

respondPalindromes = unlines . map (\xs ->
    if isPalindrome xs then "Palindrome" else "not a Palindrome") . lines
        where isPalindrome xs = xs == reverse xs

main8 = do
    handle <- openFile "test.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

main9 = do
    withFile' "test.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

main10 = do
    contents <- readFile "test.txt"
    putStr contents

main = do
    contents <- readFile "test.txt"
    writeFile "testcaps.txt" (map C.toUpper contents)
    