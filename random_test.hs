import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNum, newG) = randomR (1,10) gen ::(Int, StdGen)
    putStr "put a num:"
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNum == number
            then putStrLn "Yes"
            else putStrLn $ "wrong " ++ show randNum
        askForNumber newG