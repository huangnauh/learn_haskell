import System.IO
import System.Directory
import Data.List
import System.Environment

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
            ,("view", view)
            ,("remove", remove)
           ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [filename] = do
    contents <- readFile filename
    let tasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        tasks = lines contents
        items = delete (tasks !! number) tasks
    hPutStr tempHandle $ unlines items
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

main0 = do
    handle <- openFile "test.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let tasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
    putStrLn "items:"
    putStr $ unlines numberedTasks
    putStrLn "delete?"
    numberString <- getLine
    let number = read numberString
        newItems = delete (tasks !! number) tasks
    hPutStr tempHandle $ unlines newItems
    hClose handle
    hClose tempHandle
    renameFile tempName "test1.txt"

main1 = do
    args <- getArgs
    progName <- getProgName
    putStrLn "args:"
    mapM putStrLn  args
    putStrLn "name:"
    putStrLn progName