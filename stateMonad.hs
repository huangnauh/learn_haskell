import System.Random
import Control.Monad.State
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack ()
stackManip = do
    a <- pop
    if a == 5
        then do
            push 5
        else do
            push 3
            push 8

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [3,2,1]
        else put [1,2,3]

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a,b,c)