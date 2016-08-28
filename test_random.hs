import System.Random

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newG) = random gen
               in value:randoms' newG

finiteRandom' :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandom' 0 gen = ([], gen)
finiteRandom' n gen =
    let (value, newG) = random gen
        (restofList, finalG) = finiteRandom' (n-1) newG
    in (value:restofList, finalG)

main = do
    gen <- getStdGen
    let randomChars = randomRs ('a', 'z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStrLn second20
