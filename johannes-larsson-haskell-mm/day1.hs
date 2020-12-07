import System.IO

readInt :: String -> Int
readInt = read

createGroups :: [a] -> [(a, a)]
createGroups [] = []
createGroups (x:xs) = map ((,) x) xs ++ createGroups xs

createGroups2 :: [a] -> [(a, (a, a))]
createGroups2 [] = []
createGroups2 (x:xs) = map ((,) x) (createGroups xs) ++ createGroups2 xs

testSet :: (Int, Int) -> (Bool, Int)
testSet (a,b) = (a+b==2020, a*b)

testSet2 :: (Int, (Int, Int)) -> (Bool, Int)
testSet2 (a,(b,c)) = (a+b+c==2020, a*b*c)

part1 = do
    f <- openFile "day1" ReadMode
    cont <- hGetContents f
    let ls = lines cont
    let is = map readInt ls
    let s = map testSet $ createGroups is
    let r = filter fst s
    putStrLn $ show $ snd $ r!!0
	
part2 = do
    f <- openFile "day1" ReadMode
    cont <- hGetContents f
    let ls = lines cont
    let is = map readInt ls
    let s = map testSet2 $ createGroups2 is
    let r = filter fst s
    putStrLn $ show $ snd $ r!!0