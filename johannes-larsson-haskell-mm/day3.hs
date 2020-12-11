import System.IO ( openFile, hGetContents, IOMode(ReadMode) )

takeChar :: [String] -> (Int, Int) -> Char
takeChar ls (c, l) = (ls !! l) !! c

checkSlope :: [String] -> (Int, Int) -> Int
checkSlope ls (r,d) = 
    length $ filter (== '#') (map (takeChar ls) is)
    where 
        ll = length $ ls !! 1
        xis = take (length ls) $ map (`mod` ll) [0,r..]
        yis = [0,d..length ls]
        is =  xis `zip` yis

main = do
    f <- openFile "day3" ReadMode
    cont <- hGetContents f
    let ls = lines cont
    print "part 1:"
    print $ checkSlope ls (3,1)
    print "part 2:"
    print $ product $ map (checkSlope ls) [(3,1), (1,1), (5,1), (7,1), (1,2)]