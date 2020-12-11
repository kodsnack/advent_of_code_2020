import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List (sort)

data BoardingPass = BoardingPass Int Int deriving Show

parseInt1 :: Char -> String -> Int -- one being the char representing a one in the binary number. All others will be treated as 0
parseInt1 one ss = parseInt2 one ss 0
    where
    parseInt2 :: Char -> String -> Int -> Int
    parseInt2 _ [] a = a
    parseInt2 one (s:ss) a 
        | s == one = parseInt2 one ss (a * 2 + 1)
        | otherwise = parseInt2 one ss (a * 2)


readBP :: String -> BoardingPass
readBP ss = BoardingPass (parseInt1 'B' (take l ss)) (parseInt1 'R' (drop l ss))
    where l = 7 -- length of first part of boarding pass


seatID :: BoardingPass -> Int
seatID (BoardingPass r c) = r * 8 + c


findMissing :: [Int] -> Int
findMissing (n:[]) = -1
findMissing (n:m:ns) 
    | m - n == 1 = findMissing (m:ns)
    | otherwise = n + 1


main = do
    f <- openFile "day5" ReadMode
    cont <- hGetContents f
    let ls = lines cont
    let bps = map readBP ls
    let ids = map seatID bps
    print "part 1:"
    print $ maximum ids
    print "part2:"
    print $ findMissing $ sort ids