import Text.ParserCombinators.ReadP
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Char

readInt :: String -> Int
readInt = read

parseNumber :: ReadP String
parseNumber = many1 $ satisfy isNumber

newtype Line = Line (Int, Int, Char, String) deriving Show

parseLine :: ReadP Line
parseLine = do
    from <- parseNumber
    char '-'
    to <- parseNumber
    skipSpaces
    c <- get
    char ':'
    skipSpaces
    s <- many $ satisfy isAlpha
    return $ Line (read from, read to, c, s)

getLine2 ss = fst $ last $ readP_to_S parseLine ss

isValid1 :: Line -> Bool
isValid1 (Line (f, t, c, ss)) = 
        ac >= f && ac <= t
    where 
        ac = length (filter (==c) ss); 

isValid2 :: Line -> Bool
isValid2 (Line (f, t, c, ss)) = 
    (ss !! (f-1) == c) /= (ss !! (t-1) == c)

main = do
    f <- openFile "day2" ReadMode
    cont <- hGetContents f
    let ls = map getLine2 $ lines cont
    print "part 1:"
    print $ length $ filter id (map isValid1 ls)
    print "part 2:"
    print $ length $ filter id (map isValid2 ls)
