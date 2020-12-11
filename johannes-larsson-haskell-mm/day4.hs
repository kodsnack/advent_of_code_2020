import Text.ParserCombinators.ReadP
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.Char

newtype Passport = Passport [(String,String)] deriving Show

splitLines :: String -> String -> [String]
splitLines r [] = [r]
splitLines r ('\n':'\n':ss) = r : splitLines "" ss
splitLines r (s:ss) = splitLines (r ++ [s]) ss

parseKV :: ReadP (String, String)
parseKV = do
    key <- count 3 $ satisfy isAlpha
    char ':'
    val <- many1 $ satisfy (\c -> not (isSpace c))
    skipSpaces
    return (key,val)

parseLine :: ReadP Passport
parseLine = do
    kvs <- many1 parseKV
    return $ Passport kvs


getLine2 ss = fst $ last $ readP_to_S parseLine ss

hasKey :: Passport -> String -> Bool
hasKey (Passport ((k, _):ss)) kk = k == kk || hasKey (Passport ss) kk
hasKey (Passport []) _ = False

isValid :: Passport -> Bool
isValid (Passport p) = length p == 8 || length p == 7 && not (hasKey (Passport p) "cid")

main = do
    f <- openFile "day4" ReadMode
    cont <- hGetContents f
    let ps = splitLines "" cont
    let pps = map getLine2 ps
    print "part 1:"
    print $ map isValid pps
    print $ length $ filter id (map isValid pps)
