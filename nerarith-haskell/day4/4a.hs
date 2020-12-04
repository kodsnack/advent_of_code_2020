import Data.List.Split (splitOn)
import qualified Data.Map as Map

main = interact (output . map (isValid . readInput) . splitOn [""] . lines)

readInput :: [String] -> Map.Map String String
readInput = Map.fromList . map splitPair . concatMap words

splitPair :: String -> (String, String)
splitPair s = (key, value)
    where (key:value:_) = splitOn ":" s

isValid :: Map.Map String String -> Bool
isValid d = and $ map (flip Map.member d) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

output :: [Bool] -> String
output = (++"\n") . show . length . filter (True==)
