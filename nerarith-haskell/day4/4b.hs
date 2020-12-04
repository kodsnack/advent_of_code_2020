import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Char (isDigit, isHexDigit)

main = interact (output . map (isValid . readInput) . splitOn [""] . lines)

readInput :: [String] -> Map.Map String String
readInput = Map.fromList . map splitPair . concatMap words

splitPair :: String -> (String, String)
splitPair s = (key, value)
    where (key:value:_) = splitOn ":" s

isValid :: Map.Map String String -> Bool
isValid d = and $ map check [(byr, "byr"), (iyr, "iyr"), (eyr, "eyr"), (hgt, "hgt"), (hcl, "hcl"), (ecl, "ecl"), (pid, "pid")]
    where check (f, key) = (Just True ==) . fmap f $ Map.lookup key d

byr = and . sequenceA [(>=1920), (<=2002)] . read
iyr = and . sequenceA [(>=2010), (<=2020)] . read
eyr = and . sequenceA [(>=2020), (<=2030)] . read
hgt = isValidLength . break (not . isDigit)
    where isValidLength (lenStr, unit)
            | null lenStr = False
            | unit == "cm" && 150 <= len && len <= 193 = True
            | unit == "in" && 59 <= len && len <= 76 = True
            | otherwise = False
            where len = read lenStr
hcl (h:number) = (h == '#') && (and $ map isHexDigit number)
ecl colour = elem colour ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
pid = and . sequenceA [((9 ==) . length), (and . map isDigit)]

output :: [Bool] -> String
output = (++"\n") . show . length . filter (True==)
