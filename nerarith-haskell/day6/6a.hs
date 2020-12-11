import Data.List (nub)
import Data.List.Split (splitOn)

main = interact ((++"\n") . show . sum . map solve . readInput)

readInput :: String -> [String]
readInput = map concat . splitOn [""] . lines

solve :: String -> Int
solve = length . nub
