import Data.List (intersect)
import Data.List.Split (splitOn)

main = interact ((++"\n") . show . sum . map solve . readInput)

readInput :: String -> [[String]]
readInput = splitOn [""] . lines

solve :: [String] -> Int
solve = length . foldr1 intersect
