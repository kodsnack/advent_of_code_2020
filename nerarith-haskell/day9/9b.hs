import Data.List (inits, tails)

preambleLength = 25

main = interact ((++"\n") . show . map computeOutput . correctSubSeq . map read . lines)

correctSubSeq :: [Int] -> [[Int]]
correctSubSeq xs = filter ((1/=) . length) . filter ((n==) . sum) . concatMap tails . inits $ xs
    where n = partA xs

computeOutput :: [Int] -> Int
computeOutput xs = (minimum xs) + (maximum xs)


-- part a

partA :: [Int] -> Int
partA input = firstInvalid (take preambleLength input) (drop preambleLength input)

firstInvalid :: [Int] -> [Int] -> Int
firstInvalid (x:xs) (y:ys)
    | y `elem` ((+) <$> (x:xs) <*> (x:xs)) = firstInvalid (xs ++ [y]) ys
    | otherwise     = y
