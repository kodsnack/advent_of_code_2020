preambleLength = 25

main = interact ((++"\n") . show . solution . map read . lines)

solution :: [Int] -> Int
solution input = firstInvalid (take preambleLength input) (drop preambleLength input)

firstInvalid :: [Int] -> [Int] -> Int
firstInvalid (x:xs) (y:ys)
    | y `elem` ((+) <$> (x:xs) <*> (x:xs)) = firstInvalid (xs ++ [y]) ys
    | otherwise     = y
