main = interact(writeOutput . solve . readInput)

readInput :: String -> [Int]
readInput = map read . words

solve :: [Int] -> [Int]
solve numbers = [x | Just x <- products]
    where products = [productIfLegal a b | a <- numbers, b <- numbers, a <= b]

productIfLegal :: Int -> Int -> Maybe Int
productIfLegal a b
    | a + b == 2020 = Just (a*b)
    | otherwise = Nothing

writeOutput :: [Int] -> String
writeOutput = unlines . map show
