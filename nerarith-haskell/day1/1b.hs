main = interact(writeOutput . solve . readInput)

readInput :: String -> [Int]
readInput = map read . words

solve :: [Int] -> [Int]
solve numbers = [x | Just x <- products]
    where products = [productIfLegal a b c | a <- numbers, b <- numbers, c <- numbers, a <= b, b <= c]

productIfLegal :: Int -> Int -> Int -> Maybe Int
productIfLegal a b c
    | a + b + c == 2020 = Just (a*b*c)
    | otherwise = Nothing

writeOutput :: [Int] -> String
writeOutput = unlines . map show
