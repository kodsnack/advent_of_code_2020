import Data.List (sort)

main = interact ((++"\n") . show . solution . (\xs -> 0 : xs ++ [(maximum xs) + 3]) . map read . lines)

solution :: [Int] -> Int
solution = onesTimesThrees . diffList . sort
    where diffList xs = zipWith (-) (tail xs) xs
          onesTimesThrees xs = (length $ filter (1==) xs) * (length $ filter (3==) xs)
