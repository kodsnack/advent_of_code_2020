import Data.List (sort, group)

-- This solution works because the only differences between consecutive adapters in the input are 1 and 3 (2 never appears).

main = interact ((++"\n") . show . solution . (\xs -> 0 : xs ++ [(maximum xs) + 3]) . map read . lines)

solution = product . map (numCombinations . length) . filter (\(x:_) -> x == 1) . group . diffList . sort
    where diffList xs = zipWith (-) (tail xs) xs
          numCombinations n = tribonacci !! (n+1)

tribonacci :: [Int]
tribonacci = 0 : 1 : 1 : zipWith3 sum tribonacci (tail tribonacci) (drop 2 tribonacci)
    where sum x y z = x+y+z
