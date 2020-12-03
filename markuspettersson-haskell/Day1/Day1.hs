module Day1 where

import Data.List
import Control.Arrow

type Input = [Int]
target = 2020

combinations :: (Ord a) => [a] -> [a] -> [(a, a)]
combinations as bs = [(a, b) | a <- as, b <- bs, a >= b]

sumPair :: Num a => (a, a) -> a
sumPair = uncurry (+)

sumTriple :: Num a => (a, a, a) -> a
sumTriple (x,y,z) = x + y + z

mulPair :: Num a => (a, a) -> a
mulPair = uncurry (*)

mulTriple :: Num a => (a, a, a) -> a
mulTriple (x,y,z) = x * y * z

candidates :: [(Int, Int)] -> [(Int, Int)]
candidates = dropWhile ((>target) . sumPair)

correctNumber :: Int -> Bool
correctNumber = (== target)

candidatePairs :: [Int] -> [(Int, Int)]
candidatePairs input = candidates $ combinations input input

candidateTriples :: [Int] -> [(Int, Int, Int)]
candidateTriples input = [(x, y, z) | (x, y) <- candidatePairs input, z <- input, x + y + z <= target]

part1 :: Input -> Maybe (Int, Int)
part1 = find (correctNumber . sumPair) . candidatePairs . sort

part2 :: Input -> Maybe (Int, Int, Int)
part2 = find (correctNumber . sumTriple) . candidateTriples . sort

main :: IO ()
main = do
  input <- getNumbers <$> readFile "input"
  answer1 <- solve1 input
  answer2 <- solve2 input
  print $ mulPair answer1
  print $ mulTriple answer2
  return ()
  where
    getNumbers :: String -> [Int]
    getNumbers = map read . words
    try :: String -> Maybe a -> IO a
    try msg = maybe (ioError $ userError msg) return
    solve1 = part1 >>> try "Could not complete part 1"
    solve2 = part2 >>> try "Could not complete part 2"
