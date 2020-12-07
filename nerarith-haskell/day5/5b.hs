import Data.List

main = interact ((++"\n") . show . missingId . lines)

missingId :: [String] -> [Int]
missingId seats = [minId..maxId] \\ ids
    where maxId = maximum ids
          minId = minimum ids
          ids = map seatId seats

seatId :: String -> Int
seatId seat = foldl (\s x -> 2*s + (if x=='B' || x=='R' then 1 else 0)) 0 seat
