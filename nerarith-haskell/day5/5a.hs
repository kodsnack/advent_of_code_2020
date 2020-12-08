main = interact ((++"\n") . show . maximum . map seatId . lines)

seatId :: String -> Int
seatId seat = foldl (\s x -> 2*s + (if x=='B' || x=='R' then 1 else 0)) 0 seat
