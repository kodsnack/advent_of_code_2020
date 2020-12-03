main = interact ((++"\n") . show . solve . lines)

solve :: [String] -> Int
solve mp = product $ (map numTrees [(1,1), (1,3), (1,5), (1,7), (2,1)]) <*> pure mp

numTrees :: (Int, Int) -> [String] -> Int
numTrees (dh, dw) mp = length . filter (=='#') . map mapLookup $ positions
    where height = length mp
          width = length (mp !! 0)
          positions = zip [0,dh..(height-1)] (map (`mod` width) [0,dw..])
          mapLookup (h, w) = (mp !! h) !! w
