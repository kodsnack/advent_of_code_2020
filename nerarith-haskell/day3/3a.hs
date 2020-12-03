main = interact ((++"\n") . show . solve . lines)

numTrees :: [String] -> Int
numTrees mp = length . filter (=='#') . map mapLookup $ positions
    where height = length mp
          width = length (mp !! 0)
          positions = zip [0..(height-1)] (map (`mod` width) [0,3..])
          mapLookup (h, w) = (mp !! h) !! w
