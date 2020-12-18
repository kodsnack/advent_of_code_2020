import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

data Seat = Floor | Empty | Occupied deriving (Eq, Show, Ord)
type Pos = (Int, Int)
type Room = Map.Map Pos Seat
type CountingMethod = Room -> Pos -> Int

seat :: Char -> Seat
seat '.' = Floor
seat 'L' = Empty
seat '#' = Occupied

readRoom :: [String] -> Room
readRoom = Map.fromList . map (\(p, s) -> (p, seat s)) . concat . indexXY

indexXY :: [[b]] -> [[(Pos, b)]]
indexXY ss = [[((x,y), s) | (x, s) <- zip [0..] r] | (y, r) <- zip [0..] ss]


main = interact ((++"\n") . show . solution . readRoom . lines)

solution :: Room -> Int
solution = (countOccupied . Map.elems) . stable (nextRoom countAdjacent 4)

nextRoom :: CountingMethod -> Int -> Room -> Room
nextRoom count limit room = Map.fromList [(p, s') | (p, s) <- Map.assocs room, let s' = rule limit s (count room p)]

rule :: Int -> Seat -> Int -> Seat
rule _ Empty 0 = Occupied
rule limit Occupied n
    | n >= limit = Empty
rule _ s _ = s

countAdjacent :: CountingMethod
countAdjacent room pos = countOccupied . mapMaybe ((flip Map.lookup) room . changePos pos) $ directions
    where changePos (x, y) (x', y') = (x + x', y + y')

countOccupied :: [Seat] -> Int
countOccupied = length . filter (== Occupied)

directions :: [Pos]
directions = [(-1, -1), (0, -1), (1, -1),
              (-1, 0),           (1, 0),
              (-1, 1),  (0, 1),  (1, 1)]

stable :: (Eq a) => (a -> a) -> a -> a
stable f x
    | x == x'    = x
    | otherwise  = stable f x'
    where x' = f x
