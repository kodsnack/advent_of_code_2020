import Data.List.Split (splitOneOf)

main = interact ((++ "\n") . writeOutput . map (solve . interpret) . lines)

data Rule = Rule {lowerBound :: Int, upperBound :: Int, charToCount :: Char} deriving (Show)
type Password = String

interpret :: String -> (Rule, Password)
interpret input = (Rule (read lowerBoundString) (read upperBoundString) charToCount, password)
    where (lowerBoundString:upperBoundString:(charToCount:_):_:password:_) = splitOneOf " -:" input

solve :: (Rule, Password) -> Bool
solve (rule, password) = (lowerBound rule <= numOccurrences) && (numOccurrences <= upperBound rule)
    where numOccurrences = length . filter ((charToCount rule == )) $ password

writeOutput :: [Bool] -> String
writeOutput = show . length . filter (True==)
