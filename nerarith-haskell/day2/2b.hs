import Data.List.Split (splitOneOf)

main = interact ((++ "\n") . writeOutput . map (solve . interpret) . lines)

data Rule = Rule {position1 :: Int, position2 :: Int, mustAppear :: Char} deriving (Show)
type Password = String

interpret :: String -> (Rule, Password)
interpret input = (Rule (read position1String - 1) (read position2String -1) mustAppear, password)
    where (position1String:position2String:(mustAppear:_):_:password:_) = splitOneOf " -:" input

solve :: (Rule, Password) -> Bool
solve (rule, password) = (char1 == mustAppear rule) /= (char2 == mustAppear rule)
    where char1 = password !! (position1 rule)
          char2 = password !! (position2 rule)

writeOutput :: [Bool] -> String
writeOutput = show . length . filter (True==)
