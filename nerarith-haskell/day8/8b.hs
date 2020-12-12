import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

main = interact ((++"\n") . show . solution . readInput)

type Operation = (String, Int)
type State = (Int, Int)

solution :: [Operation] -> Int
solution program = result
    where accs = map (accAtEnd [0] (0, 0) . changedProgram program) [1..((length program)-1)]
          Just result = head . filter (/=Nothing) $ accs
    
changedProgram :: [Operation] -> Int -> [Operation]
changedProgram program index
    | op == "nop"   = x ++ ("jmp", num) : y
    | op == "jmp"   = x ++ ("nop", num) : y
    | op == "acc"   = program
        where (x, (op, num):y) = splitAt index program

accAtEnd :: [Int] -> State -> [Operation] -> Maybe Int
accAtEnd visited state@(index, acc) program
    | index == length program   = Just acc
    | newIndex `elem` visited   = Nothing
    | otherwise                 = accAtEnd (newIndex:visited) newState program
        where newState@(newIndex, newAcc) = nextOperation program state

nextOperation :: [Operation] -> (Int, Int) -> (Int, Int)
nextOperation program (index, acc)
    | op == "nop"   = (index+1, acc)
    | op == "acc"   = (index+1, acc+num)
    | op == "jmp"   = (index+num, acc)
        where (op, num) = program !! index

readInput = map (fst . last . readP_to_S operation) . lines


operation :: ReadP Operation
operation = do
    opType <- word
    num <- signedNumber
    return (opType, num)

signedNumber :: ReadP Int
signedNumber = do
    sign <- string "+" <|> string "-"
    number <- fmap read word
    return (if sign == "+" then number else -number)

word :: ReadP String
word = do
    skipSpaces
    w <- munch (/=' ')
    skipSpaces
    return w
