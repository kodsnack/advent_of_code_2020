import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

main = interact ((++"\n") . show . solution [0] (0,0) . readInput)

type Operation = (String, Int)
type State = (Int, Int)

solution :: [Int] -> State -> [Operation] -> Int
solution visited state@(index, acc) program
    | newIndex `elem` visited   = acc
    | otherwise                 = solution (newIndex:visited) newState program
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
