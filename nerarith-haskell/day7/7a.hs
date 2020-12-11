import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import qualified Data.Map as Map

main = interact ((++"\n") . show . solution . readInput)

readInput :: String -> BagRules
readInput = Map.fromList . map (fst . last . readP_to_S bagRule) . lines


type BagRules = Map.Map String [(String, Int)]

bagRule :: ReadP (String, [(String, Int)])
bagRule = do
    colour <- fmap unwords (count 2 word)
    string "bags contain"
    contents <- many content
    return (colour, contents)

word :: ReadP String
word = do
    skipSpaces
    w <- munch (/=' ')
    skipSpaces
    return w

content :: ReadP (String, Int)
content = do
    skipSpaces
    numBags <- fmap read word
    colour <- fmap unwords (count 2 word)
    string "bag" <|> string "bags"
    string "," <|> string "."
    return (colour, numBags)

solution :: BagRules -> Int
solution rules = length . filter (==True) . map (containsGoldenBag rules) $ colours
    where colours = Map.keys rules

containsGoldenBag :: BagRules -> String -> Bool
containsGoldenBag rules colour = ("shiny gold" `elem` contentsColours) || any (containsGoldenBag rules) contentsColours
        where Just contents = Map.lookup colour rules
              contentsColours = map fst contents
