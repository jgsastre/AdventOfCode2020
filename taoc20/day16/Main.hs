module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Regex

type Rule = (String, [(Int, Int)])

parseRule :: String -> Maybe Rule
parseRule line = matchRegex (mkRegex "^([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)$") line >>=
    (\x -> case x of (name : rules) -> Just (name, mkRules rules))
        where
            mkRules :: [String] -> [(Int, Int)]
            mkRules [] = []
            mkRules (x1:(x2:xs)) = (read x1, read x2) : mkRules xs

checkRule :: Rule -> Int -> Bool
checkRule rule field = let
        (_, st) = rule
        checkStatement (x1, x2) = field >= x1 && field <= x2
    in elem True $ map checkStatement st

type Ticket = [Int]

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","


reader :: (String -> a) -> [String] -> ([a], [String])
reader f xs = go xs []
    where
        go [] ys = (ys, [])
        go ("":xs) ys = (ys, xs)
        go (x:xs) ys = go xs ((f x):ys)



parseFile :: [String] -> ([Rule], Ticket, [Ticket])
parseFile lines = let
        (rules, l1) = reader parseRule lines
        (ownTickets, l2) = reader parseTicket l1
        (restTickets, _) = reader parseTicket l2
    in (catMaybes rules, head ownTickets, restTickets)


invalidFields :: [Rule] -> Ticket -> [Int]
invalidFields rules ticket = filter (not . existValidRule) ticket
    where
        existValidRule :: Int -> Bool
        existValidRule field = elem True $ map (\x -> checkRule x field) rules


rulesOrder :: [Ticket] -> [Rule] -> [[Rule]]
rulesOrder tickets rules = go allFields rules
    where
        allFields = getFields tickets
        go :: [[Int]] -> [Rule] -> [[Rule]]
        go [] _ = [[]]
        go _ [] = [[]]
        go (f:fs) rules = concat $ map generateRules validRules
            where
                validRules = filter (allFieldsValid f) rules
                generateRules :: Rule -> [[Rule]]
                generateRules rule = map (rule:) nextRules
                    where
                        nextRules = filterRules $ go fs remainingRules
                        remainingRules = otherRules rule rules
                        filterRules = filter ( (>= length remainingRules) . length)


otherRules :: Rule -> [Rule] -> [Rule]
otherRules = deleteBy (\(n1,_) (n2,_) -> n1 == n2)

getFields :: [Ticket] -> [[Int]]
getFields tickets = map (\x -> map (head . drop x) tickets) [0 .. length (head tickets) - 1]

miFactorial :: Int -> Int
miFactorial 0 = 1
miFactorial x = x * miFactorial (x - 1)

allFieldsValid :: [Int] -> Rule -> Bool
allFieldsValid fields rule = product (map (fromEnum . checkRule rule) fields) == 1

rulesValidPerField :: [[Int]] -> [Rule] -> [(Int, [Rule])]
rulesValidPerField fields rules = map (\x -> (length x, x)) rulesValids
    where rulesValids = map (\x -> filter (allFieldsValid x) rules) fields

fieldsValid :: [[Int]] -> Rule -> Int
fieldsValid fields rule = sum $ map (\x -> fromEnum(allFieldsValid x rule)) fields

validRules :: [[Int]] -> [Rule] -> [Int]
validRules fields rules = map (fieldsValid fields ) rules

processRules :: [(Int, [Rule])] -> [(Int, Rule)]
processRules rules = go [] 1
    where
        maxN = length rules
        go :: [Rule] -> Int -> [(Int, Rule)]
        go prevList i | i <= maxN = item' : go item (i + 1)
                      | otherwise = []
                            where
                                index = fromJust $ findIndex (\(j,_) -> j == i) rules
                                item = snd $ head $ drop index rules
                                item' = (index, head (item \\ prevList))


getSolution :: [(Int, Rule)] -> Ticket -> Int
getSolution rules ticket = product $ values
    where
        fields = filter (isInfixOf "departure" . fst . snd) rules
        values = map (\(i,_) -> head (drop i ticket)) fields

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
        (rules, ownTicket, otherTickets) = parseFile ls
        fields = foldl (++) [] $ map (invalidFields rules) otherTickets :: [Int]
        validTickets = filter (\x -> length (invalidFields rules x) == 0) otherTickets
        allFields = getFields validTickets
        orderedRules = rulesOrder validTickets rules
        rulesPerField = rulesValidPerField allFields rules
        fieldRules = processRules rulesPerField
        solution = getSolution fieldRules ownTicket
        {-
        checkValue = product $ map (\ticket -> product (map (\field -> product (map (\rule -> fromEnum (checkRule rule field)) rules)) ticket)) validTickets
        rulesValues = zip orderedRules ownTicket
        departuredRules = filter (isInfixOf "departure" . fst . fst) rulesValues
        prodResult = product (map snd departuredRules)
        -}
    print $ sum fields
    print $ solution
    print orderedRules
    -- print departuredRules
    -- print $ map snd departuredRules
    -- print checkValue
    -- print prodResult
