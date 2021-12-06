module Main where

import Data.List
import Data.List.Split
import Data.Maybe (catMaybes, fromMaybe)
import Text.Regex

data Rule = Rule String [(Int, String)] deriving Show

mkRule :: String -> Maybe Rule
mkRule line = bagName >>= \x -> Just (Rule x bagsContained)
    where
        bagName = matchRegex (mkRegex "^([a-z]+ [a-z]+)") firstSentence >>= return . head
        firstSentence = head tokens
        tokens = splitOn "," line
        bagsContained = catMaybes $ map parseBags tokens
        parseBags :: String -> Maybe (Int, String)
        parseBags x = matchRegex (mkRegex containBagRegex) x >>=
            \x -> return ( (read . head) x, (head . tail) x)
        containBagRegex =  "([0-9])+ ([a-z]+ [a-z]+)"


contains :: [Rule] -> Rule -> String -> Bool
contains _ (Rule _ []) _ = False
contains rules (Rule _ bags) bag = elem bag (map snd bags) || nestedContained
    where
        nestedContained = elem True subBagsContains
        subBagsContains = map subBagContains bags
        subBagContains :: (Int, String) -> Bool
        subBagContains (_, subBag) = fromMaybe False (find predicate rules >>=
            \x -> return (contains rules x bag))
                where
                    predicate :: Rule -> Bool
                    predicate (Rule name _) = name == subBag

bagsContained :: [Rule] -> Rule -> Int
bagsContained _ (Rule _ []) = 0
bagsContained rules (Rule _ bags) = sum $ map numBags bags
    where
        numBags :: (Int, String) -> Int
        numBags (n, x) = fromMaybe 1 ((find predicate rules) >>=
            \x -> return (n + n * bagsContained rules x))
            where
                predicate :: Rule -> Bool
                predicate (Rule name _) = name == x

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let rules = catMaybes $ map mkRule ls
    let goldBags = filter (\x -> contains rules x "shiny gold") rules

    let goldBagRule = find (\x -> case x of Rule name _ -> name == "shiny gold") rules
    let goldContainsBags = goldBagRule >>= return . bagsContained rules

    print $ goldContainsBags
