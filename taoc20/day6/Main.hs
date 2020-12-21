module Main where

import Data.List

data Response = Response String
data GroupResponse = GroupResponse [Response]

process :: [String] -> [GroupResponse]
process [] = []
process lines = (GroupResponse responses : process remainLines)
    where 
        remainLines = fst thisChunk
        responses = snd thisChunk
        thisChunk = parseChunk lines [] 
        parseChunk :: [String] -> [Response] -> ([String], [Response])
        parseChunk [] responses = ([], responses)
        parseChunk (x:xs) responses
            | x == "" = (xs, responses)
            | otherwise = parseChunk xs (Response x : responses)

numGroupRespones :: GroupResponse -> Int
numGroupRespones (GroupResponse groupResponses) = numResponses
    where
        numResponses = sum $ map fromEnum allPositiveResponses
        allPositiveResponses = map (\x -> elem x allResponses) questions
        allResponses = foldl (++) "" responses
        responses = [y | Response y <- groupResponses]
        questions = ['a'..'z']

numGroupEveryoneResponses:: GroupResponse -> Int
numGroupEveryoneResponses (GroupResponse groupResponses) = numResponses
    where
        numResponses = sum $ map fromEnum allPositiveResponses
        allPositiveResponses = map (\x -> elem x everyoneYesQuestion) questions
        everyoneYesQuestion = foldl (intersect) (head responses) responses
        responses = [y | Response y <- groupResponses]
        questions = ['a'..'z']

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let groupsReponses = process ls
    let yesQuestions = sum $ map numGroupEveryoneResponses groupsReponses
    print yesQuestions
