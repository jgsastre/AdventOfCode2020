module Main where

import Text.Regex
import Data.Maybe (catMaybes)

data Record = Record {
    lower :: Int,
    upper :: Int,
    char :: Char,
    password :: String
   } deriving Show


mkRecord :: [String] -> Maybe Record
mkRecord [lower, upper, char, password] = Just $ Record (read lower) (read upper) (head char) password
mkRecord _ = Nothing

parse :: String -> Maybe Record
parse line =
    mkRecord =<<
    matchRegex (mkRegex "([0-9]+)-([0-9]+) ([a-z]): ([a-z]*)$") line

policyCompliant :: Record -> Bool
policyCompliant record = count >= lowerLimit && count <= upperLimit
    where
    elements = filter (\x -> x == char record) $ password record
    count = length elements
    lowerLimit = lower record
    upperLimit = upper record

policyCompliant2 :: Record -> Bool
policyCompliant2 record = (firstChar == currentChar) /= (secondChar == currentChar)
    where
    firstChar =  password record !! (lower record - 1)
    secondChar =  password record !! (upper record - 1)
    currentChar = char record

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let records = map parse ls :: [Maybe Record]
    let validRecords = map (fmap policyCompliant2) records :: [Maybe Bool]
    let counts = length $ filter id $ catMaybes validRecords
    print counts
