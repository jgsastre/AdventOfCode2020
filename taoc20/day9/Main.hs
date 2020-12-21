module Main where

import Data.List

type Stream = [Int]

preambleSize = 25

lastValidNumber :: Stream -> Int
lastValidNumber xs = validate (take preambleSize xs) (drop preambleSize xs)
    where
        validate :: Stream -> Stream -> Int
        validate xs (y:ys) | elem y (pairs xs) = validate (tail xs ++ [y]) ys
                           | otherwise         = y

pairs :: Stream -> Stream
pairs [] = []
pairs (x:xs) = map (x +) xs ++ pairs xs


subsequence :: Stream -> [Stream]
subsequence []  = []
subsequence [x] = []
subsequence (x:xs) = xSequence ++ subsequence xs
    where xSequence = map (\y -> x : take y xs) [1..length xs]

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let numbers = map read ls
    let lastNumber = lastValidNumber numbers
    let xs = subsequence numbers
    let subStream = find (\x -> sum x == lastNumber) xs

    print subStream
    print $ (\x -> maximum x + minimum x) <$> subStream
