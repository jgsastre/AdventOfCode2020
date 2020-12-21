module Main where

import Data.List.Split
import qualified Data.Map.Strict as Map

parseNumbers :: String -> [Int]
parseNumbers line = reverse $ map read $ splitOn "," line

nextNumber :: [Int] -> Int
nextNumber (x:xs) = nextNumber' x 1 xs
    where
        nextNumber' :: Int -> Int -> [Int] -> Int
        nextNumber' _ _ [] = 0
        nextNumber' x acc (y:ys)
            | x == y = acc
            | otherwise = nextNumber' x (acc + 1) ys

generateSequence :: [Int] -> Int -> [Int]
generateSequence xs 0 = xs
generateSequence xs x = generateSequence (nextNumber xs : xs) (x - 1)

initialNumbers = "0,6,1,7,2,19,20"

type MapType = Map.Map Int Int

initNumbers :: [Int] -> MapType
initNumbers [] = Map.fromList []
initNumbers (x:xs) = Map.insert x (length xs + 1) (initNumbers xs)

generateSequence2 :: MapType -> Int -> Int -> Int -> Int
generateSequence2 map x maxCount n
    | n == maxCount = x
    | otherwise = maxCount `seq` newN `seq` newMap `seq` generateSequence2 newMap newValue maxCount newN
    where
        newValue = case Map.lookup x map of
            Just y -> n - y
            Nothing -> 0
        newMap = Map.insert x n map
        newN = n + 1

main :: IO ()
main = do
    let numbers = parseNumbers initialNumbers
        initCount = length numbers
        map = initNumbers numbers
        -- lastNumber = foldl (\x -> \y -> x + y) 0 [1..30000000]
        lastNumber = generateSequence2 map (head numbers) 30000000 initCount

    print lastNumber
