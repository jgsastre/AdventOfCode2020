module Main where

import Data.Sort

ways :: [Int] -> Int
ways []     = 1
ways (1:xs) = case trailOnes 1 xs of (n, ys) -> f n * ways ys
    where
        trailOnes :: Int -> [Int] -> (Int, [Int])
        trailOnes acc []     = (acc, [])
        trailOnes acc (3:ys) = (acc, ys)
        trailOnes acc (1:ys) = trailOnes (1 + acc) ys
        f :: Int -> Int
        f 1 = 1
        f 2 = 2
        f 3 = 4
        f x = f (x - 1) + f (x - 2) + f(x - 3)
ways (_:xs) = ways xs

ways' :: [Int] -> Int
ways' []  = 0
ways' [x] = if x <= 3 then 1 else 0
ways' (x:(y:(z:xs)))
    | x + y + z <= 3 = ways'(y:(z:xs)) + ways'(z:xs) + ways'(xs)
    | x + y <= 3     = ways'(y:(z:xs)) + ways'(z:xs)
    | x <= 3         = ways'(y:(z:xs))
    | otherwise      = 0
ways' (x:(y:xs))
    | x + y <= 3 = ways'(y:xs) + ways'(xs)
    | x <= 3     = ways'(y:xs)
    | otherwise  = 0

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let numbers = 0 : (map read ls) :: [Int]
    let s = sort (maximum numbers + 3: numbers)
    let diff = map (\x -> case x of (a,b) -> b - a) (zip s (tail s))
    let num3 = length $ filter (3==) diff
    let num2 = length $ filter (2==) diff
    let num1 = length $ filter (1==) diff
    let diffWays = ways diff
    print diff
    print diffWays
