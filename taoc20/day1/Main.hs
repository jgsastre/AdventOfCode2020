module Main where

targetValue = 2020 :: Int

breadthFirst :: [Int] -> [Int]
breadthFirst (x:xs) = searchValue x xs ++ breadthFirst xs
breadthFirst [] = []

searchValue :: Int -> [Int] -> [Int]
searchValue c (x:xs) | acc == targetValue = [c, x]
                     | otherwise = searchValue c xs
                     where acc = c + x
searchValue _ [] = []

main :: IO ()
main = do
    raw <- readFile "input.txt"
    -- since lines does not take an IO <something> but a regular String
    -- we do not use the do ... <- syntax here, but put it in a variable
    -- directly.
    let ls = lines raw

    -- we want to have an [Int], so map the read function over the lines.
    let nums = map read ls :: [Int]

    let numbers = breadthFirst nums
    print numbers
    print $ foldl (*) 1 numbers
