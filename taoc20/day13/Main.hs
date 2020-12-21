module Main where

import Data.List.Split

type Forecast = (Int, [Int])

parseLines :: [String] -> [Forecast]
parseLines [] = []
parseLines (x1:(x2:xs)) = (read x1, parse x2) : parseLines xs
    where
        parse :: String -> [Int]
        parse = map read . filter ("x" /=) . splitOn ","

firstBus :: Forecast -> (Int, Int)
firstBus (time, buses)
    | oneBusDeparts = (time, head busesDeparting)
    | otherwise = firstBus ((time + 1), buses)
    where
        oneBusDeparts = length busesDeparting > 0
        busesDeparting = filter ((0 ==) . (time `rem`)) buses


firstTime :: String -> Int
firstTime line = testTime 0 1 (read x) xs
    where
        (x:xs) = splitOn "," line

testTime :: Int -> Int -> Int -> [String] -> Int
testTime prev _ _ [] = prev
testTime prev r m ("x":xs) = testTime prev (r + 1) m xs
testTime prev r m (x:xs) = testTime (searchRem prev) (r + 1) (m*p) xs
    where
        p = read x
        searchRem 0 = searchRem m
        searchRem x
            | (x + r) `mod` p == 0 = x
            | otherwise = searchRem (m + x)

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let
        ls = lines raw
        forecasts = parseLines ls
        (arrivingTime, _) = head forecasts
        (leavingTime, bus) = firstBus $ head forecasts
        buses = head $ drop 1 ls
        firstT = firstTime buses

    print $ bus * (leavingTime - arrivingTime)
    print $ firstT
