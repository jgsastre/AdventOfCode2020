module Main where

import Data.Maybe (catMaybes)

data Board = Board [String]

data Point = Point Int Int
data Direction = Direction Int Int

newPoint :: Board -> Point -> Direction -> Point
newPoint (Board lines) (Point x y) (Direction dx dy) = Point newX $ y + dy
    where newX = mod (x + dx) $ length $ head lines

downhill :: Board -> Point -> Direction -> Int
downhill (Board []) _ _ = 0
downhill board point direction
    | endOfBoard board point = 0
    | isTree board point = 1 + restDownhill
    | otherwise = restDownhill
    where
        endOfBoard :: Board -> Point -> Bool
        endOfBoard (Board lines) (Point x y) = y >= length lines
        isTree :: Board -> Point -> Bool
        isTree (Board lines) (Point x y) = 
            '#' == (head $ drop x $ head $ drop y lines)
        point' = newPoint board point direction 
        restDownhill = downhill board point' direction

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let board = Board ls
    let directions = [ Direction 1 1, Direction 3 1, Direction 5 1, Direction 7 1, Direction 1 2 ]
    let trees = map (downhill board (Point 0 0)) directions
    print $ foldl (*) 1 trees


