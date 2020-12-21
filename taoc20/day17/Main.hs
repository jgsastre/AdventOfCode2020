module Main where

import qualified Data.Map.Strict as Map
import Data.List

type Position = (Int, Int, Int, Int)
type State = [Position]

parseFile :: [String] -> State
parseFile lines = map (\(x,y,_) -> (x,y,0,0)) enabledPositions
    where
        enabledPositions = filter (\(_,_,c) -> c == '#') indexAndValues
        indexAndValues = concat $ map transformLine (zip [0..length lines - 1] lines)
        transformLine (y, line) = let
                processLine x = (x, y, head (drop x line))
                indexes = [0..length line - 1]
            in map processLine indexes

calcNeighbors :: Position -> [Position]
calcNeighbors pos = let
        (x, y, z, w) = pos
        idx = [-1..1]
     in delete pos [(x + i, y + j, z + k, w + l) | i <- idx, j <- idx, k <- idx, l <- idx]

isActive :: State -> Position -> Bool
isActive state pos = elem pos state

evolveActive :: State -> Position -> Bool
evolveActive state pos = let
        continuesActive = activeNeighbors >= 2 && activeNeighbors <= 3
        getsActive = activeNeighbors == 3
        activeNeighbors = length $ filter (isActive state) (calcNeighbors pos)
    in case isActive state pos of
        True -> continuesActive
        False -> getsActive

evolve :: State -> State
evolve state = nub $ go [] state
    where
        go :: State -> State -> State
        go oldState [] = oldState
        go oldState (x:xs) = go (oldState ++ newState) xs
            where
                newState = filter (evolveActive state)  (x : calcNeighbors x)

loop :: Int -> State -> State
loop 0 state = state
loop n state = loop (n - 1) (evolve state)

{-
minCoords :: State -> Position
minCoords state = extremeCoords state min

maxCoords :: State -> Position
maxCoords state = extremeCoords state max

extremeCoords :: State -> (Int -> Int -> Int) -> Position
extremeCoords state f = go (head state) state
    where
        go coords [] = coords
        go (x, y, z) (p:ps) = let
            (x', y', z') = p in go (f x x', f y y', f z z') ps

printState :: State -> String
printState state = preamble ++ foldl (\acc x -> acc ++ printZ x) [] [minZ..maxZ]
    where
        preamble = "(x = " ++ show minX ++ ", y = " ++ show minY ++ ")\n"
        (minX, minY, minZ) = minCoords state
        (maxX, maxY, maxZ) = maxCoords state
        printZ z = "z = " ++ show z ++ "\n" ++ gridXY
            where
                gridXY = concat $ map printXs [minY..maxY]
                printXs y = map (printPos y) [minX..maxX] ++ "\n"
                printPos y x = case isActive state (x, y, z) of
                    True -> '#'
                    False -> '.'
-}

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
        initState = parseFile ls
        finalState = loop 6 initState
        activeBoxes = length finalState

    print activeBoxes
