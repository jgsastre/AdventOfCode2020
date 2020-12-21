module Main where

import Data.Maybe (catMaybes)
import Text.Regex

type Orientation = Int
type Position = (Int, Int)
type State = (Orientation, Position)

data Movement = North Int | South Int |
                East Int | West Int |
                TurnLeft Int | TurnRight Int |
                Forward Int deriving Show

mkMovement :: String -> String -> Maybe Movement
mkMovement "N" x = Just $ North $ read x
mkMovement "S" x = Just $ South $ read x
mkMovement "E" x = Just $ East $ read x
mkMovement "W" x = Just $ West $ read x
mkMovement "L" x = Just $ TurnLeft $ read x
mkMovement "R" x = Just $ TurnRight $ read x
mkMovement "F" x = Just $ Forward $ read x
mkMovement _ _ = Nothing

parseMovement :: String -> Maybe Movement
parseMovement token = matchRegex (mkRegex "^([NSEWLRF])([0-9]*)$") token >>=
    (\x -> let [operation, operand] = x in mkMovement operation operand)

normalize :: Int -> Int
normalize x
    | x < 0 = normalize $ x + 360
    | x >= 360 = normalize $ x - 360
    | otherwise = x

applyMovement :: State -> Movement -> State
applyMovement (o, (x, y)) (North z) = (o, (x, y - z))
applyMovement (o, (x, y)) (South z) = (o, (x, y + z))
applyMovement (o, (x, y)) (East z) = (o, (x + z, y))
applyMovement (o, (x, y)) (West z) = (o, (x - z, y))
applyMovement state (TurnLeft z) = applyMovement state (TurnRight (-z))
applyMovement (o, (x, y)) (TurnRight z) = (normalize (o + z), (x, y))
applyMovement (0, (x, y)) (Forward z) = (0, (x, y - z))
applyMovement (90, (x, y)) (Forward z) = (90, (x + z, y))
applyMovement (180, (x, y)) (Forward z) = (180, (x, y + z))
applyMovement (270, (x, y)) (Forward z) = (270, (x - z, y))


type State2 = (Position, Position)
applyMovement2 :: State2 -> Movement -> State2
applyMovement2 ((x, y), pos) (North z) = ((x, y - z), pos)
applyMovement2 ((x, y), pos) (South z) = ((x, y + z), pos)
applyMovement2 ((x, y), pos) (East z) = ((x + z, y), pos)
applyMovement2 ((x, y), pos) (West z) = ((x - z, y), pos)
applyMovement2 (pos, (x, y)) (Forward z) = let
    (x', y') = pos in (pos, (z * x' + x, z * y' + y))
applyMovement2 state (TurnLeft z) = applyMovement2 state (TurnRight (-z))
applyMovement2 (pos', pos) (TurnRight z) = (moveWaypoint pos' g, pos)
    where
        g = normalize z
        moveWaypoint :: Position -> Int -> Position
        moveWaypoint p 0 = p
        moveWaypoint p 90 = let
            (x, y) = p
            rotate x y
                | x <= 0 && y <= 0 = (-y, x)
                | x <= 0 && y >= 0 = (-y, x)
                | x >= 0 && y <= 0 = (-y, x)
                | x >= 0 && y >= 0 = (-y, x)
            in rotate x y
        moveWaypoint p g = moveWaypoint (moveWaypoint p 90) (g - 90)

manhattanDistance :: State -> State -> Int
manhattanDistance (_, (x, y)) (_, (x', y')) = abs (x - x') + abs (y - y')

manhattanDistance2 :: State2 -> State2 -> Int
manhattanDistance2 (_, (x, y)) (_, (x', y')) = abs (x - x') + abs (y - y')

originalState = (90, (0, 0)) :: State
originalState2 = ((10, -1), (0, 0)) :: State2

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
        movements = catMaybes $ map parseMovement ls
        finalState = foldl applyMovement2 originalState2 movements
        distance = manhattanDistance2 originalState2 finalState

    print distance
