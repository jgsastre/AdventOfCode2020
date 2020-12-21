module Main where

import Data.List
import Data.Maybe (catMaybes)
import Text.Regex

data Boardingpass = Boardingpass String String deriving Show

mkBoardingpass :: String -> Maybe Boardingpass
mkBoardingpass line = matchRegex (mkRegex "^([FB]{7})([LR]{3})$") line >>=
    (\x -> case x of
        [rows, cols] -> return (Boardingpass rows cols)
        otherwise -> Nothing
    )

data PlaneSeat = PlaneSeat Int Int deriving Show

planeSeat :: Boardingpass -> PlaneSeat
planeSeat (Boardingpass row col) = PlaneSeat (calcRow (reverse row)) (calcCol (reverse col))
    where
        calcRow :: String -> Int
        calcRow [] = 0
        calcRow (x:xs) = 2 * calcRow xs + xValue
            where
                xValue = case x of
                    'B' -> 1
                    'F' -> 0
        calcCol :: String -> Int
        calcCol [] = 0
        calcCol (x:xs) = 2 * calcCol xs + xValue
            where
                xValue = case x of
                    'R' -> 1
                    'L' -> 0


calcId :: PlaneSeat -> Int
calcId (PlaneSeat row col) = 8 * row + col

possibleSeats = createSeats 7 3
    where
        createSeats :: Int -> Int -> [String]
        createSeats 0 0 = [""]
        createSeats 0 y = (map ("R" ++ ) newSeats) ++ (map ("L" ++ ) newSeats)
            where newSeats = createSeats 0 (y-1)
        createSeats x y = (map ("F" ++) newSeats) ++ (map ("B" ++ ) newSeats)
            where newSeats = createSeats (x-1) y

differences :: [Int] -> [Int]
differences fs = zipWith (-) fs (tail fs)

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let boardingpasses = map mkBoardingpass ls
    let ids = map (fmap (calcId . planeSeat)) boardingpasses

    let reminders = filter (\x -> not (elem x ls)) possibleSeats
    let suspectBP = map mkBoardingpass reminders
    let possibleIds = sort $ catMaybes $ map (fmap (calcId . planeSeat)) suspectBP
    let diffs = differences possibleIds

    print $ possibleIds
