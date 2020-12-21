module Main where

import Data.Bits
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe, catMaybes, fromJust)
import qualified Data.Map.Strict as Map
import Numeric    (readInt)
import Text.Regex

type Value = Int
data Operation = Mask String | Put Int Value deriving Show

parseOperation :: String -> Maybe Operation
parseOperation line = case matchRegex (mkRegex maskRegex) line of
    Just [mask] -> return $ Mask mask
    otherwise -> case matchRegex (mkRegex putRegex) line of
        Just [address, value] -> Just $ Put (read address) (read value)
        otherwise -> Nothing
    where
        maskRegex = "mask = ([X01]{36})$"
        putRegex = "mem\\[([0-9]*)\\] = ([0-9]*)$"

type MapType = Map.Map Value Value
type State = (String, MapType)

applyOperation :: State -> Operation -> State
applyOperation (_, values) (Mask m) = (m, values)
applyOperation (x, values) (Put address value) = (x, newMap)
    where
        newMap = Map.insert address newValue values
        newValue = value .&. ones .|. zeros
        ones = fromJust $ readBin $ map (\x -> if x == 'X' then '1' else x) x
        zeros = fromJust $ readBin $ map (\x -> if x == 'X' then '0' else x) x

applyOperation2 :: State -> Operation -> State
applyOperation2 (_, values) (Mask m) = (m, values)
applyOperation2 (x, values) (Put address value) = (x, newMap)
    where
        newMap = foldl (updateAddresses value) values (getMasks (initialMask initialAddress x))
        initialAddress = pack 36 $ toBin address

updateAddresses :: Int -> MapType -> String -> MapType
updateAddresses x dict address = Map.insert ((fromJust . readBin) address) x dict

initialMask :: String -> String -> String
initialMask [] [] = []
initialMask (x:xs) ('0':ys) = x : initialMask xs ys
initialMask (_:xs) (y:ys)   = y : initialMask xs ys

getMasks :: String -> [String]
getMasks [] = [""]
getMasks ('X':xs) = map ('0':) subsMasks ++ map('1':) subsMasks
    where subsMasks = getMasks xs
getMasks (x:xs) = map (x:) (getMasks xs)

initState = ("", Map.empty) :: State

pack :: Int -> String -> String
pack n s = pack' (n - length s) s
    where
        pack' 0 s = s
        pack' n s = '0' : pack' (n-1) s

toBin :: Int -> String
toBin 0 = ['0']
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ ['1']
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ ['0']

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
        operations = catMaybes $ map parseOperation ls :: [Operation]
        finalState = foldl applyOperation2 initState operations
        sumValues acc x = acc + x
        accValue = Map.foldl sumValues 0 $ snd finalState

    print accValue
