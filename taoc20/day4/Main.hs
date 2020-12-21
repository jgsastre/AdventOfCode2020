module Main where

import Text.Regex
import Data.List.Split
import Data.Maybe (catMaybes)

data Token = Token String String deriving Show

parseToken :: String -> Maybe Token
parseToken token = mkToken =<< matchRegex (mkRegex "([a-z]+):([#a-z0-9]+)$") token
    where
        mkToken :: [String] -> Maybe Token
        mkToken [key, value] = Just $ Token key value
        mkToken _ = Nothing

data Passport = Passport [Token] deriving Show

mkPassport :: [Token] -> Maybe Passport
mkPassport tokens = case validPassport of
    True -> Just (Passport tokens)
    False -> Nothing
    where
        neededTokens = filter (\x -> x /= "cid") validTokens
        validPassport =
            length (filter (\x -> x /= "cid") keys) == length neededTokens &&
            product (map fromEnum containsNeedKey) == 1 &&
            product (map (fromEnum . validToken) tokens) == 1
        containsNeedKey = map (\x -> elem x keys) neededTokens
        keys = [x | Token x _ <- tokens]
        validToken :: Token -> Bool
        validToken (Token key value) = case key of
            "byr" -> length value == 4 && byr >= 1920 && byr <= 2002
                where byr = read value
            "iyr" -> length value == 4 && iyr >= 2010 && iyr <= 2020
                where iyr = read value
            "eyr" -> length value == 4 && eyr >= 2020 && eyr <= 2030
                where eyr = read value
            "hgt" -> case matchRegex (mkRegex "([0-9]*)(cm|in)") value of
                Just [h, "cm"] -> heigth >= 150 && heigth <= 193
                    where heigth = read h
                Just [h, "in"] -> heigth >= 59 && heigth <= 76
                    where heigth = read h
                _ -> False
            "hcl" -> case matchRegex (mkRegex "^#([0-9a-f]{6})$") value of
                Just _ -> True
                _ -> False
            "ecl" -> case matchRegex (mkRegex "^(amb|blu|brn|gry|grn|hzl|oth)$") value of
                Just _ -> True
                _ -> False
            "pid" -> case matchRegex (mkRegex "^([0-9]{9})$") value of
                Just _ -> True
                _ -> False
            "cid" -> True

parseChunk :: [String] -> [Token] -> ([String], [Token])
parseChunk [] tokens = ([], tokens)
parseChunk (x:xs) tokens
    | x == "" = (xs, tokens)
    | otherwise = parseChunk xs (tokens ++ catMaybes (map parseToken words))
        where words = splitOn " " x

process :: [String] -> [Maybe Passport]
process [] = []
process lines = (mkPassport (snd thisChunk) : process (fst thisChunk))
    where thisChunk = parseChunk lines []

validTokens :: [String]
validTokens = [
    "byr" {- Birth Year -},
    "iyr" {- Issue Year -},
    "eyr" {- Expiration Year -},
    "hgt" {- Height -},
    "hcl" {- Hair Color -},
    "ecl" {- Eye Color -},
    "pid" {- Passport ID -},
    "cid" {- Country ID -}
    ]


main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let passport = process ls :: [Maybe Passport]
    let validPassports = catMaybes passport

    print $ length validPassports
