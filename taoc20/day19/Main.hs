module Main where

import Data.List
import Data.Maybe
import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
--import Text.Parsec.Text

type RuleId = Int

data Rule =
    RefRule [RuleId] |
    OrRule [Rule] |
    LitRule Char deriving Show


type RuleItem = (RuleId, Rule)
type RuleDict = [RuleItem]

getRule :: RuleDict -> RuleId -> Maybe Rule
getRule ruleDict ruleId = let
        idFinder :: (RuleId, Rule) -> Bool
        idFinder (id, _) = ruleId == id
        desiredPair = find idFinder ruleDict :: Maybe (RuleId, Rule)
        maybeRule = desiredPair >>= \(_, rule) -> return rule
    in maybeRule

type File = (RuleDict, [String])

parseFile :: Parser File
parseFile = mkFile <$> parseRules <*> parseSamples
    where mkFile rules samples = (rules, samples)

-- Rules parsers
parseRules :: Parser RuleDict
parseRules = rule `manyTill` endOfLine

rule :: Parser RuleItem
rule = mkRuleItem <$> (number <* symbol ":") <*> (litRule <|> orRule <|> refRule)
    where mkRuleItem id rule = (id, rule)

refRule :: Parser Rule
refRule = RefRule <$> numbers <* endOfLine

orRule :: Parser Rule
orRule = mkOrRule <$> (numbers `sepBy1` sep) <* endOfLine
    where
        sep = char '|' <* spaces'
        mkOrRule :: [[RuleId]] -> Rule
        mkOrRule = OrRule . map RefRule

litRule :: Parser Rule
litRule = LitRule <$> between (char '\"') (char '\"') anyChar <* endOfLine

-- Samples parser
parseSamples :: Parser [String]
parseSamples = sample `manyTill` eof

sample :: Parser String
sample = anyChar `manyTill` endOfLine

-- Helpers
symbol :: String -> Parser String
symbol xs = spaces *> string xs <* spaces

numbers :: Parser [RuleId]
numbers = many1 (number <* (many (char ' ')))

spaces' :: Parser String
spaces' = many (char ' ')

number :: Parser Int
number = read <$> many1 digit

-- string parser Creator
--stringParser :: RuleDict -> Rule -> String

-- parser Creator
createParser :: RuleDict -> Rule -> Maybe ( Parser String )
createParser ruleDict rule = createParser' rule
    where
        createParser' :: Rule -> Maybe ( Parser String )
        createParser' (LitRule c) = Just ( string [c] )
        createParser' (RefRule xs) = foldl1 combineParsers parsers
            where
                parsers = map createParser' xs :: [ Maybe (Parser String) ]
                combineParsers :: [ Maybe (Parser String) ] -> [ Maybe (Parser String) ] -> [ Maybe (Parser String) ]
                combineParsers x y = liftM2 (<*>)
        createParser' (OrRule xs) = foldl1 combineParsers parsers 
            where
                parsers = map createParser' xs :: [ Maybe (Parser String) ]
                combineParsers x y = liftM2 ( <|> )
        --foldl1 (\x -> \y -> ( <|> ) <$> ( createParser' x ) <*> ( createParser' y )) xs --foldl1 ( ++ . go ) xs

--createParser _ (LitRule c) = (\x -> [x]) <$> char c
--createParser ruleDict OrRule xs = map xs subRule *> createParser
--createParser ruleDict (RefRule xs) = catMaybes $ map createParser' xs
    --where
        --createParser' :: RuleId -> Maybe (Parser String)
        --createParser' ruleId = (getRule ruleDict) >=> return . createParser rule

main :: IO ()
main = do
    file <- parseFromFile parseFile "input.txt"
    print file
