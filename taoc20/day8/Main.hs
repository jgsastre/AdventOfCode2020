module Main where

import Text.Regex
import Data.List
import Data.Maybe (catMaybes)

newtype AccValue = AccValue Int deriving Show

newtype Offset = Offset Int deriving (Eq, Show)

data State = State AccValue Offset deriving Show
getOffset :: State -> Offset
getOffset (State _ o) = o

data Command = Nop Int | Jmp Int | Acc Int deriving Show
parseCommand :: String -> Maybe Command
parseCommand token = matchRegex (mkRegex regex) line >>= mkCommand
    where
        line = filter (\x -> x /= '+') token
        regex = "(nop|acc|jmp) ([-0-9]*)"
        mkCommand :: [String] -> Maybe Command
        mkCommand ["nop", quantity] = Just (Nop (read quantity))
        mkCommand ["acc", quantity] = Just (Acc (read quantity))
        mkCommand ["jmp", quantity] = Just (Jmp (read quantity))
        _                           = Nothing

execCommand :: Command -> State -> State
execCommand (Nop       _) (State value (Offset offset)) = State value (Offset (offset + 1))
execCommand (Acc operand) (State (AccValue value) (Offset offset)) = State (AccValue (value + operand)) (Offset (offset + 1))
execCommand (Jmp operand) (State value (Offset offset)) = State value (Offset (offset + operand))

newtype Program = Program [Command] deriving Show
getCommand :: Program -> Offset -> Command
getCommand (Program xs) (Offset o) = head $ drop o xs

concatProgram :: Program -> Program -> Program
concatProgram (Program left) (Program right) = Program (left ++ right)

programFinished :: Program -> Offset -> Bool
programFinished (Program xs) (Offset o) = length xs == o

executeProgram :: Program -> State -> State
executeProgram program initState = execUntilLoop initState []
    where
        execUntilLoop :: State -> [Offset] -> State
        execUntilLoop state offsets
            | elem offset offsets             = state
            | programFinished program offset  = state
            | otherwise                       = execUntilLoop newState (offset:offsets)
                where
                    offset = getOffset state
                    command = getCommand program offset
                    newState = execCommand command state


modifyProgram :: Program -> [Program]
modifyProgram program = modifyProgram' (Program []) program
    where
        modifyProgram' :: Program -> Program -> [Program]
        modifyProgram' _ (Program []) = []
        modifyProgram' preProgram (Program ((Acc x):xs)) = modifyProgram' newOriginalProgram (Program xs)
            where newOriginalProgram = concatProgram preProgram (Program [(Acc x)])
        modifyProgram' preProgram (Program (x:xs)) = modifiedProgram : (modifyProgram' newOriginalProgram (Program xs))
            where
                newOriginalProgram = concatProgram preProgram (Program [x])
                modifiedProgram = concatProgram (concatProgram preProgram (Program [modifiedCommand])) (Program xs)
                modifiedCommand = case x of
                    Jmp x -> Nop x
                    Nop x -> Jmp x

initialState = State (AccValue 0) (Offset 0) :: State

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ln = lines raw
    let program = Program $ catMaybes $ map parseCommand ln
    let finalState = executeProgram program initialState
    let modifiedPrograms = modifyProgram program
    let finalStates = map (\x -> executeProgram x initialState) modifiedPrograms
    let stateProgramEnds = find (\x -> programFinished program (getOffset x)) finalStates
    print stateProgramEnds
