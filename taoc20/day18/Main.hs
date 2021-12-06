module Main where

import Data.Either
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator


data Expression =
    Add Expression Expression |
    Mult Expression Expression |
    EInt Int deriving Show

{-
binomy :: Expression -> Parser Expression
binomy exp = f <$> symbol "+" <*> term
    where f _ x = Add exp x

monomy :: Expression -> Parser Expression
monomy exp = f <$> symbol "*" <*> term
    where f _ x = Mult exp x

expr :: Parser Expression
expr = do leftExp <- term
          go leftExp
    where
        go :: Expression -> Parser Expression
        go e = do newExp <- binomy e <|> monomy e
                  go newExp
               <|> return e
-}


expr :: Parser Expression
expr = do t <- term
          do symbol "*"
             e <- expr
             return (Mult t e)
           <|> return t

term :: Parser Expression
term = do f <- factor
          do symbol "+"
             t <- term
             return (Add t f)
           <|> return f

factor :: Parser Expression
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> natural

symbol :: String -> Parser String
symbol xs = foo (string xs)

natural :: Parser Expression
natural = EInt <$> foo nat

nat :: Parser Int
nat = read <$> many1 digit

foo :: Parser a -> Parser a
foo p = do spaces
           v <- p
           spaces
           return v


eval :: Expression -> Int
eval (Add x y)  = eval x + eval y
eval (Mult x y) = eval x * eval y
eval (EInt x)   = x


main :: IO ()
main = do
    raw <- readFile "input.txt"
    let ls = lines raw
        result = sum $ rights $ map (parse expr "" >=> (return . eval)) ls
    print result
