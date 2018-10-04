module Tokenizer where

data Token = TDigit Integer
           | TIdent String
           | TOp Operator
           | TLParen
           | TRParen
           | TAssign
           | TEof
           deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Pow
              deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEof]
tokenize str =
        case t of
          TDigit x ->
            case head ts of
              TDigit y -> TDigit (imerge x y) : (drop 1 ts)
              otherwise -> t : ts
          TIdent s ->
            case head ts of
              TIdent ss -> TIdent (s ++ ss) : (drop 1 ts)
              otherwise -> t : ts
          otherwise -> t : ts
        where
          t = head $ strtosym str
          ts = tokenize $ drop 1 $ delSpaces str

strtosym :: String -> [Token]
strtosym [] = [TEof]
strtosym (c : cs) | isOperator c   = TOp (operator c) : strtosym cs
                  | isDigit c      = TDigit (digit c) : strtosym cs
                  | isAlpha c      = TIdent [alpha c] : strtosym cs
                  | c == '('       = TLParen : strtosym cs
                  | c == ')'       = TRParen : strtosym cs
                  | c == '='       = TAssign : strtosym cs
                  | isWhiteSpace c = strtosym cs
                  | otherwise = error ("Lexical error: unacceptable character " ++ [c])

imerge :: Integer -> Integer -> Integer
imerge a b = read $ (show a) ++ (show b) :: Integer

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Pow
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

digit :: Char -> Integer
digit c | c == '0' = 0
        | c == '1' = 1
        | c == '2' = 2
        | c == '3' = 3
        | c == '4' = 4
        | c == '5' = 5
        | c == '6' = 6
        | c == '7' = 7
        | c == '8' = 8
        | c == '9' = 9
digit c = error ("Lexical error: " ++ c : " is not a digit!")

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

alpha :: Char -> Char
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

delSpaces :: String -> String
delSpaces [] = []
delSpaces (c : str) | c `elem` " \t\n" = delSpaces str
                    | otherwise = c : delSpaces str 