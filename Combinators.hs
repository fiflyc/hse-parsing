module Combinators where
-- Make sure that the names don't clash
import Prelude hiding (lookup, (>>=), map, pred, return, elem, until)

-- Input abstraction
type Input = String

-- Result is polymorphic in the ... result
data Result r = Success r
              | Error String
              deriving (Show)

-- The result of parsing is some payload r and the suffix which wasn't parsed
type Parser r = Input -> Result (r, Input)

-- Choice combinator: checks if the input can be parsed with either the first, or the second parser
-- Left biased: make sure, that the first parser consumes more input
infixl 6 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \inp -> 
  case p inp of
    Error _ -> q inp
    result  -> result

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 >>=
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= q = \inp ->
  case p inp of
    Success (r, inp') -> q r inp'
    Error err -> Error err

-- Like previous but ignores white spaces
infixl 7 >>>=
(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= q = 
  p >>= \pr ->
  matchSpaces >>= \_ ->
  matchComments >>= \_ -> 
  matchSpaces >>= \_ -> q pr

-- Sequential combinator which ignores the result of the first parser
infixl 7 |>
(|>) :: Parser a -> Parser b -> Parser b
p |> q = p >>= const q

-- Like previous but ignores white spaces
infixl 7 |>>
(|>>) :: Parser a -> Parser b -> Parser b
p |>> q = p >>>= const q

-- Succeedes without consuming any input, returning a value
return :: a -> Parser a
return r inp = Success (r, inp)

-- Always fails
zero :: String -> Parser a
zero err = const $ Error err

-- Chops of the first element of the string
elem :: Parser Char
elem (c : cs) = Success (c, cs) 
elem [] = Error "Empty string"

-- Checks if the first character of the string is the given one
char :: Char -> Parser Char
char c = sat (== c) elem

-- Checks if the parser result satisfies the predicate
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred parser inp =
  case parser inp of
    Success (r, inp') | pred r ->  Success (r, inp')
    Success _ -> Error "Predicate is not satisfied"
    Error err -> Error err

-- Applies the function to the result of the parser
map :: (a -> b) -> Parser a -> Parser b
map f parser inp =
  case parser inp of
    Success (r, inp') -> Success (f r, inp')
    Error err -> Error err

-- Removes string consists comments from the top of input, always returns Success
matchComments :: Parser String
matchComments =
  ( comment >>= \s1 ->
    matchSpaces >>= \s2 ->
    matchComments >>= \s3 -> return (s1 ++ s2 ++ s3)
  )
  <|> comment
  <|> return []

-- Removes spaces from the top of input, always returns Success
matchSpaces :: Parser String
matchSpaces [] = Success ([], [])
matchSpaces (c : inp) =
  case c of
    ' '   -> (matchSpaces >>= \s -> return (' '  : s)) inp
    '\n'  -> (matchSpaces >>= \s -> return ('\n' : s)) inp
    '\t'  -> (matchSpaces >>= \s -> return ('\t' : s)) inp
    other -> Success ([], c : inp)

comment :: Parser String
comment =
  ( lcomm |>
    until rcomm >>= \str ->
    return $ "//" ++ str
  )
  <|> ( lcomm |>
        until empty >>= \str ->
        return $ "//" ++ str
      )
  <|> ( lmulcomm |>
        until rmulcomm >>= \str ->
        return $ "/*" ++ str ++ "*/"
      )

empty :: Parser Char
empty []   = Success ('\0', [])
empty str  = Error ("Syntax error on: " ++ str)

fstres :: Result (a, b) -> Result a
fstres r =
  case r of
    Error err -> Error err
    Success (a, b) -> Success a

until :: Parser a -> Parser String
until p []  =
  case p [] of
    Success _ -> Success ([], [])
    Error err -> Error err
until p (c : inp) =
  case p (c : inp) of
    Success (_, inp') -> Success ([], inp')
    Error _ -> until p inp

lcomm :: Parser String
lcomm =
  char '/' >>= \_ ->
  char '/' >>= \_ -> return "//"

rcomm :: Parser String
rcomm =
  char '\n' >>= \_ -> return "\n"

lmulcomm :: Parser String
lmulcomm =
  char '/'  >>= \_ ->
  char '*'  >>= \_ -> return "/*"

rmulcomm :: Parser String
rmulcomm =
  char '*'  >>= \_ ->
  char '/'  >>= \_ -> return "*/"