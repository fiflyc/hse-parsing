module Combinators where
-- Make sure that the names don't clash
import Prelude hiding (lookup, (>>=), map, pred, return, elem)
import qualified Tokenizer as T(isWhiteSpace, isAlpha, isDigit, isBracket)

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
  let inp' = delSpaces inp in
  case p inp' of
    Error _ -> q inp'
    result  -> result

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 >>=
(>>=) :: Parser a -> (a -> Parser b ) -> Parser b
p >>= q = \inp ->
  case p inp of
    Success (r, inp') -> q r inp'
    Error err -> Error err

-- Sequential combinator which ignores the result of the first parser
infixl 7 |>
(|>) :: Parser a -> Parser b -> Parser b
p |> q = p >>= const q

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

delSpaces :: String -> String
delSpaces []             = []
delSpaces (c : [])       =
  case T.isWhiteSpace c of
    True  -> []
    False -> c : []
delSpaces (c1 : c2 : []) =
  case T.isWhiteSpace c1 of
    True  -> delSpaces $ c2 : []
    False -> c1 : (delSpaces $ c2 : [])
delSpaces (c1 : c2 : c3 : s)  = 
  case T.isWhiteSpace c1 of
    True  -> delSpaces $ c2 : c3 : s
    False ->
      case T.isWhiteSpace c2 of
        True  ->
          case T.isWhiteSpace c3 of
            True  -> delSpaces $ c1 : c3 : s
            False ->
              case beTogether c1 c3 of
                True  -> c1 : c3 : s
                False -> c1 : c2 : c3 : s
        False -> c1 : (delSpaces $ c2 : c3 : s)

imerge :: Integer -> Integer -> Integer
imerge a b = read $ (show a) ++ (show b) :: Integer

beTogether :: Char -> Char -> Bool
beTogether c1 c2 =
  case T.isBracket c1 || T.isBracket c2 of
    True -> True
    False -> (T.isAlpha c1 || T.isDigit c1) /= (T.isAlpha c2 || T.isDigit c2)
