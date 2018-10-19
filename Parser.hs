module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AST = APart AST AST
         | ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | APow T.Operator AST AST
         | AUnary AST
         | AAssign String AST
         | ANum Integer
         | AIdent String

parse :: String -> Maybe (Result AST)
parse []  = Nothing
parse inp = Just $ fstres $ ( matchSpaces >>>= \_ ->
                              exprlist >>>= \e ->
                              isempty |>> return e
                            ) inp

exprlist :: Parser AST
exprlist =
  ( expression >>>= \e ->
    separator |>>
    exprlist >>>= \l -> return (APart e l)
  )
  <|> expression

expression :: Parser AST
expression =
  ( identifier >>>= \(AIdent i) ->
    assignment |>>
    expression >>>= \e -> return (AAssign i e)
  )
  <|> ( term       >>>= \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>>= \op ->
        expression >>>= \r  -> return (ASum op l r)
      )
  <|> term

term :: Parser AST
term =
  -- make sure we don't reparse the factor (Term -> Factor (('/' | '*') Term | epsilon ))
  factor >>>= \l ->
  ( ( divMult >>>= \op ->
      term    >>>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

cell :: Parser AST
cell =
  ( unary >>>= \m ->
    cell >>>= \c -> return (AUnary c)
  )
  <|>
  ( lparen |>>
    expression >>>= \e ->
    rparen |>> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> number

factor :: Parser AST
factor =
  cell >>>= \l ->
  ( ( pow >>>= \p ->
      factor >>>= \r -> return (APow p l r)
    )
    <|> return l
  )

number :: Parser AST
number =
  ( digit >>=  \d ->
    number >>= \(ANum n) -> return (ANum $ imerge d n)
  )
  <|> digit >>= \d -> return (ANum d)

identifier :: Parser AST
identifier =
  ( symb >>= \c ->
    identifier >>= \(AIdent s) -> return (AIdent $ c : s)
  )
  <|> symb >>= \c -> return (AIdent $ c : [])

digit :: Parser Integer
digit = map T.digit (sat T.isDigit elem)

symb :: Parser Char
symb = sat T.isAlpha elem

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

pow :: Parser T.Operator
pow = map T.operator (char '^')

unary :: Parser T.Operator
unary = map T.operator (char '-')

separator :: Parser Char
separator = char ';'




instance Show AST where
  show tree = show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  APart l r    -> ";\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  APow  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  AUnary     e -> "-\n" ++  show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> show i)
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'
      showOp T.Pow   = '^'