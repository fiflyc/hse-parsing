module Parser where

import Tokenizer
import Prelude hiding (lookup)

data AST = ASum Operator AST AST
         | AProd Operator AST AST
         | APow Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AIdent String

parse :: String -> Maybe AST
parse input =
  let ts = tokenize input in
  case ts of
    [TEof] -> Nothing
    _ -> let (tree, ts') = expression ts in
         if ts' == [TEof]
         then Just tree
         else error ("Parsing error on: " ++ show ts')

expression :: [Token] -> (AST, [Token])
expression ts =
  let (termpNode, ts') = termp ts in
  case lookup ts' of
    TOp op | op == Plus || op == Minus ->
      let (exprNode, ts'') = expression $ accept ts' in
      (ASum op termpNode exprNode, ts'')
    TAssign ->
      case termpNode of
        AIdent v -> let (exprNode, ts'') = expression $ accept ts' in
                    (AAssign v exprNode, ts'')
        _ -> error "Syntax error: assignment is only possible to identifiers"
    _ -> (termpNode, ts')

term :: [Token] -> (AST, [Token])
term ts =
  let (factNode, ts') = factor ts in
  case lookup ts' of
    TOp op | op == Mult || op == Div ->
      let (termpNode, ts'') = termp $ accept ts' in
      (AProd op factNode termpNode, ts'')
    _ -> (factNode, ts')

termp :: [Token] -> (AST, [Token])
termp ts =
  let (factNode, ts') = factor ts in
  case lookup ts' of
    TOp op | op == Pow ->
      let (termpNode, ts'') = termp $ accept ts' in
      (APow op factNode termpNode, ts'')
    _ -> term ts

factor :: [Token] -> (AST, [Token])
factor ts =
  case lookup ts of
    TLParen ->
      let (exprNode, ts') = expression $ accept ts in
      case lookup ts' of
        TRParen -> (exprNode, accept ts')
        _ -> error "Syntax error: mismatched parentheses"
    TIdent v -> (AIdent v, accept ts)
    TDigit d -> (ANum d, accept ts)
    _ -> error "Syntax error: factor can only be a digit, an identifier or a parenthesised expression"

lookup :: [Token] -> Token
lookup = head

accept :: [Token] -> [Token]
accept = tail

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  APow op l r  -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> show i)
      ident = (+1)
      showOp Plus  = '+'
      showOp Minus = '-'
      showOp Mult  = '*'
      showOp Div   = '/'
      showOp Pow   = '^'