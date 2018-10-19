{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree ++ "\n"
  show (Just (Error err)) = "\nSyntax error: " ++ err
  show Nothing = "\nEmpty tree"

main :: IO ()
main = do
  runParser " 12 - 23 - 3 "
  runParser " (((999)))"
  runParser " 1 * 22 - 32 / 4 + 35"
  runParser "var = (-12) * 12"
  runParser "!"
  runParser "-1 + 2"
  runParser "2 ^ 3 ^ 4"
  runParser "12 * 13 + (-2) * 3^(1 + 1)^(-1) + 20 / (-4)"
  runParser "f   ;  f; f "
  runParser "1 * 2 - 3 / 4 + (-5) ^ 73; width = 14 + 128 * -length"
  runParser "hop hey lalaley gde vopros a gde otvet"
  runParser "dvaxdva = 4;\ndvaxdva = 4\n/*\neto vsem isvestno\nv tselom\nmire\n*/"
  runParser "//kek"
  runParser "@\nOut in the distance\nThere's so much gold\nThe treasure that I've found\nIs more than enough\nFar to the hill we've to go"