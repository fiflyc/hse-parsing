{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn $ input ++ "\n"
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

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