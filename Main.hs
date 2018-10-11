module Main where

import Parser

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

main :: IO ()
main = do
  runParser " 12 - 23 - 3 "
  runParser " (((999)))"
  runParser " 1 * 22 - 32 / 4 + 35"
  runParser "var = (-12) * 12"
  runParser "2 * 4 * 8"
  runParser "2 ^ 3 ^ 4"
  runParser "-(12 * 13) + (-2)* 3^(1 + 1)^1 + 20 / 4"