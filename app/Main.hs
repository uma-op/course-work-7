module Main (main) where

import Parser

main :: IO ()
main =
  do
    parsed <- readFormula
    case parsed of
      Left err -> print err
      Right formula -> print formula
