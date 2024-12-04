module Main (main) where

import Parser
import KripkeModel

main :: IO ()
main =
  do
    parsed <- readFormula
    case parsed of
      Left err -> print err
      Right formula -> print $ decomposeFormula formula

