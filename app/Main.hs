module Main (main) where

import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile =
  do result <- many line
     eof
     return result

eol :: GenParser Char st Char
eol = char '\n'

line :: GenParser Char st [String]
line =
  do result <- cells
     eol
     return result

cells :: GenParser Char st [String]
cells =
  do first <- cellContent
     next <- remainingCells
     return (first : next)

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

remainingCells :: GenParser Char st [String]
remainingCells =
  (char ',' >> cells)
  <|> return []


parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

main :: IO ()
main = print r
  where
     r =
       case parseCSV "1,2,3\n4,5,6\n" of
         Left error -> undefined
         Right parsed -> parsed
    
