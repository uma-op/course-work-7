module Parser where

import Text.ParserCombinators.Parsec
import Formula

{-

disjunction ::= conjucntion disjunction'
disjunction' ::= \/ conjunction disjunction | e
conjunction ::= conjunction' implication
conjunction' ::= /\ conjunction' implication | e
implication ::= negation implication'
implication' ::= -> negation implication' | e
negation ::= -variable | variable | _|_
variable ::= [a-z]+ | (disjunction)

-}

data ParsingStatus = Done | Continue

buildParseInput :: String -> String -> (String, ParsingStatus)
buildParseInput other "\n" = (other, Done)
buildParseInput other new = (other ++ new, Continue)

readFormula :: IO (Either ParseError Formula)
readFormula =
  do
    input <- readParseInput ""
    return (parseLogicalFormula (filter (' ' /=) $ filter ('\n' /=) input))
  where
    parseLogicalFormula = parse disjunction "(unknown)"
    readParseInput readed =
      do
        new <- getLine
        case buildParseInput readed (new ++ "\n") of
          (result, Done) -> return result
          (result, Continue) -> readParseInput result

disjunctionOp :: GenParser Char st String
disjunctionOp = string "\\/"

conjunctionOp :: GenParser Char st String
conjunctionOp = string "/\\"

implicationOp :: GenParser Char st String
implicationOp = string "->"

negationOp :: GenParser Char st Char
negationOp = char '-'

variable :: GenParser Char st Formula
variable =
  do
    _ <- string "_|_"
    return Absurdity
  <|>
  do
    _ <- char '('
    disjunct <- disjunction
    _ <- char ')'
    return disjunct
  <|>
  do
    first <- letter
    other <- many letter
    return (Variable (first : other))

negation :: GenParser Char st Formula
negation =
  do
    _ <- negationOp
    Negation <$> negation
  <|>
  do
    variable


implication :: GenParser Char st Formula
implication =
  do
    neg <- negation
    other <- implication'
    case other of
      [] -> return neg
      _nonEmpty -> return (Implication (neg : other))
  where
    implication' =
      do
        _ <- implicationOp
        neg <- negation
        other <- implication'
        return (neg : other)
      <|> return []

conjunction :: GenParser Char st Formula
conjunction =
  do
    impl <- implication
    other <- conjunction'
    case other of
      [] -> return impl
      _nonEmpty -> return (Conjunction (impl : other))
  where
    conjunction' =
      do
        _ <- conjunctionOp
        impl <- implication
        other <- conjunction'
        return (impl : other)
      <|> return []
        

disjunction :: GenParser Char st Formula
disjunction =
  do
    conjunct <- conjunction
    other <- disjunction'
    case other of
      [] -> return conjunct
      _nonEmpty -> return (Disjunction (conjunct : other))
  where
    disjunction' =
      do
        _ <- disjunctionOp
        conjunct <- conjunction
        other <- disjunction'
        return (conjunct : other)
      <|> return []

