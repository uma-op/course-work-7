module TestingUtils where

import Parser
import Paths_course_work

import Test.HUnit ((@?), AssertionPredicable(assertionPredicate))
import qualified Data.Either as Either
import Text.Pretty.Simple (pPrint)

parseFromFile filename =
  do
    filepath <- getDataFileName filename
    input <- readFile filepath
    return $ parseLogicalFormula (filter (' ' /=) $ filter('\n' /=) input)

buildAssertionPredicable :: (Eq a, Show a) => a -> a -> IO Bool
buildAssertionPredicable expected got =
  do
    if got == expected then
      return True
    else do
      putStrLn "===== TEST FAILED ====="
      putStrLn "Expected:"
      print expected
      putStrLn "Got:"
      print got
      return False

buildAssertion mutateInput expected filename = predicate @? ""
  where
    predicate = assertionPredicate $
      do
        parsed <- parseFromFile filename
        return $ buildAssertionPredicable expected (mutateInput $ Either.fromRight undefined parsed)
  
