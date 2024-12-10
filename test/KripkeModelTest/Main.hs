module Main where

-- Adapted from an example by Don Stewart. For licensing information
-- please see the file "example/Test/Framework/Example.lhs" in the source tree.
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit ( assertEqual )
import TestingUtils (buildAssertion)

import Formula
import KripkeModel (decomposeFormula)
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Formula decomposition"
    [ testCase "Simple" $
      buildAssertion decomposeFormula
      ( Set.fromList
        [ variable "p"
        , negation $ variable "p"
        , negation $ negation $ variable "p"
        , implication [variable "p", negation $ negation $ variable "p"]
        ]
      )
      "test/data/tautology.txt"
    ]
  ]
