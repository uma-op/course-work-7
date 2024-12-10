module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TestingUtils (buildAssertion)

import Formula

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Parsing"
    [ testCase "Tautology" $
      buildAssertion id
      (implication [variable "p", negation $ negation$ variable "p"])
      "test/data/tautology.txt"
    ]
  ]
