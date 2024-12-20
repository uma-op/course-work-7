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
      (implication [variable "a", negation $ negation $ variable "a"])
      "test/data/tautology.txt"
    , testCase "Axiom 1" $
      buildAssertion id
      (implication [variable "a", variable "b", variable "a"])
      "test/data/axiom-1.txt"
    , testCase "Axiom 2" $
      buildAssertion id
      (implication
       [ implication [variable "a", variable "b"]
       , implication [variable "a", variable "b", variable "c"]
       , implication [variable "a", variable "c"]
       ]
      )
      "test/data/axiom-2.txt"
    , testCase "Axiom 3" $
      buildAssertion id
      (implication
       [ conjunction [variable "a", variable "b"]
       , variable "a"
       ]
      )
      "test/data/axiom-3.txt"
    , testCase "Axiom 4" $
      buildAssertion id
      (implication [conjunction [variable "a", variable "b"], variable "b"])
      "test/data/axiom-4.txt"
    , testCase "Axiom 5" $
      buildAssertion id
      (implication
       [ variable "a"
       , variable "b"
       , conjunction [variable "a", variable "b"]
       ]
      )
      "test/data/axiom-5.txt"
    , testCase "Axiom 6" $
      buildAssertion id
      (implication
       [ variable "a"
       , disjunction [variable "a", variable "b"]
       ]
      )
      "test/data/axiom-6.txt"
    , testCase "Axiom 7" $
      buildAssertion id
      (implication
       [ variable "b"
       , disjunction [variable "a", variable "b"]
       ]
      )
      "test/data/axiom-7.txt"
    , testCase "Axiom 8" $
      buildAssertion id
      (implication
       [ implication [variable "a", variable "c"]
       , implication [variable "b", variable "c"]
       , disjunction [variable "a", variable "b"]
       , variable "c"
       ]
      )
      "test/data/axiom-8.txt"
    , testCase "Axiom 9" $
      buildAssertion id
      (implication
       [ implication [variable "a", variable "b"]
       , implication [variable "a", negation $ variable "b"]
       , negation $ variable "a"
       ]
      )
      "test/data/axiom-9.txt"
    , testCase "Axiom 10" $
      buildAssertion id
      (implication
       [ variable "a"
       , negation $ variable "a"
       , variable "b"
       ]
      )
      "test/data/axiom-10.txt"
    ]
  ]
