module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TestingUtils (buildAssertion)

import Formula

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Put parens"
    [ testCase "Tautology" $
      buildAssertion paren
      (implication [variable "a", negation $ negation $ variable "a"])
      "test/data/tautology.txt"
    , testCase "Axiom 1" $
      buildAssertion paren
      (implication [variable "a", implication [variable "b", variable "a"]])
      "test/data/axiom-1.txt"
    , testCase "Axiom 2" $
      buildAssertion paren
      (implication
       [ implication [variable "a", variable "b"]
       , implication
         [ implication [variable "a", implication [variable "b", variable "c"]]
         , implication [variable "a", variable "c"]
         ]
       ]
      )
      "test/data/axiom-2.txt"
    , testCase "Axiom 3" $
      buildAssertion paren
      (implication
       [ conjunction [variable "a", variable "b"]
       , variable "a"
       ]
      )
      "test/data/axiom-3.txt"
    , testCase "Axiom 4" $
      buildAssertion paren
      (implication [conjunction [variable "a", variable "b"], variable "b"])
      "test/data/axiom-4.txt"
    , testCase "Axiom 5" $
      buildAssertion paren
      (implication
       [ variable "a"
       , implication
         [ variable "b"
         , conjunction [variable "a", variable "b"]
         ]
       ]
      )
      "test/data/axiom-5.txt"
    , testCase "Axiom 6" $
      buildAssertion paren
      (implication
       [ variable "a"
       , disjunction [variable "a", variable "b"]
       ]
      )
      "test/data/axiom-6.txt"
    , testCase "Axiom 7" $
      buildAssertion paren
      (implication
       [ variable "b"
       , disjunction [variable "a", variable "b"]
       ]
      )
      "test/data/axiom-7.txt"
    , testCase "Axiom 8" $
      buildAssertion paren
      (implication
       [ implication [variable "a", variable "c"]
       , implication
         [ implication [variable "b", variable "c"]
         , implication
           [ disjunction [variable "a", variable "b"]
           , variable "c"
           ]
         ]
       ]
      )
      "test/data/axiom-8.txt"
    , testCase "Axiom 9" $
      buildAssertion paren
      (implication
       [ implication [variable "a", variable "b"]
       , implication
         [ implication [variable "a", negation $ variable "b"]
         , negation $ variable "a"
         ]
       ]
      )
      "test/data/axiom-9.txt"
    , testCase "Axiom 10" $
      buildAssertion paren
      (implication
       [ variable "a"
       , implication
         [negation $ variable "a"
         , variable "b"
         ]
       ]
      )
      "test/data/axiom-10.txt"
    ]
  ]
