module Main where

-- Adapted from an example by Don Stewart. For licensing information
-- please see the file "example/Test/Framework/Example.lhs" in the source tree.
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TestingUtils (buildAssertion)

import Formula
import KripkeModel (decomposeFormula)
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Formula decomposition"
    [ testCase "Tautology" $
      buildAssertion decomposeFormula
      ( Set.fromList
        [ variable "a"
        , negation $ variable "a"
        , negation $ negation $ variable "a"
        , implication [variable "a", negation $ negation $ variable "a"]
        ]
      )
      "test/data/tautology.txt"
    , testCase "Axiom 1" $
      buildAssertion decomposeFormula
      ( Set.fromList
       [ variable "a"
       , variable "b"
       , implication [variable "b", variable "a"]
       , implication [variable "a", implication [variable "b", variable "a"]]
       ]
      )
      "test/data/axiom-1.txt"
    , testCase "Axiom 2" $
      buildAssertion decomposeFormula
      (Set.fromList
       [ variable "a"
       , variable "b"
       , variable "c"
       , implication [variable "a", variable "b"]
       , implication [variable "a", variable "c"]
       , implication [variable "b", variable "c"]
       , implication [variable "a", implication [variable "b", variable "c"]]
       , implication
         [ implication
           [ variable "a"
           , implication
             [ variable "b"
             , variable "c"
             ]
           ]
         , implication
           [ variable "a"
           , variable "c"
           ]
         ]
       , implication
         [ implication [variable "a", variable "b"]
         , implication
           [ implication
             [ variable "a"
             , implication
               [ variable "b"
               , variable "c"
               ]
             ]
           , implication
             [ variable "a"
             , variable "c"
             ]
           ] 
         ]
       ]
      )
      "test/data/axiom-2.txt"
    , testCase "Axiom 3" $
      buildAssertion decomposeFormula
      (Set.fromList
       [ variable "a"
       , variable "b"
       , conjunction [variable "a", variable "b"]
       , implication [conjunction [variable "a", variable "b"], variable "a"]
       ])
      "test/data/axiom-3.txt"
    , testCase "Axiom 4" $
      buildAssertion decomposeFormula
      (Set.fromList
       [ variable "a"
       , variable "b"
       , conjunction [variable "a", variable "b"]
       , implication [conjunction [variable "a", variable "b"], variable "b"]
       ]
      )
      "test/data/axiom-4.txt"
    , testCase "Long implication" $
      buildAssertion decomposeFormula
      (Set.fromList
       [ variable "a"
       , variable "b"
       , variable "c"
       , variable "d"
       , variable "e"
       , implication [variable "a", variable "a"]
       , implication [variable "b", variable "c"]
       , implication [variable "d", variable "e"]
       , implication [variable "a", implication [variable "a", variable "a"]]
       , implication [variable "a", implication [variable "b", variable "c"]]
       , implication [variable "d", implication [variable "d", variable "e"]]
       , implication
         [ implication [variable "a", implication [variable "b", variable "c"]]
         , implication [variable "d", implication [variable "d", variable "e"]]
         ]
       , implication
         [ implication [variable "a", implication [variable "a", variable "a"]]
         , implication
           [ implication [variable "a", implication [variable "b", variable "c"]]
           , implication [variable "d", implication [variable "d", variable "e"]]
           ]
         ]
       ]
      )
      "test/data/long-implication.txt"
    ]
  ]
