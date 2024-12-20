module Main where

-- Adapted from an example by Don Stewart. For licensing information
-- please see the file "example/Test/Framework/Example.lhs" in the source tree.
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TestingUtils (buildAssertion)

import Formula
import KripkeModel (decomposeFormula, KripkeModel (..), derivable)
import qualified Data.Set as Set
import qualified Data.Map as Map

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
    ],
    testGroup "Formula derivation"
    [ testCase "Simple" $
      buildAssertion (derivable ( KripkeModel
                                  { worlds = Set.fromList [ 1 ]
                                  , order = Set.empty
                                  , valuation = Map.fromList [((1, "a"), False)]
                                  }
                                ))
      False
      "test/data/simple.txt"
    , testCase "Negation" $
      buildAssertion (derivable ( KripkeModel
                                  { worlds = Set.fromList [ 1, 2 ]
                                  , order = Set.fromList [ (1, 2) ]
                                  , valuation =
                                      Map.fromList
                                      [ ((1, "a"), False)
                                      , ((2, "a"), True)
                                      ]
                                  
                                  }
                                ))
      False
      "test/data/negation.txt"
    , testCase "Non tautology" $
      buildAssertion (derivable ( KripkeModel
                                  { worlds = Set.fromList [ 1, 2 ]
                                  , order = Set.fromList [ (1, 2) ]
                                  , valuation =
                                      Map.fromList
                                      [ ((1, "a"), False)
                                      , ((2, "a"), True)
                                      ]
                                  }
                                ))
      False
      "test/data/not-tautology.txt"
    , testCase "Another tautology" $
      buildAssertion (derivable ( KripkeModel
                                  { worlds = Set.fromList [ 1, 2, 3, 4 ]
                                  , order = Set.fromList [ (1, 2), (1, 3), (2, 4), (3, 4) ]
                                  , valuation =
                                      Map.fromList
                                      [ ((1, "a"), False)
                                      , ((1, "b"), True)
                                      , ((2, "a"), False)
                                      , ((2, "b"), True)
                                      , ((3, "a"), False)
                                      , ((3, "b"), True)
                                      , ((4, "a"), False)
                                      , ((4, "b"), True)
                                      ]
                                  }
                                ))
      True
      "test/data/another-tautology.txt"
    ]
  ]
