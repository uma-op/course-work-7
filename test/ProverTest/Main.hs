module Main where

import Formula
import Prover.Prover
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import TestingUtils (buildAssertion)
import qualified KripkeModel
import Prover.Sequent (reduceToImplication, fromFormula)

main :: IO ()
main = defaultMain tests

assertDerivation :: Formula -> Bool
assertDerivation formula = case buildDerivationTree formula of
                             Left model -> KripkeModel.derivable model formula
                             Right _ -> True

tests =
  [ testGroup
      "Building derivation tree"
      [ testCase "Derivable: a -> b -> a" $
          buildAssertion assertDerivation
            True
            "test/data/axiom-1.txt",
        testCase "Derivable: (a -> b) -> (a -> b -> c) -> (a -> c)" $
          buildAssertion assertDerivation
            True
            "test/data/axiom-2.txt",
        testCase "Not derivable: a -> b" $
          buildAssertion (not . assertDerivation)
            True
            "test/data/not-tautology-1.txt",
        testCase "Not derivable: a" $
          buildAssertion (not . assertDerivation)
            True
            "test/data/simple.txt"
      ],
    testGroup
    "Building counter model"
    [ testCase "" $
      buildAssertion (not . assertDerivation)
      True
      "test/data/not-tautology-1.txt",
      testCase "" $
      buildAssertion (not . assertDerivation)
      True
      "test/data/bad-implication.txt"
    ],
    testGroup
    "Testing with absurdity"
    [ testCase "" $
      buildAssertion assertDerivation
      True
      "test/data/axiom-11.txt",
      testCase "" $
      buildAssertion (not . assertDerivation)
      True
      "test/data/distributed-negation.txt"
    ],
    testGroup
    "Reducing IPC to implicational fragment"
    [ testCase "" $
      buildAssertion reduceToImplication
      (fromFormula (implication [variable "a", implication [variable "a", absurdity], variable "b"]))
      "test/data/axiom-10.txt",
      testCase "" $
      buildAssertion reduceToImplication
      (fromFormula (implication [variable "a", implication [implication [variable "a", absurdity], absurdity]]))
      "test/data/tautology.txt"
    ],
    testGroup
    "Testing with negation"
    [ testCase "" $
      buildAssertion assertDerivation
      True
      "test/data/axiom-10.txt",
      testCase "" $
      buildAssertion assertDerivation
      True
      "test/data/tautology.txt",
      testCase "" $
      buildAssertion assertDerivation
      True
      "test/data/axiom-9.txt",
      testCase "" $
      buildAssertion (not . assertDerivation)
      True
      "test/data/negation.txt"
    ]
  ]
