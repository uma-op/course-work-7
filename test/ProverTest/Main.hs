module Main where

import qualified Data.Set as Set

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import TestingUtils (buildAssertion)

import Prover.DerivationTree
import Prover.Prover

import Formula

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Building derivation tree"
    [ testCase "Derivable: a -> b -> a" $
      buildAssertion (Set.null . goals . buildDerivationTree . paren)
      True
      "test/data/axiom-1.txt"
    , testCase "Derivable: (a -> b) -> (a -> b -> c) -> (a -> c)" $
      buildAssertion (Set.null . goals . buildDerivationTree . paren)
      True
      "test/data/axiom-2.txt"
    , testCase "Not derivable: a -> b" $
      buildAssertion (Set.null . goals . buildDerivationTree . paren)
      False
      "test/data/not-tautology-1.txt"
    , testCase "Not derivable: a" $
      buildAssertion (Set.null . goals . buildDerivationTree . paren)
      False
      "test/data/simple.txt"
    ]
  ]

