module Main where

import qualified Data.Either as Either
import Formula
import Prover.Prover
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import TestingUtils (buildAssertion)
import qualified KripkeModel
import qualified Debug.Trace as Trace
main :: IO ()
main = defaultMain tests

assertDerivation :: Formula -> Bool
assertDerivation formula = KripkeModel.derivable model formula
  where
    model = Either.fromLeft undefined $ buildDerivationTree formula

tests =
  [ testGroup
      "Building derivation tree"
      [ testCase "Derivable: a -> b -> a" $
          buildAssertion
            (Either.isRight . buildDerivationTree)
            True
            "test/data/axiom-1.txt",
        testCase "Derivable: (a -> b) -> (a -> b -> c) -> (a -> c)" $
          buildAssertion
            (Either.isRight . buildDerivationTree)
            True
            "test/data/axiom-2.txt",
        testCase "Not derivable: a -> b" $
          buildAssertion
            (Either.isLeft . buildDerivationTree)
            True
            "test/data/not-tautology-1.txt",
        testCase "Not derivable: a" $
          buildAssertion
            (Either.isLeft . buildDerivationTree)
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
      
    ]
  ]
