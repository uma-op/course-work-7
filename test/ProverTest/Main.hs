module Main where

import qualified Data.Set as Set
import Formula
import qualified KripkeModel
import Prover.Prover
import Prover.Sequent (fromFormula, reduceToImplication)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import TestingUtils (buildAssertion)
import qualified DebugUtils

main :: IO ()
main = defaultMain tests

assertDerivation :: Formula -> Bool
assertDerivation formula = case buildDerivationTree formula of
  Left model -> KripkeModel.derivable model formula
  Right _ -> True

tests =
  [ testGroup
      "Reducing IPC to implicational fragment"
      [ testCase "" $
          buildAssertion
            reduceToImplication
            ( let fa =
                    Set.fromList
                      [ implication [variable "#_|_", variable "_|_"],
                        implication [variable "_|_", variable "#_|_"],
                        implication [variable "#a", variable "a"],
                        implication [variable "a", variable "#a"],
                        implication [variable "#b", variable "b"],
                        implication [variable "b", variable "#b"],
                        implication [variable "#_|_", variable "#_|_"],
                        implication [variable "#_|_", variable "#a"],
                        implication [variable "#_|_", variable "#b"],
                        implication [variable "#_|_", variable "#(a -> _|_)"],
                        implication [variable "#_|_", variable "#((a -> _|_) -> b)"],
                        implication [variable "#_|_", variable "#(a -> ((a -> _|_) -> b))"],
                        implication [variable "#(a -> _|_)", variable "#a", variable "#_|_"],
                        implication [implication [variable "#a", variable "#_|_"], variable "#(a -> _|_)"],
                        implication [variable "#((a -> _|_) -> b)", variable "#(a -> _|_)", variable "#b"],
                        implication [implication [variable "#(a -> _|_)", variable "#b"], variable "#((a -> _|_) -> b)"],
                        implication [variable "#(a -> ((a -> _|_) -> b))", variable "#a", variable "#((a -> _|_) -> b)"],
                        implication [implication [variable "#a", variable "#((a -> _|_) -> b)"], variable "#(a -> ((a -> _|_) -> b))"]
                      ]
                  hashedA = variable "#(a -> ((a -> _|_) -> b))"
               in (fa, implication (Set.toList fa ++ [hashedA]))
            )
            "test/data/axiom-10.txt",
        testCase "" $
          buildAssertion
            reduceToImplication
            ( let fa =
                    Set.fromList
                      [ implication [variable "#a", variable "a"],
                        implication [variable "a", variable "#a"],
                        implication [variable "#_|_", variable "_|_"],
                        implication [variable "_|_", variable "#_|_"],
                        implication [variable "#_|_", variable "#a"],
                        implication [variable "#_|_", variable "#(a -> a)"],
                        implication [variable "#(a -> a)", variable "#a", variable "#a"],
                        implication [implication [variable "#a", variable "#a"], variable "#(a -> a)"]
                      ]
                  hashedA = variable "#(a -> a)"
               in (fa, DebugUtils.trace $ implication (Set.toList fa ++ [hashedA]))
            )
            "test/data/axiom-12.txt"
      ],
    testGroup
      "Building derivation tree"
      [ testCase "" $
          buildAssertion
            assertDerivation
            True
            "test/data/axiom-13.txt"
            --        testCase "" $
            --          buildAssertion
            --            reduceToImplication
            --            ( let fa = []
            --                  hashedA = undefined
            --               in (Set.fromList fa, implication (fa ++ [hashedA]))
            --            )
            -- (fromFormula (implication [variable "a", implication [implication [variable "a", absurdity], absurdity]]))
            --            "test/data/tautology.txt"
      ]
      --    testGroup
      --      "Building derivation tree"
      --      [ testCase "Derivable: a -> b -> a" $
      --          buildAssertion
      --            assertDerivation
      --            True
      --            "test/data/axiom-1.txt",
      --        testCase "Derivable: (a -> b) -> (a -> b -> c) -> (a -> c)" $
      --          buildAssertion
      --            assertDerivation
      --            True
      --            "test/data/axiom-2.txt",
      --        testCase "Not derivable: a -> b" $
      --          buildAssertion
      --            (not . assertDerivation)
      --            True
      --            "test/data/not-tautology-1.txt",
      --        testCase "Not derivable: a" $
      --          buildAssertion
      --            (not . assertDerivation)
      --            True
      --            "test/data/simple.txt"
      --      ],
      --    testGroup
      --      "Building counter model"
      --      [ testCase "" $
      --          buildAssertion
      --            (not . assertDerivation)
      --            True
      --            "test/data/not-tautology-1.txt",
      --        testCase "" $
      --          buildAssertion
      --            (not . assertDerivation)
      --            True
      --            "test/data/bad-implication.txt"
      --      ],
      --    testGroup
      --      "Testing with absurdity"
      --      [ testCase "" $
      --          buildAssertion
      --            assertDerivation
      --            True
      --            "test/data/axiom-11.txt",
      --        testCase "" $
      --          buildAssertion
      --            (not . assertDerivation)
      --            True
      --            "test/data/distributed-negation.txt"
      --      ],
      --    testGroup
      --      "Testing with negation"
      --      [ testCase "" $
      --          buildAssertion
      --            assertDerivation
      --            True
      --            "test/data/axiom-10.txt",
      --        testCase "" $
      --          buildAssertion
      --            assertDerivation
      --            True
      --            "test/data/tautology.txt",
      --        testCase "" $
      --          buildAssertion
      --            assertDerivation
      --            True
      --            "test/data/axiom-9.txt",
      --        testCase "" $
      --          buildAssertion
      --            (not . assertDerivation)
      --            True
      --            "test/data/negation.txt"
      --      ]
  ]
