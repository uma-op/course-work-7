module Prover.Prover where

import Formula
import Prover.DerivationTree
import Prover.Sequent


import Debug.Trace

buildDerivationTree :: Formula -> DerivationTree
buildDerivationTree = buildDerivationTree' . fromSequent . fromFormula
  where
    buildDerivationTree' = applyRight
      where
        applyAxiom tree =
          case axiom tree of
            (newTree, True) -> buildDerivationTree' newTree
            (oldTree, False) -> oldTree
            
        applyRight tree =
          case right tree of
            (newTree, True) -> applyRight newTree
            (oldTree, False) -> applyFocus oldTree
            
        applyFocus tree =
          case focus tree of
            (newTree, True) -> applyFocus newTree  -- actually we need to apply focus rule only one time
            (oldTree, False) -> applyLeft oldTree

        applyLeft tree =
          case left tree of
            (newTree, True) -> applyAxiom newTree
            (oldTree, False) -> applyRestart oldTree

        applyRestart tree =
          case restart tree of
            (newTree, True) -> buildDerivationTree' newTree
            (oldTree, False) -> applyAxiom oldTree
