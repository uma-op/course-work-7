{-# LANGUAGE TupleSections #-}

module Prover.Prover where

import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import Formula
import KripkeModel (KripkeModel (..))
import Prover.DerivationTree
import Prover.Sequent
import Text.Pretty.Simple (pShow)
import qualified Debug.Trace as Trace
import Data.Text.Lazy (unpack)

buildCounterModel :: DerivationTree -> KripkeModel
buildCounterModel tree@(DerivationTree {currentGoal = Nothing}) = undefined
buildCounterModel tree = model
  where
    failedGoalId = currentGoal tree
    failedGoal' = nodes tree Map.! Maybe.fromJust failedGoalId
    failedGoal = Trace.trace (unpack $ pShow failedGoal') failedGoal'
    model =
      KripkeModel
        { worlds = Set.fromList listedWorlds,
          order = Set.fromList $ List.map (0,) listedWorlds,
          valuation =
            Map.unions
              [ Map.fromList (List.map ((,False) . (0,)) $ Foldable.toList $ rightFocusUsed failedGoal),
                Map.fromList [((0, atom $ rightFormula failedGoal), False), ((1, atom $ rightFormula failedGoal), False)],
                Map.fromList (List.concatMap (\w -> List.map ((,True) . (w,)) $ getAtoms $ leftUnlabeled failedGoal) listedWorlds),
                Map.fromList
                  ( List.concat $
                      List.zipWith
                        (\f s -> ((f, fst s), False) : List.map ((,True) . (f,)) (getAtoms $ snd s))
                        (List.drop 2 listedWorlds)
                        (Map.toList $ leftLabeled failedGoal)
                  )
              ]
        }
      where
        listedWorlds = List.take ((2 +) $ Set.size (rightFocusUsed failedGoal)) [0..]
        getAtoms = List.foldl getAtomsFoldingFunction []
        getAtomsFoldingFunction container element = case element of
          Variable _ a -> a : container
          _unused -> container

buildDerivationTree :: Formula -> Either KripkeModel DerivationTree
buildDerivationTree formula =
  if Set.null $ goals builded
    then Either.Right builded
    else Either.Left $ buildCounterModel builded
  where
    builded = buildDerivationTree' $ fromSequent $ fromFormula formula
    buildDerivationTree' = applyAxiom
      where
        applyAxiom tree =
          case Trace.trace "Axiom" $ axiom tree of
            (newTree, True) -> applyAxiom newTree
            (oldTree, False) -> applyRight oldTree

        applyRight tree =
          case Trace.trace "Right" $ right tree of
            (newTree, True) -> applyAxiom newTree
            (oldTree, False) -> applyFocus oldTree

        applyFocus tree =
          case Trace.trace "Focus" $ focus tree of
            (newTree, True) -> applyAxiom newTree
            (oldTree, False) -> applyLeft oldTree

        applyLeft tree =
          case Trace.trace "Left" $ left tree of
            (newTree, True) -> applyAxiom newTree
            (oldTree, False) -> applyRestart oldTree

        applyRestart tree =
          case Trace.trace "Restart" $ restart tree of
            (newTree, True) -> applyAxiom newTree
            (oldTree, False) -> oldTree
