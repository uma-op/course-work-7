{-# LANGUAGE TupleSections #-}

module Prover.Sequent where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Formula
import KripkeModel (decomposeFormula)
import qualified Debug.Trace as Trace

data Sequent = Sequent
  { usedContexts :: !(Set (Formula, Atom)),
    leftFocus :: !(Set Formula),
    leftLabeled :: !(Map Atom (Set Formula)),
    leftUnlabeled :: !(Set Formula),
    rightFocusUsed :: !(Set Atom),
    rightFocus :: !(Set Atom),
    rightFormula :: !Formula
  }
  deriving (Eq, Ord)

instance Show Sequent where
  show s =
    show (Set.toList $ usedContexts s)
      ++ " ** {"
      ++ show (Set.toList $ leftFocus s)
      ++ "} ** "
      ++ show (Map.toList $ leftLabeled s)
      ++ " ** "
      ++ show (Set.toList $ leftUnlabeled s)
      ++ " => ["
      ++ show (Set.toList $ Set.union (rightFocus s) (rightFocusUsed s))
      ++ "] ** "
      ++ show (rightFormula s)

fromFormula :: Set Formula -> Formula -> Sequent
fromFormula premises formula =
  Sequent
    { usedContexts = Set.empty,
      leftFocus = Set.empty,
      leftLabeled = Map.fromSet (const Set.empty) (atoms formula),
      leftUnlabeled = Set.empty,
      rightFocusUsed = Set.empty,
      rightFocus = Set.empty,
      rightFormula = formula
    }

-- reduceToImplication :: Formula -> Sequent
-- reduceToImplication formula = reduceToImplication' formula (fromFormula formula)
--   where
--     reduceToImplication' :: Formula -> Sequent -> Sequent
--     reduceToImplication' formula sequent =
--       case formula of
--         Variable {} -> fromFormula formula
--         Negation {} ->
--           let reduced = reduceToImplication' (operand formula) sequent
--            in reduced
--                 { leftLabeled = Map.insert (atom absurdity) Set.empty (leftLabeled sequent),
--                   rightFormula = implication [rightFormula reduced, absurdity]
--                 }
--         Implication {} ->
--           let reducedLhs = reduceToImplication' (lhs formula) sequent
--               reducedRhs = reduceToImplication' (rhs formula) sequent
--            in sequent
--                 { leftLabeled = Map.union (leftLabeled reducedLhs) (leftLabeled reducedRhs),
--                   rightFormula = implication [rightFormula reducedLhs, rightFormula reducedRhs]
--                 }
--         _otherwise -> sequent

reduceToImplication :: Formula -> (Set Formula, Formula)
reduceToImplication formula = (fa, Trace.trace (show a') a')
  where
    distributeNegation formula =
      case formula of
        Variable {} -> formula
        Negation {} -> implication [distributeNegation $ operand formula, absurdity]
        Implication {} -> implication [distributeNegation $ lhs formula, distributeNegation $ rhs formula]
        Conjunction {} -> conjunction [distributeNegation $ lhs formula, distributeNegation $ rhs formula]
        Disjunction {} -> disjunction [distributeNegation $ lhs formula, distributeNegation $ rhs formula]
    distributed = distributeNegation formula

    subformulas = decomposeFormula distributed

    hashFormula = variable . ('#' :) . show
    atomMapping = Set.fold foldingFunction Set.empty (Set.insert absurdity $ Set.filter ((== VariableType) . getFormulaType) subformulas)
      where
        foldingFunction element =
          Set.insert (implication [element, hashFormula element])
            . Set.insert (implication [hashFormula element, element])

    -- subformulasMapping = Set.map (implication . ([hashFormula absurdity] ++) . List.singleton) subformulas
    subformulasMapping = Set.fold foldingFunction Set.empty subformulas
      where
        foldingFunction element container =
          case element of
            Implication {} ->
              ( Set.insert (implication [hashFormula element, hashFormula $ lhs element, hashFormula $ rhs element]) .
                Set.insert (implication [implication [hashFormula $ lhs element, hashFormula $ rhs element], hashFormula element]) .
                Set.insert (implication [hashFormula absurdity, hashFormula element])
              ) container
            Conjunction {} ->
              ( Set.insert (implication [hashFormula element, hashFormula $ lhs element]) .
                Set.insert (implication [hashFormula element, hashFormula $ rhs element]) .
                Set.insert (implication [hashFormula $ lhs element, hashFormula $ rhs element, hashFormula element]) .
                Set.insert (implication [hashFormula absurdity, hashFormula element])
              ) container
            _otherwise -> Set.insert (implication [hashFormula absurdity, hashFormula element]) container

    fa = Set.union atomMapping subformulasMapping
    a' = implication (Set.toList fa ++ [hashFormula distributed])
