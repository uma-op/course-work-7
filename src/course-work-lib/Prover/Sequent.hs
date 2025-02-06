{-# LANGUAGE TupleSections #-}

module Prover.Sequent where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Formula

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
      ++ show (Set.toList $ rightFocus s)
      ++ "] ** "
      ++ show (rightFormula s)

fromFormula :: Formula -> Sequent
fromFormula formula =
  Sequent
    { usedContexts = Set.empty,
      leftFocus = Set.empty,
      leftLabeled = Map.fromList $ List.map (,Set.empty) (Set.toList $ atoms formula),
      leftUnlabeled = Set.empty,
      rightFocusUsed = Set.empty,
      rightFocus = Set.empty,
      rightFormula = formula
    }

reduceToImplication :: Formula -> Sequent
reduceToImplication formula = reduceToImplication' formula (fromFormula formula)
  where
    reduceToImplication' :: Formula -> Sequent -> Sequent
    reduceToImplication' formula sequent =
      case formula of
        Variable {} -> fromFormula formula
        Negation {} ->
          let reduced = reduceToImplication' (operand formula) sequent
           in reduced
                { leftLabeled = Map.insert (atom absurdity) Set.empty (leftLabeled sequent),
                  rightFormula = implication [rightFormula reduced, absurdity]
                }
        Implication {} ->
          let reducedLhs = reduceToImplication' (lhs formula) sequent
              reducedRhs = reduceToImplication' (rhs formula) sequent
           in sequent
                { leftLabeled = Map.union (leftLabeled reducedLhs) (leftLabeled reducedRhs),
                  rightFormula = implication [rightFormula reducedLhs, rightFormula reducedRhs]
                }
        _otherwise -> sequent
