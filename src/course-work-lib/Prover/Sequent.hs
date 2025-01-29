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
    leftLabeled :: !(Map Atom [Formula]),
    leftUnlabeled :: ![Formula],
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
      ++ show (leftUnlabeled s)
      ++ " => ["
      ++ show (Set.toList $ rightFocus s)
      ++ "] ** "
      ++ show (rightFormula s)

fromFormula :: Formula -> Sequent
fromFormula formula =
  Sequent
    { usedContexts = Set.empty,
      leftFocus = Set.empty,
      leftLabeled = Map.fromList $ List.map (,[]) (Set.toList $ atoms formula),
      leftUnlabeled = [],
      rightFocusUsed = Set.empty,
      rightFocus = Set.empty,
      rightFormula = formula
    }
