{-# LANGUAGE TupleSections #-}
module Prover.Sequent where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq(..))
import qualified Data.List as List

import Formula

data Sequent = Sequent
  { usedContexts :: !(Set (Formula, Atom))
  , leftFocus :: !(Set Formula)
  , leftLabeled :: !(Map Atom [Formula])
  , leftUnlabeled :: ![Formula]
  , rightFocusUsed :: !(Set Atom)
  , rightFocus :: !(Seq Atom)
  , rightFormula :: !Formula
  } deriving (Show, Eq, Ord)

fromFormula :: Formula -> Sequent
fromFormula formula =
  Sequent
  { usedContexts = Set.empty
  , leftFocus = Set.empty
  , leftLabeled = Map.fromList $ List.map (, []) (Set.toList $ atoms formula)
  , leftUnlabeled = []
  , rightFocusUsed = Set.empty
  , rightFocus = Sequence.empty
  , rightFormula = formula
  }

