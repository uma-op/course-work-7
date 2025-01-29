module Prover.DerivationTree where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Trace
import Formula
import Prover.Sequent

data RuleType = Axiom | Focus | Restart | Right | Left deriving (Show)

data DerivationTree = DerivationTree
  { edges :: !(Map (Int, Int) RuleType),
    nodes :: !(Map Int Sequent),
    goals :: !(Set Int),
    lastNodeId :: !Int,
    currentGoal :: !(Maybe Int)
  }
  deriving (Show)

fromSequent :: Sequent -> DerivationTree
fromSequent s =
  DerivationTree
    { edges = Map.empty,
      nodes = Map.singleton 0 s,
      goals = Set.singleton 0,
      lastNodeId = 0,
      currentGoal = Just 0
    }

addSequent :: Int -> Sequent -> RuleType -> DerivationTree -> DerivationTree
addSequent goalId s rt tree =
  DerivationTree
    { edges = Map.insert (goalId, lastNodeId tree + 1) rt $ edges tree,
      nodes = Map.insert (lastNodeId tree + 1) s $ nodes tree,
      goals = Set.insert (lastNodeId tree + 1) $ Set.delete goalId $ goals tree,
      lastNodeId = lastNodeId tree + 1,
      currentGoal = Just $ lastNodeId tree + 1
    }

axiom :: DerivationTree -> (DerivationTree, Bool)
axiom tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
axiom tree =
  if rightFormula goal `Set.member` leftFocus goal
    then
      let newGoals = Set.delete goalId (goals tree)
          newTree =
            tree
              { goals = newGoals,
                currentGoal = if Set.null newGoals then Nothing else Just $ Set.findMin newGoals
              }
       in (newTree, True)
    else (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal' = nodes tree Map.! goalId
    goal = Trace.trace (show goal') goal'

focus :: DerivationTree -> (DerivationTree, Bool)
focus tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
focus tree =
  if not (List.all (`Set.member` leftFocus goal) (leftUnlabeled goal))
    then
      let newSequent = goal {leftFocus = Set.fromList $ leftUnlabeled goal}
          newTree = addSequent goalId newSequent Prover.DerivationTree.Focus tree
       in (newTree, True)
    else (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal' = nodes tree Map.! goalId
    goal = Trace.trace (show goal') goal'

restart :: DerivationTree -> (DerivationTree, Bool)
restart tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
restart tree =
  if (not . Set.null) (rightFocus goal)
    then
      let newSequent =
            goal
              { usedContexts = Set.empty,
                leftFocus = Set.empty,
                leftLabeled =
                  Map.adjust (++ leftUnlabeled goal) (atom $ rightFormula goal) $
                    Map.adjust (const []) used $
                      leftLabeled goal,
                leftUnlabeled = unlabeled,
                rightFocusUsed = Set.insert used $ rightFocusUsed goal,
                rightFocus = unused,
                rightFormula = variable used
              }
            where
              (used, unused) = Set.deleteFindMin $ rightFocus goal
              unlabeled = leftLabeled goal Map.! used

          newTree = addSequent goalId newSequent Prover.DerivationTree.Restart tree
       in (newTree, True)
    else
      (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal' = nodes tree Map.! goalId
    goal = Trace.trace (show goal') goal'

right :: DerivationTree -> (DerivationTree, Bool)
right tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
right tree =
  if getFormulaType (rightFormula goal) == ImplicationType
    then
      let [lhs, rhs] = Formula.operands $ rightFormula goal -- implication type means implication with two operands
          newSequent =
            goal
              { leftUnlabeled = lhs : leftUnlabeled goal, -- TODO: check the append ordering
                rightFormula = rhs
              }
          newTree = addSequent goalId newSequent Prover.DerivationTree.Right tree
       in (newTree, True)
    else (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal' = nodes tree Map.! goalId
    goal = Trace.trace (show goal') goal'

left :: DerivationTree -> (DerivationTree, Bool)
left tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
left tree =
  if Maybe.isJust leftmost
    then
      let formula = Maybe.fromJust leftmost
          [lhs, rhs] = Formula.operands formula
          newSequentLeft =
            goal
              { usedContexts = Set.insert (formula, atom $ rightFormula goal) $ usedContexts goal,
                leftLabeled =
                  Map.adjust
                    (++ leftUnlabeled goal)
                    (atom $ rightFormula goal)
                    (leftLabeled goal),
                rightFocus =
                  if Set.member (atom $ rightFormula goal) (rightFocusUsed goal)
                    then rightFocus goal
                    else Set.insert (atom $ rightFormula goal) (rightFocus goal),
                rightFormula = lhs
              }
          newSequentRight =
            goal
              { usedContexts = Set.insert (formula, atom $ rightFormula goal) $ usedContexts goal,
                leftUnlabeled = rhs : leftUnlabeled goal
              }
          newTree =
            ( addSequent goalId newSequentLeft Prover.DerivationTree.Left
                . addSequent goalId newSequentRight Prover.DerivationTree.Left
            )
              tree
       in (newTree, True)
    else (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal' = nodes tree Map.! goalId
    goal = Trace.trace (show goal') goal'
    leftmost = List.find leftmostFindFunc (leftFocus goal)
      where
        leftmostFindFunc f
          | getFormulaType f == ImplicationType =
              not $
                (f, atom $ rightFormula goal)
                  `Set.member` usedContexts goal
          | otherwise = False
