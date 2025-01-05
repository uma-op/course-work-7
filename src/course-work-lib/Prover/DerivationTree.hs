module Prover.DerivationTree where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Prover.Sequent

data RuleType = Axiom | Focus | Restart | Right | Left
data DerivationTree = DerivationTree
  { edges :: !(Map (Int, Int) RuleType)
  , nodes :: !(Map Int Sequent)
  , goals :: !(Set Int)
  , lastNodeId :: !Int
  }
  
addSequent :: Int -> Sequent -> RuleType -> DerivationTree -> DerivationTree
addSequent goalId s rt tree = DerivationTree
  { edges = Map.insert (goalId, lastNodeId tree + 1) rt $ edges tree
  , nodes = Map.insert (lastNodeId tree + 1) s $ nodes tree
  , goals = Set.insert (lastNodeId tree + 1) $ Set.delete goalId $ goals tree
  , lastNodeId = lastNodeId tree + 1
  }
  
axiom :: DerivationTree -> Int -> (DerivationTree, Bool)
axiom tree goalId =
  if rightFormula goal `Set.member` leftFocus goal
  then (tree { goals = Set.delete goalId (goals tree) }, True)
  else (tree, False)
  where
    goal = nodes tree Map.! goalId

focus :: DerivationTree -> Int -> (DerivationTree, Bool)
focus tree goalId =
  if not (List.all (`Set.member` leftFocus goal) (leftUnlabeled goal))
  then
    let
      newSequent = goal { leftFocus = Set.fromList $ leftUnlabeled goal }
      newTree = addSequent goalId newSequent Prover.Focus tree
    in
      (newTree, True)
  else (tree, False)
  where
    goal = nodes tree Map.! goalId


restart :: DerivationTree -> Int -> (DerivationTree, Bool)
restart tree goalId =
  if (not . Sequence.null) (rightFocus goal)
  then
    let
      newSequent = goal
        { usedContexts = Set.empty
        , leftFocus = Set.empty
        , leftLabeled = Map.adjust (++ leftUnlabeled goal) (atom $ rightFormula goal)
                        $ Map.adjust (const []) used
                        $ leftLabeled goal
        , leftUnlabeled = leftUnlabeled goal ++ unlabeled
        , rightFocusUsed = Set.insert used $ rightFocusUsed goal
        , rightFocus = unused Sequence.|> atom (rightFormula goal)
        , rightFormula = variable used
        }
        where
          used :<| unused = rightFocus goal
          unlabeled = leftLabeled goal Map.! used
          
      newTree = addSequent goalId newSequent Prover.Restart tree
    in
      (newTree, True)
  else
    (tree, False)
  where
    goal = nodes tree Map.! goalId

right :: DerivationTree -> Int -> (DerivationTree, Bool)
right tree goalId =
  if getFormulaType (rightFormula goal) == ImplicationType
  then
    let
      [lhs, rhs] = Formula.operands $ rightFormula goal  -- implication type means implication with two operands
      newSequent = goal
        { leftUnlabeled = lhs : leftUnlabeled goal  -- TODO: check the append ordering
        , rightFormula = rhs
        }
      newTree = addSequent goalId newSequent Prover.Right tree
    in
      (newTree, True)
  else (tree, True)
  where
    goal = nodes tree Map.! goalId
    
left :: DerivationTree -> Int -> (DerivationTree, Bool)
left tree goalId =
  if Maybe.isJust leftmost
  then
    let
      formula = Maybe.fromJust leftmost
      [lhs, rhs] = Formula.operands formula
      newSequentLeft = goal
        { usedContexts = Set.insert (formula, atom $ rightFormula goal) $ usedContexts goal
        , leftLabeled = Map.adjust
                        (++ leftUnlabeled goal)
                        (atom $ rightFormula goal)
                        (leftLabeled goal)
        , rightFocus = rightFocus goal Sequence.|> atom (rightFormula goal)
        , rightFormula = lhs
        }
      newSequentRight = goal
        { usedContexts = Set.insert (formula, atom $ rightFormula goal) $ usedContexts goal
        , leftUnlabeled = rhs : leftUnlabeled goal
        }
      newTree = ( addSequent goalId newSequentLeft Prover.Left
                . addSequent goalId newSequentRight Prover.Left
                ) tree
    in
      (newTree, True)
  else (tree, False)
  where
    goal = nodes tree Map.! goalId
    leftmost = List.find leftmostFindFunc (leftFocus goal)
      where
        leftmostFindFunc f
          | getFormulaType f == ImplicationType = (f, atom $ rightFormula goal)
                                                  `Set.member` usedContexts goal
          | otherwise = False
