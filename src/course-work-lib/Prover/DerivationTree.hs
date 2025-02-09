module Prover.DerivationTree where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Trace
import qualified Formula
import Prover.Sequent

data RuleType = Axiom | Focus | Restart | Right | Left deriving (Show)

data DerivationTree = DerivationTree
  { goals :: !(Map Int Sequent),
    lastNodeId :: !Int,
    currentGoal :: !(Maybe Int)
  }
  deriving (Show)

-- traceCurrentGoal :: DerivationTree -> String
-- traceCurrentGoal tree = List.concatMap showEdge chain ++ show (nodes tree Map.! 0) ++ "\n"
--   where
--     showEdge e = show (edges tree Map.! e) ++ " | " ++ show (nodes tree Map.! snd e) ++ "\n"
--     keys = Map.keys $ edges tree
--     findChain :: [(Int, Int)] -> Int -> [(Int, Int)]
--     findChain links lst = List.reverse (findChain' links lst [])
--       where
--         findChain' links' lst' acc' =
--           let (newLasts, newLinks) = List.partition ((== lst') . snd) links'
--            in if List.null newLasts then acc' else findChain' newLinks (fst $ head newLasts) (head newLasts : acc')
--     chain = findChain keys (Maybe.fromJust $ currentGoal tree)

fromSequent :: Sequent -> DerivationTree
fromSequent s =
  DerivationTree
    { goals = Map.singleton 0 s,
      lastNodeId = 0,
      currentGoal = Just 0
    }

addSequent :: Sequent -> DerivationTree -> DerivationTree
addSequent s tree =
  DerivationTree
    { goals = Map.insert (lastNodeId tree + 1) s (goals tree),
      lastNodeId = lastNodeId tree + 1,
      currentGoal = Just $ lastNodeId tree + 1
    }

deleteSequent :: Int -> DerivationTree -> DerivationTree
deleteSequent goalId tree =
  tree
    { goals = newGoals,
      currentGoal = if Map.null newGoals then Nothing else Just $ fst $ Map.findMin newGoals
    }
  where
    newGoals = Map.delete goalId $ goals tree

axiom :: DerivationTree -> (DerivationTree, Bool)
axiom tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
axiom tree =
  if rightFormula goal `Set.member` leftFocus goal || Formula.absurdity `Set.member` leftFocus goal
    then
      (deleteSequent goalId tree, True)
    else (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal = Maybe.fromMaybe (error "Axiom. No such goal") $ goalId `Map.lookup` goals tree

focus :: DerivationTree -> (DerivationTree, Bool)
focus tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
focus tree =
  if not (List.all (`Set.member` leftFocus goal) (leftUnlabeled goal))
    then
      let newSequent = goal {leftFocus = leftUnlabeled goal}
          newTree = (addSequent newSequent . deleteSequent goalId) tree
       in (newTree, True)
    else (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal = goals tree Map.! goalId

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
                  Map.adjust (`Set.union` leftUnlabeled goal) (Formula.atom $ rightFormula goal) $
                    Map.adjust (const Set.empty) used $
                      leftLabeled goal,
                leftUnlabeled = unlabeled,
                rightFocusUsed = Set.insert used $ rightFocusUsed goal,
                rightFocus = unused,
                rightFormula = Formula.variable used
              }
            where
              (used, unused) = Set.deleteFindMin $ rightFocus goal
              unlabeled = case used `Map.lookup` leftLabeled goal of
                Just v -> v
                Nothing -> error "Restart. No such goal"

          newTree = (addSequent newSequent . deleteSequent goalId) tree
       in (newTree, True)
    else
      (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal = case goalId `Map.lookup` goals tree of
      Just v -> v
      Nothing -> error "Restart. No such goal"

right :: DerivationTree -> (DerivationTree, Bool)
right tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
right tree =
  if Formula.getFormulaType (rightFormula goal) == Formula.ImplicationType
    then
      let (Formula.Implication _ lhs rhs) = rightFormula goal -- implication type means implication with two operands
          newSequent =
            goal
              { leftUnlabeled = Set.insert lhs $ leftUnlabeled goal, -- TODO: check the append ordering
                rightFormula = rhs
              }
          newTree = (addSequent newSequent . deleteSequent goalId) tree
       in (newTree, True)
    else (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal = goals tree Map.! goalId

left :: DerivationTree -> (DerivationTree, Bool)
left tree@(DerivationTree {currentGoal = Nothing}) = (tree, False)
left tree =
  if Maybe.isJust leftmost
    then
      let formula = Maybe.fromJust leftmost
          lhs = Formula.lhs formula
          rhs = Formula.rhs formula
          newSequentLeft =
            goal
              { usedContexts = Set.insert (formula, Formula.atom $ rightFormula goal) $ usedContexts goal,
                leftLabeled =
                  Map.adjust
                    (`Set.union` leftUnlabeled goal)
                    (Formula.atom $ rightFormula goal)
                    (leftLabeled goal),
                rightFocus =
                  if Set.member (Formula.atom $ rightFormula goal) (rightFocusUsed goal)
                    then rightFocus goal
                    else Set.insert (Formula.atom $ rightFormula goal) (rightFocus goal),
                rightFormula = lhs
              }
          newSequentRight =
            goal
              { usedContexts = Set.insert (formula, Formula.atom $ rightFormula goal) $ usedContexts goal,
                leftUnlabeled = Set.insert rhs $ leftUnlabeled goal
              }
          newTree =
            ( addSequent newSequentLeft
                . addSequent newSequentRight
                . deleteSequent goalId
            )
              tree
       in (newTree, True)
    else (tree, False)
  where
    goalId = Maybe.fromJust $ currentGoal tree
    goal = case goalId `Map.lookup` goals tree of
      Just v -> v
      Nothing -> error "There is no such goal"

    leftmost = List.foldl leftmostFindFunc Nothing (leftFocus goal)
      where
        leftmostFindFunc m f
          | Formula.getFormulaType f == Formula.ImplicationType
              && not ((f, Formula.atom $ rightFormula goal) `Set.member` usedContexts goal) =
              case m of
                Nothing -> Just f
                Just m' -> if Formula.length f > Formula.length m' then Just f else m
          | otherwise = m

--          | Formula.getFormulaType f == Formula.ImplicationType =
--              not $
--                (f, Formula.atom $ rightFormula goal)
--                  `Set.member` usedContexts goal
--          | otherwise = False
