module KripkeModel where

import Formula ( Atom, Formula(..) )
import qualified Formula

-- import Data.List ( List )
import qualified Data.List as List
import Data.Map ( Map )
-- import qualified Data.Map as Map
import Data.Set ( Set, (\\) )
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

type World = Int
type Order = Set (World, World)
type Valuation = Map (World, Atom) Bool

data KripkeModel = KripkeModel
  { worlds :: !(Set World)
  , order :: !Order
  , valuation :: !Valuation
  }

isValuationCorrect :: KripkeModel -> Bool
isValuationCorrect = undefined

related :: World -> World -> KripkeModel -> Bool
related w1 w2 model = related' $ Set.singleton w1
  where
   related' done | Set.member w2 done = True
                 | otherwise = related' $ Set.map snd $ Set.filter ((`Set.member` done) . fst) (order model)

greater :: World -> KripkeModel -> Set World
greater w model = greater' (Set.singleton w) (Set.singleton w)
  where
    greater' gs done | Set.null done = gs
                     | Set.null newDone = gs
                     | otherwise = greater' (Set.union newDone gs) newDone
                     where
                       newDone = gs \\ Set.map snd (Set.filter ((`Set.member` done) . fst) (order model))

less :: World -> KripkeModel -> Set World
less w model = greater w (model { order = Set.map Tuple.swap $ order model })

derivable :: Formula -> KripkeModel -> Bool
derivable = undefined

decomposeFormula :: Formula -> Set Formula
decomposeFormula (Disjunction _ []) = Set.empty  -- actually not possible because of parser
decomposeFormula (Disjunction _ (hds : tds)) =
  Set.unions (fst (List.foldl foldDisjunctionFunc (Set.empty, hds) tds)
             : List.map decomposeFormula (hds : tds))
  where
    foldDisjunctionFunc (container, lhs) rhs =
      (Set.insert foldedDisjunct container, foldedDisjunct)
      where
        foldedDisjunct = Disjunction (Formula.length lhs + Formula.length rhs + 1) [lhs, rhs]

decomposeFormula (Conjunction _ []) = Set.empty  -- actually not possible because of parser
decomposeFormula (Conjunction _ (hcs : tcs)) =
  Set.unions (fst (List.foldl foldConjunctionFunc (Set.empty, hcs) tcs)
             : List.map decomposeFormula (hcs : tcs))
  where
    foldConjunctionFunc (container, lhs) rhs =
      (Set.insert foldedConjunct container, foldedConjunct)
      where
        foldedConjunct = Conjunction (Formula.length lhs + Formula.length rhs + 1) [lhs, rhs]

decomposeFormula (Implication _ []) = Set.empty  -- actually not possible because of parser
decomposeFormula (Implication _ is) =
  Set.unions (fst (List.foldr foldImplicationFunc (Set.empty, Nothing) is)
             : List.map decomposeFormula is)
  where
    foldImplicationFunc lhs (container, Nothing) = (container, Just lhs)
    foldImplicationFunc lhs (container, Just rhs) =
      (Set.insert foldedImplication container, Just foldedImplication)
      where
        foldedImplication = Implication (Formula.length lhs + Formula.length rhs + 1) [lhs, rhs]

decomposeFormula (Negation l f) = Set.insert (Negation l f) (decomposeFormula f)
decomposeFormula (Variable l a) = Set.singleton (Variable l a)
decomposeFormula (Absurdity l) = Set.singleton (Absurdity l)
