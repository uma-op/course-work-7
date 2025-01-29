{-# LANGUAGE TupleSections #-}

module KripkeModel where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import Formula (Atom, Formula (..))
import qualified Formula

type World = Int

type Order = Set (World, World)

type Valuation = Map (World, Atom) Bool

data KripkeModel = KripkeModel
  { worlds :: !(Set World),
    order :: !Order,
    valuation :: !Valuation
  } deriving Show

empty :: KripkeModel
empty =
  KripkeModel
    { worlds = Set.empty,
      order = Set.empty,
      valuation = Map.empty
    }

isValuationCorrect :: KripkeModel -> Bool
isValuationCorrect = undefined

related :: World -> World -> KripkeModel -> Bool
related w1 w2 model = related' $ Set.singleton w1
  where
    related' done
      | Set.member w2 done = True
      | otherwise = related' $ Set.map snd $ Set.filter ((`Set.member` done) . fst) (order model)

greater :: World -> KripkeModel -> Set World
greater w model = greater' (Set.singleton w) (Set.singleton w)
  where
    greater' gs done
      | Set.null done = gs
      | Set.null newDone = gs
      | otherwise = greater' (Set.union newDone gs) newDone
      where
        newDone = gs \\ Set.map snd (Set.filter ((`Set.member` done) . fst) (order model))

less :: World -> KripkeModel -> Set World
less w model = greater w (model {order = Set.map Tuple.swap $ order model})

maxWorlds :: KripkeModel -> Set World
maxWorlds model = right Set.\\ left
  where
    separate (cl, cr) (l, r) = (Set.insert l cl, Set.insert r cr)
    (left, right) = Set.foldl separate (Set.empty, Set.empty) (order model)

derivable :: KripkeModel -> Formula -> Bool
derivable model f = List.all ((derivableValue Map.!) . (,f)) (Set.toList $ worlds model)
  where
    decomposed = decomposeFormula f
    derivable' ::
      Map (World, Formula) Bool ->
      Formula ->
      Map (World, Formula) Bool
    derivable' worldMap formula =
      case formula of
        Disjunction _ [f1, f2] ->
          let newWorldMap = getNewWorldMap [f1, f2]
           in Set.foldl (foldingFunction (\w -> (newWorldMap Map.! (w, f1)) || (newWorldMap Map.! (w, f2)))) newWorldMap $ worlds model
        Conjunction _ [f1, f2] ->
          let newWorldMap = getNewWorldMap [f1, f2]
           in Set.foldl (foldingFunction (\w -> (newWorldMap Map.! (w, f1)) && (newWorldMap Map.! (w, f2)))) newWorldMap $ worlds model
        Implication _ [f1, f2] ->
          let newWorldMap = getNewWorldMap [f1, f2]
           in Set.foldl (foldingFunction (\w -> not (newWorldMap Map.! (w, f1)) || (newWorldMap Map.! (w, f2)))) newWorldMap $ worlds model
        Negation _ n ->
          let newWorldMap = getNewWorldMap [n]; derivableValue = List.and $ Set.map (not . (newWorldMap Map.!) . (,n)) $ maxWorlds model
           in Set.foldl (flip $ flip Map.insert derivableValue) newWorldMap $ Set.map (,formula) (worlds model)
        Variable _ _ -> worldMap
        Absurdity _ -> worldMap
      where
        foldingFunction :: (World -> Bool) -> Map (World, Formula) Bool -> World -> Map (World, Formula) Bool
        foldingFunction isDerivableInWorld container element = Map.insert (element, formula) (isDerivableInWorld element) container

        getNewWorldMap :: [Formula] -> Map (World, Formula) Bool
        getNewWorldMap = List.foldl derivable' worldMap

    derivableValue :: Map (World, Formula) Bool
    derivableValue =
      Set.foldl
        derivable'
        ( Map.union
            (Map.fromSet (const False) (Set.map (,Formula.absurdity) (worlds model))) -- Absurdity is not derivable in all worlds
            (Map.mapKeys (Bifunctor.second Formula.variable) (valuation model))
        )
        decomposed

decomposeFormula :: Formula -> Set Formula
decomposeFormula f = decomposeFormula' $ Formula.paren f
  where
    decomposeFormula' :: Formula -> Set Formula
    decomposeFormula' (Negation l n) = Set.insert (Negation l n) (decomposeFormula' n)
    decomposeFormula' (Variable l a) = Set.singleton (Variable l a)
    decomposeFormula' (Absurdity l) = Set.singleton (Absurdity l)
    decomposeFormula' p = Set.insert p $ Set.unions $ List.map decomposeFormula' $ operands p
