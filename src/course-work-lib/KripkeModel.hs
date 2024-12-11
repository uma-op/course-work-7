module KripkeModel where

import Formula ( Atom, Formula(..), paren )

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
decomposeFormula f = decomposeFormula' $ paren f
  where
    decomposeFormula' :: Formula -> Set Formula
    decomposeFormula' (Negation l n) = Set.insert (Negation l n) (decomposeFormula' n)
    decomposeFormula' (Variable l a) = Set.singleton (Variable l a)
    decomposeFormula' (Absurdity l) = Set.singleton (Absurdity l)
    decomposeFormula' p = Set.insert p $ Set.unions $ List.map decomposeFormula' $ operands p
    
