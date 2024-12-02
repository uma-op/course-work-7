module KripkeModel where

import Formula ( Atom, Formula )

import Data.Map ( Map )
-- import qualified Data.Map as Map

import Data.Set ( Set )
import qualified Data.Set as Set

type World = Int
type Order = Set (World, World)
type Valuation = Map (World, Atom) Bool

data KripkeModel = KripkeModel
  { worlds :: !(Set World)
  , order :: !Order
  , valuation :: !Valuation
  }

related :: World -> World -> KripkeModel -> Bool
related w1 w2 = Set.member (w1, w2) . order

derivable :: Formula -> KripkeModel -> Bool
derivable = undefined
