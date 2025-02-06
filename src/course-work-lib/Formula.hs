module Formula where

import qualified Data.Function as Function
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

type Atom = String

data FormulaType
  = DisjunctionType
  | ConjunctionType
  | ImplicationType
  | NegationType
  | VariableType
  deriving (Eq)

data Formula
  = Disjunction {length :: !Int, lhs :: !Formula, rhs :: !Formula}
  | Conjunction {length :: !Int, lhs :: !Formula, rhs :: !Formula}
  | Implication {length :: !Int, lhs :: !Formula, rhs :: !Formula}
  | Negation {length :: !Int, operand :: !Formula}
  | Variable {length :: !Int, atom :: !Atom}
  deriving (Eq)

disjunction :: [Formula] -> Formula
disjunction [] = error "Empty list of disjuncts"
disjunction [d] = d
disjunction (dh : dt) = adjustLength $ List.foldl (Disjunction 0) dh dt

conjunction :: [Formula] -> Formula
conjunction [] = error "Empty list of conjuncts"
conjunction [c] = c
conjunction (ch : ct) = adjustLength $ List.foldl (Conjunction 0) ch ct

implication :: [Formula] -> Formula
implication [] = error "Empty list of implication operands"
implication [i] = i
implication is = adjustLength $ List.foldr (Implication 0) initial toFold
  where
    reversed = reverse is
    initial = head reversed
    toFold = reverse $ tail reversed

adjustLength :: Formula -> Formula
adjustLength f
  | formulaType == DisjunctionType || formulaType == ConjunctionType || formulaType == ImplicationType =
      let adjustedLhs = adjustLength $ lhs f
          adjustedRhs = adjustLength $ rhs f
       in f
            { Formula.length = Formula.length adjustedLhs + Formula.length adjustedRhs + 1,
              Formula.lhs = adjustedLhs,
              Formula.rhs = adjustedRhs
            }
  | formulaType == NegationType =
      let adjustedOperand = adjustLength $ operand f
       in f
            { Formula.length = Formula.length adjustedOperand + 1,
              Formula.operand = adjustedOperand
            }
  | otherwise = f {Formula.length = 0}
  where
    formulaType = getFormulaType f

negation :: Formula -> Formula
negation f =
  Negation
    { Formula.length = Formula.length f + 1,
      Formula.operand = f
    }

variable :: Atom -> Formula
variable a =
  Variable
    { Formula.length = 0,
      Formula.atom = a
    }

absurdity :: Formula
absurdity = variable "_|_"

getFormulaType :: Formula -> FormulaType
getFormulaType Disjunction {} = DisjunctionType
getFormulaType Conjunction {} = ConjunctionType
getFormulaType Implication {} = ImplicationType
getFormulaType Negation {} = NegationType
getFormulaType Variable {} = VariableType

instance Ord Formula where
  compare f1 f2
    | Formula.length f1 == Formula.length f2 = Function.on compare show f1 f2
    | otherwise = Function.on compare Formula.length f1 f2

instance Show Formula where
  show (Disjunction _ lhs rhs) = "(" ++ show lhs ++ " \\/ " ++ show rhs ++ ")"
  show (Conjunction _ lhs rhs) = "(" ++ show lhs ++ " /\\ " ++ show rhs ++ ")"
  show (Implication _ lhs rhs) = "(" ++ show lhs ++ " -> " ++ show rhs ++ ")"
  show (Negation _ f) = "-" ++ show f
  show (Variable _ a) = a


atoms :: Formula -> Set Atom
atoms formula = atoms' [formula] Set.empty
  where
    atoms' [] result = result
    atoms' (h : t) result =
      case h of
        Variable _ a -> atoms' t $ Set.insert a result
        Negation _ f -> atoms' (f : t) result
        _other -> atoms' (lhs h : rhs h : t) result
