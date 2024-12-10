module Formula where

import qualified Data.Function as Function
import qualified Data.List as List

type Atom = String
data Formula = Disjunction { length :: !Int, operands :: ![Formula] }
             | Conjunction { length :: !Int, operands :: ![Formula] }
             | Implication { length :: !Int, operands :: ![Formula] }
             | Negation { length :: !Int, operand :: !Formula }
             | Variable { length :: !Int, atom :: !Atom }
             | Absurdity { length :: !Int }
             deriving Eq

disjunction :: [Formula] -> Formula
disjunction ds = Disjunction (List.length ds - 1 + List.sum (List.map Formula.length ds)) ds

conjunction :: [Formula] -> Formula
conjunction cs = Conjunction (List.length cs - 1 + List.sum (List.map Formula.length cs)) cs

implication :: [Formula] -> Formula
implication is = Implication (List.length is - 1 + List.sum (List.map Formula.length is)) is

negation :: Formula -> Formula
negation f = Negation (Formula.length f + 1) f

variable :: Atom -> Formula
variable = Variable 0

absurdity :: Formula
absurdity = Absurdity 0

instance Ord Formula where
  compare f1 f2 | Formula.length f1 == Formula.length f2 = Function.on compare show f1 f2
                | otherwise = Function.on compare Formula.length f1 f2

instance Show Formula where
  show (Disjunction _ (hfs : tfs)) = "(" ++ show hfs ++ concatMap (\e -> " \\/ " ++ show e) tfs ++ ")"
  show (Conjunction _ (hfs : tfs)) = "(" ++ show hfs ++ concatMap (\e -> " /\\ " ++ show e) tfs ++ ")"
  show (Implication _ (hfs : tfs)) = "(" ++ show hfs ++ concatMap (\e -> " -> " ++ show e) tfs ++ ")"
  show (Negation _ f) = "-" ++ show f
  show (Variable _ a) = a
  show (Absurdity _) = "_|_"

