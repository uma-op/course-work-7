module Formula where

type Atom = String
data Formula = Disjunction ![Formula]
             | Conjunction ![Formula]
             | Implication ![Formula]
             | Negation !Formula
             | Variable !Atom
             | Absurdity
             deriving Show

