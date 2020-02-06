module Tartlet.Expr

import Env

%access public export
%default covering

data Expr : Type where
  Var : Name -> Expr

  -- * Typing expressions
  Pi : Name -> Expr -> Expr -> Expr
  Sigma : Name -> Expr -> Expr -> Expr

  ||| Primitive type of Natural numbers
  Nat : Expr

  -- * Value-level expressions
  Lam : Name -> Expr -> Expr
  App : Expr -> Expr -> Expr

  ||| Induction/recursion over natural numbers
  IndNat : Expr -> Expr -> Expr -> Expr -> Expr

  ||| ???
  Equal : Expr -> Expr -> Expr -> Expr

  ||| Equivalent of `Refl` a value denoting equality
  ||| at type level.
  Same : Expr

  ||| Use some equality to replace values within expressions
  Replace : Expr -> Expr -> Expr -> Expr

  ||| Unit type, called `One` in minilang
  Trivial : Expr

  ||| Unit constructor, called `Unit` in minilang
  Sole : Expr

  ||| `Void` type, the type without any inhabitant
  Absurd : Expr

  IndAbsurd : Expr -> Expr -> Expr

  ||| Primitive type of atoms
  Atom : Expr

  ||| Atom values
  Tick : String -> Expr

  ||| The Universe of types
  U : Expr

  ||| Type ascription operator
  The : Expr -> Expr -> Expr

  ||| Pairing expressions
  Pair : Expr -> Expr -> Expr
  ||| Projection of a pair on its first element
  P1 : Expr -> Expr
  ||| Projection of a pair on its second element
  P2 : Expr -> Expr

  ||| Zero
  Z : Expr

  ||| Successor
  S : Expr -> Expr


Show Expr where
  show (Var name) = name
  show (Pi name ty body) = "Pi (" ++ name ++ " : " ++ show ty ++ "). " ++ show body
  show (Sigma name ty body) = "Sigma (" ++ name ++ " : " ++ show ty ++ "). " ++ show body
  show Nat = "Nat"
  show (Lam name body) = "λ " ++ name ++ "." ++ show body
  show (App l r) = "(" ++ show l ++ " " ++ show r ++ ")"
  show (IndNat tgt mot base step) = "IntNat " ++ show tgt ++ " " ++ show mot ++ " " ++ show base ++ " " ++ show step
  show (Equal ty from to) = "(= " ++ show ty ++ " " ++ show from ++ " " ++ show to ++ ")"
  show Same = "Same"
  show (Replace tgt mot base) = "Replace " ++ show tgt ++ " " ++ show mot ++ " " ++ show base
  show Trivial = "Trivial"
  show Sole = "Sole"
  show Absurd = "Abzurd"
  show (IndAbsurd tgt mot) = "IndAbsurd " ++ show tgt ++ " " ++ show mot
  show Atom = "Atom"
  show (Tick sym) = "'" ++ sym
  show U = "U"
  show (The ty e) = "The " ++ show ty ++ " " ++ show e
  show (Pair a b) = "(" ++ show a ++ "," ++ show b ++ ")"
  show (P1 e) = "(P1 " ++ show e ++ ")"
  show (P2 e) = "(P2 " ++ show e ++ ")"
  show Z = "Z"
  show (S e) = "S " ++ show e
