module Minilang.STG.Lang where

import           Data.Text (Text)

type Prog = [ Bind ]

data Bind = Bind Var Lambda
  deriving (Eq, Show)

type Binds = [ Bind ]

data Var = Var Text
  deriving (Eq, Show)

data Lambda = Lambda [ Var ] UpdateFlag [ Var ] Expr
  deriving (Eq, Show)

data UpdateFlag = Updatable | NotUpdatable
  deriving (Eq, Show)

data Expr = Let Binds Expr
          | Letrec Binds Expr
          | Case Expr Alts
          | Ap Var Atoms
          | Ctor Var Atoms
          | Prim PrimOp Atoms
          | Lit Literal
  deriving (Eq, Show)

data Alts = AAlts [ AlgAlt ] Default
          | PAlts [ PrimAlt ] Default
  deriving (Eq, Show)


data AlgAlt = AlgAlt Var Vars Expr
  deriving (Eq, Show)

data PrimAlt = PrimAlt Literal Expr
  deriving (Eq, Show)

data Default = DefBind Var Expr
             | Default Expr
  deriving (Eq, Show)

newtype Literal = Literal Int
  deriving (Eq,Show)

newtype PrimOp = PrimOp Text
  deriving (Eq, Show)


type Vars = [ Var ]

type Atoms = [ Atom ]

data Atom = V Var | L Literal
  deriving (Eq, Show)
