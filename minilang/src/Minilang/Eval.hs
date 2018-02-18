module Minilang.Eval where

import qualified Data.Map        as Map
import           Data.Text       (Text)
import           Minilang.Parser

type Name = Text

newtype Env = Env { unEnv :: Map.Map Name Value }
  deriving (Eq, Show)

data Value = EI Integer | ED Double
           | EU | EUnit
           | EPair Value Value
           | EAbs FunClos
  deriving (Eq, Show)

data FunClos = Cl Binding AST Env
  deriving (Eq, Show)

eval
  :: AST -> Env -> Value
eval (I n)      _env = EI n
eval (D d)      _env = ED d
eval U          _env = EU
eval Unit       _env = EUnit
eval (Pair a b)  env = EPair v w
  where
    v = eval a env
    w = eval b env
eval (Abs p e)  env  = EAbs $ Cl p e env
eval _ _       = undefined
