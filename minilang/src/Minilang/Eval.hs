module Minilang.Eval where

import qualified Data.Map        as Map
import           Data.Text       (Text)
import           Minilang.Parser

type Name = Text

newtype Env = Env { unEnv :: Map.Map Name Value }
  deriving (Eq, Show)

data Value = EI Integer | ED Double
  deriving (Eq, Show, Read)

eval
  :: AST -> Env -> (Value, Env)
eval (I n) env = (EI n, env)
