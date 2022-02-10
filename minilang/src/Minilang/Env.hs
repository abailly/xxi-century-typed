{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minilang.Env where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Minilang.Parser

type Name = Text

-- ** Evaluation Environment

data Env' value
  = EmptyEnv
  | ExtendPat (Env' value) Binding value
  | ExtendDecl (Env' value) Decl
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

emptyEnv :: Env' value
emptyEnv = EmptyEnv

extend :: Decl -> Env' value -> Env' value
extend = flip ExtendDecl
