{-# LANGUAGE MultiParamTypeClasses #-}
module Minilang.Env where

import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import           Minilang.Parser

type Name = Text

-- ** Evaluation Environment

data Env' value = EmptyEnv
         | ExtendPat (Env' value) Binding value
         | ExtendDecl (Env' value) Decl
  deriving (Eq)

instance (Show value) => Show (Env' value) where
  show e = "{ " <> show' e <> " }"
    where
      show' EmptyEnv          = "∅"
      show' (ExtendPat ρ b v) = show b  <> " ↦ " <> show v <> ", " <> show' ρ
      show' (ExtendDecl ρ (Decl b t m))  = show b  <> " : " <> show t <> " ↦ " <> show m <> ", " <> show' ρ
      show' (ExtendDecl ρ (RDecl b t m))  = show b  <> " : " <> show t <> " ↦ " <> show m <> ", " <> show' ρ

emptyEnv :: Env' value
emptyEnv = EmptyEnv

extend :: Decl -> Env' value -> Env' value
extend = flip ExtendDecl
