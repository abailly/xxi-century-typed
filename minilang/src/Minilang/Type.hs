module Minilang.Type where

import           Data.Monoid     ((<>))
import           Minilang.Eval
import           Minilang.Parser

-- * Typing Context

data Context = EmptyContext
             | Context Context Name Value
  deriving (Eq,Show)

lookupType :: Name -> Context -> Value
lookupType x (Context γ x' t)
  | x == x'               = t
  | otherwise             = lookupType x γ
lookupType x EmptyContext = error $ "cannot find " <> show x <> " in typing context"


bindType :: Binding -> Value -> Value -> Context -> Context
bindType (B x)     t          _ γ = Context γ x t
bindType Wildcard  _          _ γ = γ
bindType (Pat x y) (ESig t g) v γ =
  let
    γ1 = bindType x t (p1 v) γ
    γ2 = bindType y (inst g (p1 v)) (p2 v) γ1
  in
    γ2
bindType p     t v  γ             = error $ "don't know how to bind " <> show p <> " to type " <> show t <> " and value " <> show v <> " in context "<> show γ

-- * Typing Judgments

-- ** Check Declaration Correctness

checkD
  :: Binding -> AST -> AST -> Env -> Context -> Context
checkD p a m ρ γ =
  if checkT a ρ γ && check m t ρ γ
  then bindType p t (eval m ρ) γ
  else error $ show a <> " is not a valid type for " <> show p <> " with value "  <> show m <> " in env " <> show ρ <> " and context " <> show γ
  where
    t = eval a ρ

-- ** Check Type Well-Formedness

checkT
  :: AST -> Env -> Context -> Bool
checkT _ _ _ = True -- a is assumed to be of type U

-- ** Check Type assignment

check
  :: AST -> Value -> Env -> Context -> Bool
check (Ctor c_i m) (ESum (c, ν)) ρ γ =
  case choose c c_i of
    Nothing             -> error $ "invalid ctor " <> show c_i <> " among " <> show c <> " while typing"
    Just (Choice _ a_i) -> check m (eval a_i ν) ρ γ
check Unit EUnit _  _ = True
check m t ρ γ = error $ "expr " <> show m <> " does not have type "<> show t <> " in env " <> show ρ <> " and context " <> show γ
