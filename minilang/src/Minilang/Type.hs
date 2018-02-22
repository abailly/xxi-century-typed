module Minilang.Type where

import           Data.Monoid        ((<>))
import           Minilang.Eval
import           Minilang.Normalize
import           Minilang.Parser

-- * Typing Context

data Context = EmptyContext
             | Context Context Name Value
  deriving (Eq,Show)

lookupType :: Name -> Context -> Value
lookupType x (Context γ x' t)
  | x == x'               = t
  | otherwise             = lookupType x γ
lookupType x EmptyContext = error $ "cannot find " <> show x <> " in empty context"


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
  :: Int -> Decl -> Env -> Context -> Context
checkD l (Decl p a m) ρ γ =
  if checkT l a ρ γ && check l m t ρ γ
  then bindType p t (eval m ρ) γ
  else error $ show a <> " is not a valid type for " <> show p <> " with value "  <> show m <> " in env " <> show ρ <> " and context " <> show γ
  where
    t = eval a ρ

checkD l d@(RDecl p a m) ρ γ =
  if checkT l a ρ γ && check (l+1) m t ρ_1 γ_1
  then bindType p t v γ
  else error $ show a <> " is not a valid type for " <> show p <> " with value "  <> show m <> " in env " <> show ρ <> " and context " <> show γ
  where
    x_l = ENeut $ NV $ NVar l
    t   = eval a ρ
    γ_1 = bindType p t x_l γ
    ρ_1 = ExtendPat ρ p x_l
    v   = eval m (ExtendDecl ρ d)

-- ** Check Type Well-Formedness

checkT
  :: Int -> AST -> Env -> Context -> Bool
checkT l (Sigma p a b) ρ γ = checkDependentT l p a b ρ γ
checkT l (Pi p a b)    ρ γ = checkDependentT l p a b ρ γ
checkT _ U             _ _ = True
checkT l a             ρ γ = check l a EU ρ γ

checkDependentT
  :: Int -> Binding -> AST -> AST -> Env -> Context -> Bool
checkDependentT l p a b ρ γ = checkT l a ρ γ && checkT (l+1) b ρ_1 γ_1
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l
    γ_1 = bindType p (eval a ρ) x_l γ

-- ** Check Type assignment

check
  :: Int -> AST -> Value -> Env -> Context -> Bool
check l (Ctor c_i m) (ESum (c, ν)) ρ γ =
  case choose c c_i of
    Nothing             -> error $ "invalid ctor " <> show c_i <> " among " <> show c <> " while typing"
    Just (Choice _ a_i) -> check l m (eval a_i ν) ρ γ
check l (Pair m n)   (ESig t g)    ρ γ =
  check l m t ρ γ && check l n (inst g (eval m ρ)) ρ γ
check _ Unit     EUnit _  _ = True
check _ Unit     EU    _  _ = True
check l (Case cs) (EPi (ESum (cs', ν)) g) ρ γ =
  length cs == length cs' &&
  all (\ (Choice c_i m_i,Choice c_i' a_i) -> c_i == c_i' && check l m_i (EPi (eval a_i ν) (ClComp g c_i)) ρ γ) (zip cs cs')
check l (Sum cs) EU    ρ  γ =
  all (\ (Choice _ a) -> check l a EU ρ γ) cs
check l (Sigma p a b) EU ρ  γ =
  check l a EU  ρ γ  && check (l+1) b EU ρ_1 γ_1
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l
    γ_1 = bindType p (eval a ρ) x_l γ
check l (Pi p a b) EU ρ  γ =
  check l a EU  ρ γ  && check (l+1) b EU ρ_1 γ_1
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l
    γ_1 = bindType p (eval a ρ) x_l γ
check l (Abs p m) (EPi t g) ρ γ =
  check (l+1) m (inst g x_l) ρ_1 γ_1
  where
    x_l = ENeut $ NV $ NVar l
    ρ_1 = ExtendPat ρ p x_l
    γ_1 = bindType p t x_l γ
check l m t ρ γ =
  if normalize l t == (normalize l t' :: Normal)
  then True
  else error $ "[" <> show l <> "] expr " <> show m <> " does not have type "<> show t <> " in env " <> show ρ <> " and context " <> show γ
  where
    t' = checkI l m ρ γ


-- ** Inter Type of an Expression

checkI
  :: Int -> AST -> Env -> Context -> Value
checkI _ (Var x)  _ γ = lookupType x γ
checkI l (Ap m n) ρ γ =
  if   check l n t ρ γ
  then inst g (eval n ρ)
  else error $  "[" <> show l <> "] type of " <> show n <> " is not " <> show t <> " in env " <> show ρ <> " and context " <> show γ
  where
    EPi t g = checkI l m ρ γ
checkI l e ρ γ = error $  "[" <> show l <> "] cannot infer type of " <> show e <> " in env " <> show ρ <> " and context " <> show γ
