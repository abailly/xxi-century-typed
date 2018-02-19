module Minilang.Eval where

import           Data.Text       (Text)
import           Minilang.Parser

type Name = Text

data Env = EmptyEnv
         | ExtendPat Env Binding Value
  deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = EmptyEnv

data Value = EI Integer | ED Double
           | EU | EUnit
           | EPair Value Value
           | ECtor Name Value
           | EAbs FunClos
           | EPi Value FunClos
           | ESig Value FunClos
           | ENeut Neutral
           | ECase SumClos
           | ESum SumClos
  deriving (Eq, Show)

type SumClos = ( [ Choice ], Env)

data FunClos = Cl Binding AST Env | ClComp FunClos Name
  deriving (Eq, Show)

data Neutral = Gen Int
             | NP1 Neutral
             | NP2 Neutral
             | NCase SumClos Neutral
             | NAp Neutral Value
  deriving (Eq, Show)

eval
  :: AST -> Env -> Value
eval (I n)         _ = EI n
eval (D d)         _ = ED d
eval U             _ = EU
eval Unit          _ = EUnit
eval (Pair a b)    ρ = EPair (eval a ρ) (eval b ρ)
eval (Abs p e)     ρ = EAbs $ Cl p e ρ
eval (Pi p t e)    ρ = EPi (eval t ρ) $ Cl p e ρ
eval (Sigma p t e) ρ = ESig (eval t ρ) $ Cl p e ρ
eval (Ap u v)      ρ = app (eval u ρ) (eval v ρ)
eval (Var x)       ρ = rho ρ x
eval (P1 e)        ρ = p1 (eval e ρ)
eval (P2 e)        ρ = p2 (eval e ρ)
eval (Case cs)     ρ = ECase (cs,ρ)
eval (Sum cs)      ρ = ESum (cs,ρ)
eval (Ctor n e)    ρ = ECtor n (eval e ρ)
eval e             ρ = error $ "don't know how to evaluate " ++ show e ++ " in  " ++ show ρ


app :: Value -> Value -> Value
app (EAbs f@Cl{})     v          = inst f v
app c@(ECase (cs,ρ)) (ECtor n v) = app (eval m ρ) v
  where
    Choice _ m = maybe (error $ "invalid constructor " ++ show n ++ " in case " ++ show c) id $ choose cs n
app (ECase s)        (ENeut k)   = ENeut $ NCase s k
app (ENeut k)        v           = ENeut $ NAp k v
app l r             = error $ "don't know how to apply " ++ show l ++ " to "++ show r

inst :: FunClos -> Value -> Value
inst (Cl b e ρ)   v = eval e (ExtendPat ρ b v)
inst (ClComp f c) v = inst f (ECtor c v)

p1 :: Value -> Value
p1 (ENeut k)   = ENeut $ NP1 k
p1 (EPair x _) = x
p1 v           = error $ "don't know how to apply first projection to value " ++ show v

p2 :: Value -> Value
p2 (ENeut k)   = ENeut $ NP2 k
p2 (EPair _ y) = y
p2 v           = error $ "don't know how to apply first projection to value " ++ show v

rho :: Env -> Name -> Value
rho EmptyEnv x = error $ "name " ++ show x ++ " is not defined in empty environment"
rho (ExtendPat ρ b v) x
  | x `inPat` b = proj x b v
  | otherwise   = rho ρ x

inPat :: Name -> Binding -> Bool
inPat x (B p') | x == p' = True
inPat _ _      = False

proj :: Name -> Binding -> Value -> Value
proj _ (B _) v = v
proj _ _ _     = undefined
