module Minilang.Eval where

import           Control.Applicative ((<|>))
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import           Minilang.Parser

type Name = Text

data Env = EmptyEnv
         | ExtendPat Env Binding Value
         | ExtendDecl Env Binding AST AST
         | ExtendRDecl Env Binding AST AST
  deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = EmptyEnv

extend :: Decl -> Env -> Env
extend (Decl b a m) e  = ExtendDecl e b a m
extend (RDecl b a m) e = ExtendRDecl e b a m

-- should probably be possible to have a single AST structure
-- shared by all stages and indexed with a result type, so that
-- we can add whatever specialised information we need
data Value = EU
           | EUnit
           | EI Integer
           | ED Double
           | ENeut Neutral
           | EAbs FunClos
           | ECtor Name Value
           | EPi Value FunClos
           | ESig Value FunClos
           | EPair Value Value
           | ESum SumClos
           | ECase SumClos
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
eval (Def d m)     ρ = eval m (extend d ρ)
eval e             ρ = error $ "don't know how to evaluate " ++ show e ++ " in  " ++ show ρ


app
  :: Value -> Value -> Value
app (EAbs f@Cl{})     v          = inst f v
app c@(ECase (cs,ρ)) (ECtor n v) = app (eval m ρ) v
  where
    Choice _ m = maybe (error $ "invalid constructor " ++ show n ++ " in case " ++ show c) id $ choose cs n
app (ECase s)        (ENeut k)   = ENeut $ NCase s k
app (ENeut k)        v           = ENeut $ NAp k v
app l r             = error $ "don't know how to apply " ++ show l ++ " to "++ show r

inst
  :: FunClos -> Value -> Value
inst (Cl b e ρ)   v = eval e (ExtendPat ρ b v)
inst (ClComp f c) v = inst f (ECtor c v)

p1
  :: Value -> Value
p1 (ENeut k)   = ENeut $ NP1 k
p1 (EPair x _) = x
p1 v           = error $ "don't know how to apply first projection to value " ++ show v

p2
  :: Value -> Value
p2 (ENeut k)   = ENeut $ NP2 k
p2 (EPair _ y) = y
p2 v           = error $ "don't know how to apply first projection to value " ++ show v

rho
  :: Env -> Name -> Value
rho EmptyEnv x = error $ "name " ++ show x ++ " is not defined in empty environment"
rho (ExtendPat ρ b v) x
  | x `inPat` b = proj x b v
  | otherwise   = rho ρ x
rho (ExtendDecl ρ b _a m) x
  | x `inPat` b = proj x b (eval m ρ)
  | otherwise   = rho ρ x
rho ρ'@(ExtendRDecl ρ b _a m) x
  | x `inPat` b = proj x b (eval m ρ')
  | otherwise   = rho ρ x

inPat
  :: Name -> Binding -> Bool
inPat x (B p')
  | x == p'                     = True
inPat x (Pat p p')
  | x `inPat` p' || x `inPat` p = True
inPat _ _                       = False

proj
  :: Name -> Binding -> Value -> Value
proj nam bnd val = fromJust $ proj' nam bnd val
  where
    proj' n (B n')      v
      | n == n'                     = Just v
      | otherwise                   = Nothing
    proj' n (Pat b b') (EPair v v') = proj' n b v <|> proj' n b' v'
    proj' _ _ _                     = undefined
