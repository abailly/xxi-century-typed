{-# LANGUAGE MultiParamTypeClasses #-}
module Minilang.Normalize where

import           Minilang.Env
import           Minilang.Eval
import           Minilang.Parser
import           Minilang.Primitives

type NEnv = Env' Normal

data Normal = NU
            | NUnit
            | NOne
            | NPrim PrimType
            | NI Integer
            | ND Double
            | NS String
            | NNeut NNeutral
            | NAbs NVar Normal
            | NCtor Name (Maybe Normal)
            | NPi NVar Normal Normal
            | NSig NVar Normal Normal
            | NPair Normal Normal
            | NSum NSumClos
            | NFun NCaseClos
  deriving (Eq, Show)

type NSumClos = ( [ Choice ], NEnv)

type NCaseClos = ( [ Clause ], NEnv)

data NNeutral = NNV NVar
              | NNAp NNeutral Normal
              | NNPi1 NNeutral
              | NNPi2 NNeutral
              | NNCase NCaseClos NNeutral
  deriving (Eq,Show)


class Normalize val norm where
  normalize :: Int -> val -> norm

instance Normalize Value Normal where
  normalize _ EUnit       = NUnit
  normalize _ EOne        = NOne
  normalize _ EU          = NU
  normalize _ (EI i)      = NI i
  normalize _ (ED d)      = ND d
  normalize _ (ES s)      = NS s
  normalize _ (EPrim p)   = NPrim p
  normalize n (ECtor c e) = NCtor c (normalize n <$> e)
  normalize n (EPair u v) = NPair (normalize n u) (normalize n v)
  normalize n (EAbs clos) = NAbs x_n (normalize (n+1) $ inst clos (ENeut $ NV x_n))
    where
      x_n = NVar n
  normalize n (ENeut k)   = NNeut (normalize n k)
  normalize n (EPi t g)   = NPi x_n (normalize n t) (normalize (n+1) $ inst g (ENeut $ NV x_n))
    where
      x_n = NVar n
  normalize n (ESig t g)   = NSig x_n (normalize n t) (normalize (n+1) $ inst g (ENeut $ NV x_n))
    where
      x_n = NVar n
  normalize n (ESum (s,ρ)) = NSum (s, normalize n ρ)
  normalize n (ECase(s,ρ)) = NFun (s, normalize n ρ)

instance Normalize Neutral NNeutral where
  normalize _ (NV x)          = NNV x
  normalize n (NAp k v)       = NNAp (normalize n k) (normalize n v)
  normalize n (NP1 k)         = NNPi1 (normalize n k)
  normalize n (NP2 k)         = NNPi2 (normalize n k)
  normalize n (NCase (s,ρ) k) = NNCase (s, normalize n ρ) (normalize n k)

instance Normalize Env NEnv where
  normalize _ EmptyEnv          = EmptyEnv
  normalize n (ExtendPat ρ p v) = ExtendPat (normalize n ρ) p (normalize n v)
  normalize n (ExtendDecl ρ d)  = ExtendDecl (normalize n ρ) d
