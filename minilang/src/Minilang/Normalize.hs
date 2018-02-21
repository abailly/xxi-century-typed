module Minilang.Normalize where

import           Data.Monoid     ((<>))
import           Minilang.Eval
import           Minilang.Parser

data Normal = NAbs NVar Normal
            | NPi NVar Normal Normal
            | NU
            | NNeut NNeutral
            | NPair Normal Normal
            | NSig NVar Normal Normal
            | NUnit
            | NI Integer
            | ND Double
            | NCtor Name Normal
            | NSum SumClos
            | NCase SumClos
  deriving (Eq, Show)

data NNeutral = NNV NVar
              | NNAp NNeutral Normal
              | NNPi1 NNeutral
              | NNPi2 NNeutral
              | NNCase SumClos NNeutral
  deriving (Eq,Show)

data NEnv = NEmptyEnv
          | NExtendEnv NEnv Binding Normal
          | NExtendDecl NEnv Decl
  deriving (Eq, Show)

class Normalize val norm where
  normalize :: Int -> val -> norm

instance Normalize Value Normal where
  normalize _ EUnit       = NUnit
  normalize _ EU          = NU
  normalize _ (EI i)      = NI i
  normalize _ (ED d)      = ND d
  normalize n (ECtor c e) = NCtor c (normalize n e)
  normalize n (EPair u v) = NPair (normalize n u) (normalize n v)
  normalize n (EAbs clos) = NAbs x_n (normalize (n+1) $ inst clos (ENeut $ NV x_n))
    where
      x_n = NVar n
  normalize n (ENeut k)   = NNeut (normalize n k)
  normalize n v     = error $ "don't know how to normalize_" <> show n <> " value " <> show v

instance Normalize Neutral NNeutral where
  normalize _ (NV x) = NNV x
  normalize n v      = error $ "don't know how to normalize_" <> show n <> " neutral " <> show v
