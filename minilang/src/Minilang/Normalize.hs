module Minilang.Normalize where

import           Data.Monoid     ((<>))
import           Data.Text
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

newtype NVar = NVar Text
  deriving (Eq, Show)

data NNeutral = NV NVar
              | NNAp NNeutral Normal
              | NNPi1 NNeutral
              | NNPi2 NNeutral
              | NNCase SumClos NNeutral
  deriving (Eq,Show)

data NEnv = NEmptyEnv
          | NExtendEnv NEnv Binding Normal
          | NExtendDecl NEnv Decl
  deriving (Eq, Show)

normalize :: Value -> Normal
normalize EUnit = NUnit
normalize EU    = NU
normalize v     = error $ "don't know how to normalize " <> show v
