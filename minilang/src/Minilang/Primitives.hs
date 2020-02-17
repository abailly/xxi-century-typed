{-# LANGUAGE DeriveAnyClass #-}
module Minilang.Primitives where

import           Data.Aeson
import           GHC.Generics

data PrimType = PrimInt
    | PrimDouble
    | PrimString
    deriving (Eq, Show, Generic, ToJSON, FromJSON)
