module Minilang.NormalizeSpec where

import           Minilang.Eval
import           Minilang.Normalize
import           Minilang.Parser
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Normalizer" $ do

  it "normalizes constants to constants" $ do
    normalize 0 EUnit `shouldBe` NUnit
    normalize 0 EU `shouldBe` NU
    normalize 0 (EI 12) `shouldBe` NI 12
    normalize 0 (ED 12) `shouldBe` ND 12

  it "normalizes constructor" $ do
    normalize 0 (ECtor "foo" EUnit)
      `shouldBe` NCtor "foo" NUnit

  it "normalizes pairs" $ do
    normalize 0 (EPair EUnit (EI 12))
      `shouldBe` NPair NUnit (NI 12)

  it "normalizes abstraction" $ do
    normalize 0 (EAbs (Cl (B "x") (Var "x") EmptyEnv))
      `shouldBe` NAbs (NVar 0) (NNeut (NNV (NVar 0)))
