module Minilang.NormalizeSpec where

import           Minilang.Eval
import           Minilang.Normalize
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Normalizer" $ do

  it "normalizes constants to constants" $ do
    normalize EUnit `shouldBe` NUnit
    normalize EU `shouldBe` NU
