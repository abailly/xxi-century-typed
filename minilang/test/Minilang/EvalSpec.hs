module Minilang.EvalSpec where

import           Minilang.Eval
import           Minilang.Parser
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Expressions Evaluator" $ do

  it "evaluates a number as itself" $ do
    let emptyEnv = Env mempty
    eval (I 12) emptyEnv `shouldBe` (EI 12, emptyEnv)
    eval (D 12) emptyEnv `shouldBe` (ED 12, emptyEnv)
