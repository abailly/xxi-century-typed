module Minilang.EvalSpec where

import           Minilang.Eval
import           Minilang.Parser
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Expressions Evaluator" $ do

  let emptyEnv = Env mempty

  it "evaluates constants to themselves" $ do
    eval (I 12) emptyEnv `shouldBe` EI 12
    eval (D 12) emptyEnv `shouldBe` ED 12
    eval U      emptyEnv `shouldBe` EU
    eval Unit   emptyEnv `shouldBe` EUnit

  it "evaluates pairs to pairs of values" $ do
    eval (Pair (D 12) Unit) emptyEnv `shouldBe` EPair (ED 12) EUnit

  it "evaluates abstraction to a closure" $ do
    eval (Abs (B "foo") (Var "foo")) emptyEnv
      `shouldBe` EAbs (Cl (B "foo") (Var "foo") emptyEnv)
