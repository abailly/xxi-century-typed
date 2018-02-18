module Minilang.EvalSpec where

import           Minilang.Eval
import           Minilang.Parser
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Expressions Evaluator" $ do

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

  it "evaluates product type" $ do
    eval (Pi (B "foo") U (Var "bar")) emptyEnv
      `shouldBe` EPi EU (Cl (B "foo") (Var "bar") emptyEnv)

  it "evaluates sum type" $ do
    eval (Sigma (B "foo") U (Var "bar")) emptyEnv
      `shouldBe` ESig EU (Cl (B "foo") (Var "bar") emptyEnv)

  it "evaluates projections" $ do
    let extended = ExtendPat (ExtendPat emptyEnv (B "x") (ENeut $ Gen 1))
                   (B "y") (ENeut $ Gen 2)

    eval (P1 (Pair (Var "x") (Var "y"))) extended
      `shouldBe` ENeut (Gen 1)

    eval (P2 (Pair (Var "x") (Var "y"))) extended
      `shouldBe` ENeut (Gen 2)

  it "evaluates Application of a function" $ do
    eval (Ap (Abs (B "x") (Var "x")) (I 12)) emptyEnv
      `shouldBe` EI 12
