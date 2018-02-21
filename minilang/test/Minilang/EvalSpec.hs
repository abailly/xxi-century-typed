module Minilang.EvalSpec where

import           Control.Exception
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

  it "evaluates Application of a choice to a unary ctor" $ do
    eval (Ap (Case [ Choice "A" (Abs (B "x") (Var "x"))
                   , Choice "B" (Abs Wildcard (D 13))
                   ])
           (Ctor "A" (D 14))) emptyEnv
      `shouldBe` ED 14

  it "raises error when evaluates Application of a choice without matching ctor" $ do
    evaluate (eval (Ap (Case [ Choice "A" (Abs (B "x") (Var "x"))
                             , Choice "B" (Abs Wildcard (D 13))
                             ])
                     (Ctor "C" (D 14))) emptyEnv)
      `shouldThrow` anyException

  it "evaluates application of choice to neutral value as neutral" $ do
    let extended = ExtendPat emptyEnv (B "x") (ENeut $ Gen 1)
    eval (Ap (Case [ Choice "A" (Abs (B "x") (Var "x")) ])
           (Var "x")) extended
      `shouldBe` ENeut (NCase ([ Choice "A" (Abs (B "x") (Var "x")) ], extended) (Gen 1))

  it "evaluates application of neutral to value as neutral" $ do
    let extended = ExtendPat emptyEnv (B "x") (ENeut $ Gen 1)
    eval (Ap (Var "x") (I 12)) extended
      `shouldBe` ENeut (NAp (Gen 1) (EI 12))

  it "evaluates Constructor expression" $ do
    eval (Ctor "foo" (I 12)) emptyEnv
      `shouldBe` ECtor "foo" (EI 12)

  it "evaluates case match" $ do
    let extended = ExtendPat (ExtendPat emptyEnv (B "x") (ENeut $ Gen 1))
                   (B "y") (ENeut $ Gen 2)

    eval (Case [ Choice "foo" (Abs (C $ I 12) (Var "x"))
               , Choice "bar" (Abs (B "z") (Ap (Var "y") (Var "z")))
               ])
      extended `shouldBe` ECase ([ Choice "foo" (Abs (C $ I 12) (Var "x"))
                                 , Choice "bar" (Abs (B "z") (Ap (Var "y") (Var "z")))
                                 ]
                                , extended)

  it "evaluates Sum definition" $ do
    let extended = ExtendPat (ExtendPat emptyEnv (B "x") (ENeut $ Gen 1))
                   (B "y") (ENeut $ Gen 2)

    eval (Sum [ Choice "true" Unit, Choice "false" Unit])
      extended `shouldBe` ESum ([ Choice "true" Unit, Choice "false" Unit]
                                , extended)

  it "evaluates declaration continuation in extended env" $ do
    eval (Def (Decl (B "id")
               (Pi  (B "A") U (Pi Wildcard (Var "A") (Var "A")))
               (Abs (B "A") (Abs (B "x") (Var "x"))))
           (Ap (Ap (Var "id") U) (I 12))) emptyEnv
      `shouldBe`
           EI 12
