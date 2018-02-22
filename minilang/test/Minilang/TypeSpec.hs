module Minilang.TypeSpec where

import           Control.Exception
import           Minilang.Eval
import           Minilang.Parser
import           Minilang.Type
import           Test.Hspec


spec :: Spec
spec = parallel $ describe "Type Checker" $ do

  describe "Typing Context" $ do

    it "fails to lookup variable x in EmptyContext" $ do
      evaluate (lookupType "x" EmptyContext)
        `shouldThrow` anyException

    it "find variable x given x is defined in context" $ do
      lookupType "x" (Context EmptyContext "x" EU)
        `shouldBe` EU

    it "find variable x given y is also defined in context" $ do
      lookupType "x" (Context (Context EmptyContext "x" EU) "y" (EI 12))
        `shouldBe` EU

    it "binds decl x : t = v in context" $ do
      let γ' = bindType (B "x") EU EUnit EmptyContext
      lookupType "x" γ' `shouldBe`EU

    it "ignores decl with wildcard _ : t = v" $ do
      let γ' = bindType Wildcard EU EUnit EmptyContext
      γ' `shouldBe` EmptyContext

    it "binds pattern binding (p,p') : Σ t g = v decomposing pair" $ do
      let γ' = bindType (Pat (B "x") (B "y"))
               (ESig EU (Cl (B "x") (Pi Wildcard (Var "x") U) emptyEnv))
               (EPair EUnit (EI 12))
               EmptyContext

      lookupType "x" γ' `shouldBe` EU
      lookupType "y" γ' `shouldBe` EPi EUnit (Cl Wildcard U (ExtendPat EmptyEnv (B "x") EUnit))

  describe "Typing Judgments" $ do

    describe "Check a declaration is correct" $ do

      it "checks a simple declaration is correct given env and empty context" $ do
        let
          ρ = ExtendDecl EmptyEnv (Decl (B "Bool") U (Sum [ Choice "true" Unit, Choice "false" Unit]))
          γ = Context EmptyContext "Bool" (ESum ([ Choice "true" Unit, Choice "false" Unit], EmptyEnv))
          γ' = checkD 0 (Decl (B "x") (Var "Bool") (Ctor "true" Unit)) ρ γ

        lookupType "x" γ' `shouldBe` ESum ([Choice "true" Unit,Choice "false" Unit], EmptyEnv)

      it "checks a recursive declaration is correct given env and empty context" $ do
        let
          ρ = ExtendDecl EmptyEnv (RDecl (B "Nat") U (Sum [Choice "zero" Unit,Choice "succ" (Var "Nat")]))
          γ' = checkD 0 (RDecl (Pat (B "V") (B "T"))
                       (Sigma (B "X") U
                        (Pi Wildcard (Var "X") U))
                       (Pair
                        (Sum [ Choice "nat" Unit
                             , Choice "pi" (Sigma (B "x") (Var "V")
                                            (Pi Wildcard
                                             (Ap (Var "T") (Var "x"))
                                             (Var "V")))
                             ])
                        (Case [ Choice "nat" (Abs Wildcard (Var "Nat"))
                              , Choice "pi" (Abs (Pat (B "x") (B "f"))
                                             (Pi (B "y")
                                              (Ap (Var "T") (Var "x"))
                                              (Ap
                                                (Var "T")
                                                (Ap (Var "f")
                                                  (Var "y")))))
                              ]))) ρ EmptyContext

        lookupType "V" γ' `shouldBe` EUnit
