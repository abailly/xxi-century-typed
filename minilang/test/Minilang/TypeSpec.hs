module Minilang.TypeSpec where

import           Minilang.Eval
import           Minilang.Parser
import           Minilang.Type
import           Test.Hspec


spec :: Spec
spec = parallel $ describe "Type Checker" $ do

  describe "Typing Context" $ do

    it "fails to lookup variable x in EmptyContext" $ do
      lookupType "x" EmptyContext
        `shouldThrow` anyException

    it "find variable x given x is defined in context" $ do
      lookupType "x" (Context EmptyContext "x" EU)
        `shouldReturn` EU

    it "find variable x given y is also defined in context" $ do
      lookupType "x" (Context (Context EmptyContext "x" EU) "y" (EI 12))
        `shouldReturn` EU

    it "binds decl x : t = v in context" $ do
      γ' <- bindType (B "x") EU EUnit EmptyContext
      lookupType "x" γ' `shouldReturn`EU

    it "ignores decl with wildcard _ : t = v" $ do
      bindType Wildcard EU EUnit EmptyContext
        `shouldReturn` EmptyContext

    it "binds pattern binding (p,p') : Σ t g = v decomposing pair" $ do
      γ' <- bindType (Pat (B "x") (B "y"))
               (ESig EU (Cl (B "x") (Pi Wildcard (Var "x") U) emptyEnv))
               (EPair EUnit (EI 12))
               EmptyContext

      lookupType "x" γ' `shouldReturn` EU
      lookupType "y" γ' `shouldReturn` EPi EUnit (Cl Wildcard U (ExtendPat EmptyEnv (B "x") EUnit))

  describe "Typing Judgments" $ do

    describe "Check a declaration is correct" $ do

      it "checks a recursive declaration is correct given env and empty context" $ do
        γ  <- checkD 0 (RDecl (B "Nat") U (Sum [Choice "zero" Unit, Choice "succ" (Var "Nat")]))
               EmptyEnv EmptyContext
        γ' <- checkD 0 (RDecl (Pat (B "V") (B "T"))
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
                                ]))) EmptyEnv γ

        lookupType "V" γ' `shouldReturn` EU

      it "Check a declaration followed by an expression has type EUnit" $ do
        check 0 (Def
                  (Decl (B "Bool") U (Sum [ Choice "true" Unit, Choice "false" Unit]))
                  (Def (Decl (B "elimBool")
                         (Pi (B "C")
                           (Pi Wildcard (Var "Bool") U)
                           (Pi Wildcard
                             (Ap (Var "C") (Ctor "false" Unit))
                             (Pi Wildcard
                               (Ap (Var "C") (Ctor "true" Unit))
                               (Pi (B "b") (Var "Bool")
                                 (Ap (Var "C") (Var "b"))))))
                         (Abs (B "C")
                           (Abs (B "h0")
                             (Abs (B "h1")
                               (Case [ Choice "true" (Abs Wildcard (Var "h1"))
                                     , Choice "false" (Abs Wildcard (Var "h0"))])))))
                    (Ap (Var "elimbool") (Ctor "false" Unit))))
          EUnit
          EmptyEnv EmptyContext
          `shouldReturn` ()

spec' :: Spec
spec' = describe "" $ do
      it "Check a declaration followed by an expression has type EUnit" $ do
        check 0 (Def
                  (Decl (B "Bool") U (Sum [ Choice "true" Unit, Choice "false" Unit]))
                  (Def (Decl (B "elimBool")
                         (Pi (B "C")
                           (Pi Wildcard (Var "Bool") U)
                           (Pi Wildcard
                             (Ap (Var "C") (Ctor "false" Unit))
                             (Pi Wildcard
                               (Ap (Var "C") (Ctor "true" Unit))
                               (Pi (B "b") (Var "Bool")
                                 (Ap (Var "C") (Var "b"))))))
                         (Abs (B "C")
                           (Abs (B "h0")
                             (Abs (B "h1")
                               (Case [ Choice "true" (Abs Wildcard (Var "h1"))
                                     , Choice "false" (Abs Wildcard (Var "h0"))])))))
                    Unit))
          EUnit
          EmptyEnv EmptyContext
          `shouldReturn` ()
