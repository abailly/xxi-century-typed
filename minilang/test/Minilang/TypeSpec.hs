module Minilang.TypeSpec where

import qualified Data.Text       as Text
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
        γ  <- checkD 0 (RDecl (B "Nat") U (Sum [Choice "zero" Nothing, Choice "succ" (Just $ Var "Nat")]))
               EmptyEnv EmptyContext
        γ' <- checkD 0 (RDecl (Pat (B "V") (B "T"))
                         (Sigma (B "X") U
                          (Pi Wildcard (Var "X") U))
                         (Pair
                          (Sum [ Choice "nat" Nothing
                               , Choice "pi" (Just $ Sigma (B "x") (Var "V")
                                              (Pi Wildcard
                                               (Ap (Var "T") (Var "x"))
                                               (Var "V")))
                               ])
                          (Case [ Clause "nat" (Abs Wildcard (Var "Nat"))
                                , Clause "pi" (Abs (Pat (B "x") (B "f"))
                                               (Pi (B "y")
                                                (Ap (Var "T") (Var "x"))
                                                (Ap
                                                 (Var "T")
                                                 (Ap (Var "f")
                                                   (Var "y")))))
                                ]))) EmptyEnv γ

        lookupType "V" γ' `shouldReturn` EU

      it "Check simple Bool function" $ do
        check 0 (Def
                  (Decl (B "Bool") U (Sum [ Choice "true" Nothing, Choice "false" Nothing]))
                  (Def
                   (Decl (B "not")
                     (Pi Wildcard (Var "Bool") (Var "Bool"))
                     (Case [ Clause "true" (Abs Wildcard (Ctor "false" Nothing))
                           , Clause "false" (Abs Wildcard (Ctor "true" Nothing))
                           ]))
                   (Ap (Var "not") (Ctor "false" Nothing))))
          (ESum ([Choice "true" Nothing, Choice "false" Nothing], EmptyEnv))
          EmptyEnv EmptyContext
          `shouldReturn` ()

      it "Check Non empty list and head function" $ do
        let
          e = parseProgram False $
              Text.unlines [ "Unit : U = Sum(tt);"
                           , "rec NEList : Π A:U . U = λ A . Sum(S A| C (Σ _ : A . NEList A));"
                           , "head : Π A:U . NEList A -> A = λ A . fun(S a -> a| C (a,_) -> a);"
                           , "l : NEList Unit = $C ($tt, $C($tt, $S $tt));"
                           , "x : Unit -> [] = fun(tt -> ());"
                           , "x (head Unit l)"
                           ]
        check 0 e EOne EmptyEnv EmptyContext
          `shouldReturn` ()

      -- TODO following tests are failing due to unknown issue in type checker algorithm

      it "Check Unit and unitElim" $ do
        check 0 (Def
                  (Decl (B "Unit") U (Sum [Choice "tt" Nothing]))
                  (Def
                    (Decl (B "elimUnit")
                      (Pi (B "C") (Pi Wildcard (Var "Unit") U)
                        (Pi Wildcard (Ap (Var "C") (Ctor "tt" Nothing))
                          (Pi (B "x") (Var "Unit") (Ap (Var "C") (Var "x")))))
                      (Abs (B "C")
                        (Abs (B "h")
                          (Case [Clause "tt" (Abs Wildcard (Var "h"))]))))
                    Unit))
          EOne
          EmptyEnv
          EmptyContext
          `shouldReturn` ()

      it "Check Bool and elimBool declarations followed by an expression has type EOne" $ do
        check 0 (Def
                  (Decl (B "Bool") U (Sum [ Choice "true" Nothing, Choice "false" Nothing]))
                  (Def (Decl (B "elimBool")
                         (Pi (B "C")
                           (Pi Wildcard (Var "Bool") U)
                           (Pi Wildcard
                             (Ap (Var "C") (Ctor "false" Nothing))
                             (Pi Wildcard
                               (Ap (Var "C") (Ctor "true" Nothing))
                               (Pi (B "b") (Var "Bool")
                                 (Ap (Var "C") (Var "b"))))))
                         (Abs (B "C")
                           (Abs (B "h0")
                             (Abs (B "h1")
                               (Case [ Clause "true" (Abs Wildcard (Var "h1"))
                                     , Clause "false" (Abs Wildcard (Var "h0"))])))))
                    (Ap (Var "elimBool") (Ctor "false" Nothing))))
          EUnit
          EmptyEnv EmptyContext
          `shouldReturn` ()

      -- it "Check Nat and elimNat declarations followed by an expression has type EOne" $ do
      --   check 0 (Def
      --             (RDecl (B "Nat") U (Sum [Choice "zero" One, Choice "succ" (Var "Nat")]))
      --             (Def
      --             (RDecl (B "natrec")
      --              (Pi (B "C")
      --               (Pi Wildcard (Var "Nat") U)
      --               (Pi Wildcard (Ap (Var "C") (Ctor "zero" Unit))
      --                (Pi Wildcard
      --                 (Pi (B "n")
      --                  (Var "Nat")
      --                  (Pi Wildcard (Ap (Var "C") (Var "n"))
      --                   (Ap (Var "C")
      --                    (Ctor "succ" (Var "n")))))
      --                  (Pi (B "n")
      --                   (Var "Nat")
      --                   (Ap (Var "C") (Var "n"))))))
      --               (Abs (B "C")
      --                (Abs (B "a")
      --                 (Abs (B "g")
      --                  (Case [Choice "zero" (Abs Wildcard (Var "a"))
      --                        ,Choice "succ" (Abs (B "n1")
      --                                        (Ap
      --                                         (Ap
      --                                          (Ap
      --                                           (Ap
      --                                            (Ap
      --                                             (Ap (Var "g") (Var "n1"))
      --                                             (Var "natrec"))
      --                                             (Var "C"))
      --                                            (Var "a"))
      --                                           (Var "g"))
      --                                          (Var "n1")))
      --                        ])))))
      --              Unit))
      --     EUnit
      --     EmptyEnv EmptyContext
      --     `shouldReturn` ()
