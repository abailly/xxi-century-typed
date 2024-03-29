{-# LANGUAGE TypeApplications #-}

module Minilang.TypeSpec where

import qualified Data.Text as Text
import Minilang.Env
import Minilang.Eval
import Minilang.Parser
import Minilang.Primitives
import Minilang.Type
import Test.Hspec

spec :: Spec
spec = parallel $
    describe "Type Checker" $ do
        describe "Typing Context" $ do
            it "fails to lookup variable x in EmptyContext" $
                lookupType "x" EmptyContext
                    `shouldThrow` anyException

            it "find variable x given x is defined in context" $
                lookupType "x" (Context EmptyContext "x" (EU 0))
                    `shouldReturn` EU 0

            it "find variable x given y is also defined in context" $
                lookupType "x" (Context (Context EmptyContext "x" (EU 0)) "y" (EI 12))
                    `shouldReturn` EU 0

            it "binds decl x : t = v in context" $ do
                γ' <- bindType (B "x") (EU 0) EUnit EmptyContext
                lookupType "x" γ' `shouldReturn` EU 0

            it "ignores decl with wildcard _ : t = v" $
                bindType Wildcard (EU 0) EUnit EmptyContext
                    `shouldReturn` EmptyContext

            it "binds pattern binding (p,p') : Σ t g = v decomposing pair" $ do
                γ' <-
                    bindType
                        (Pat (B "x") (B "y"))
                        (ESig (EU 0) (Cl (B "x") (Pi Wildcard (Var "x") (U 0)) emptyEnv))
                        (EPair EUnit (EI 12))
                        EmptyContext

                lookupType "x" γ' `shouldReturn` EU 0
                lookupType "y" γ' `shouldReturn` EPi EUnit (Cl Wildcard (U 0) (ExtendPat EmptyEnv (B "x") EUnit))

        describe "Typing Judgments" $ do
            describe "Builtins" $ do
                it "types primitive int as #Int" $
                    checkI 0 (I 12) EmptyEnv EmptyContext
                        `shouldReturn` EPrim PrimInt

                it "types primitive double as #Double" $
                    checkI 0 (D 12) EmptyEnv EmptyContext
                        `shouldReturn` EPrim PrimDouble

                it "resolves Int as primitive type #Int" $
                    lookupType "Int" EmptyContext `shouldReturn` EU 0

                it "resolves Double as primitive type #Double" $
                    lookupType "Double" EmptyContext `shouldReturn` EU 0

                it "types primitive string as #String" $
                    checkI 0 (S "foo") EmptyEnv EmptyContext
                        `shouldReturn` EPrim PrimString

            describe "Type inference" $ do
                it "can infer type of zero-arg constructor" $ do
                    let dec = Decl (B "Bool") (U 0) (Sum [Choice "true" Nothing, Choice "false" Nothing])
                    (ρ, γ) <- checkD 0 dec EmptyEnv EmptyContext
                    t <- checkI 0 (Ctor "true" Nothing) ρ γ
                    t `shouldBe` ESum (SumClos ([Choice "true" Nothing, Choice "false" Nothing], EmptyEnv))

                it "can infer type of non polymorphic one-arg constructor applied" $ do
                    let dec = RDecl (B "Nat") (U 0) (Sum [Choice "Z" Nothing, Choice "S" (Just $ Var "Nat")])
                    (ρ, γ) <- checkD 0 dec EmptyEnv EmptyContext
                    t <- checkI 0 (Ctor "S" (Just $ Ctor "Z" Nothing)) ρ γ
                    t
                        `shouldBe` ESum
                            ( SumClos
                                (
                                    [ Choice "Z" Nothing
                                    , Choice "S" (Just (Var "Nat"))
                                    ]
                                , ExtendPat
                                    ( ExtendDecl
                                        EmptyEnv
                                        ( RDecl
                                            (B "Nat")
                                            (U 0)
                                            (Sum [Choice "Z" Nothing, Choice "S" (Just (Var "Nat"))])
                                        )
                                    )
                                    (B "u")
                                    (ECtor "Z" Nothing)
                                )
                            )

                it "rejects one-arg constructor applied to Nothing" $ do
                    let dec =
                            RDecl
                                (B "NEList")
                                (Pi (B "A") (U 0) (U 0))
                                ( Abs
                                    (B "A")
                                    (Sum [Choice "S" (Just (Var "A")), Choice "C" (Just (Sigma (B "a") (Var "A") (Ap (Var "NEList") (Var "A"))))])
                                )
                    (ρ, γ) <- checkD 0 dec EmptyEnv EmptyContext
                    checkI 0 (Ctor "S" Nothing) ρ γ
                        `shouldThrow` \TypingError{} -> True

                -- it "can infer type of polymorphic one-arg constructor applied" $ do
                --     let nat = RDecl (B "Nat") (U 0) (Sum [Choice "zero" Nothing, Choice "succ" (Just $ Var "Nat")])
                --         nelist =
                --             RDecl
                --                 (B "NEList")
                --                 (Pi (B "A") (U 0) (U 0))
                --                 ( Abs
                --                     (B "A")
                --                     (Sum [Choice "S" (Just (Var "A")), Choice "C" (Just (Sigma (B "a") (Var "A") (Ap (Var "NEList") (Var "A"))))])
                --                 )

                --     γ <- checkD 0 nat EmptyEnv EmptyContext
                --     let ρ = extend nat EmptyEnv
                --     γ' <- checkD 0 nelist ρ γ
                --     let ρ' = extend nelist ρ
                --     t <- checkI 0 (Ctor "S" (Just (Ctor "succ" (Just (Ctor "zero" Nothing))))) ρ' γ'
                --     t
                --         `shouldBe` ESum
                --             ( SumClos
                --                 (
                --                     [ Choice "S" (Just (Var "Nat"))
                --                     , Choice "C" (Just (Sigma (B "a") (Var "Nat") (Ap (Var "NEList") (Var "Nat"))))
                --                     ]
                --                 , EmptyEnv
                --                 )
                --             )

                it "can infer type of zero-arg ctor in nested complex env" $ do
                    let ρ =
                            ExtendDecl
                                ( ExtendDecl
                                    ( ExtendDecl
                                        ( ExtendDecl
                                            ( ExtendDecl EmptyEnv (Decl (B "Unit") (U 0) (Sum [Choice "tt" Nothing]))
                                            )
                                            ( Decl
                                                (B "elimUnit")
                                                ( Pi
                                                    (B "C")
                                                    (Pi Wildcard (Var "Unit") (U 0))
                                                    ( Pi
                                                        Wildcard
                                                        (Ap (Var "C") (Ctor "tt" Nothing))
                                                        (Pi (B "x") (Var "Unit") (Ap (Var "C") (Var "x")))
                                                    )
                                                )
                                                (Abs (B "C") (Abs (B "h") (Case [Clause "tt" (Abs Wildcard (Var "h"))])))
                                            )
                                        )
                                        (Decl (B "Bool") (U 0) (Sum [Choice "true" Nothing, Choice "false" Nothing]))
                                    )
                                    ( Decl
                                        (B "elimBool")
                                        ( Pi
                                            (B "C")
                                            (Pi Wildcard (Var "Bool") (U 0))
                                            ( Pi
                                                Wildcard
                                                (Ap (Var "C") (Ctor "false" Nothing))
                                                ( Pi
                                                    Wildcard
                                                    ( Ap
                                                        (Var "C")
                                                        (Ctor "true" Nothing)
                                                    )
                                                    (Pi (B "b") (Var "Bool") (Ap (Var "C") (Var "b")))
                                                )
                                            )
                                        )
                                        ( Abs
                                            (B "C")
                                            ( Abs
                                                (B "h0")
                                                ( Abs
                                                    (B "h1")
                                                    ( Case
                                                        [ Clause "true" (Abs Wildcard (Var "h1"))
                                                        , Clause "false" (Abs Wildcard (Var "h0"))
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                                ( Decl
                                    (B "not")
                                    (Pi Wildcard (Var "Bool") (Var "Bool"))
                                    ( Case
                                        [ Clause "true" (Abs Wildcard (Ctor "false" Nothing))
                                        , Clause "false" (Abs Wildcard (Ctor "true" Nothing))
                                        ]
                                    )
                                )

                    ESum (SumClos (closure, _)) <- checkI 0 (Ctor "true" Nothing) ρ EmptyContext
                    closure `shouldBe` [Choice "true" Nothing, Choice "false" Nothing]

                it "can infer type of universe" $ do
                    t <- checkI 0 (U 0) EmptyEnv EmptyContext
                    t `shouldBe` EU 1

                it "can check universe type judgment" $ do
                    check @IO 0 (U 2) (EU 3) EmptyEnv EmptyContext
                    check @IO 0 (U 3) (EU 3) EmptyEnv EmptyContext
                        `shouldThrow` \TypingError{} -> True

            describe "Check a declaration is correct" $ do
                it "check declarations registers constructors in env and context" $ do
                    let dec = Decl (B "Bool") (U 0) (Sum [Choice "true" Nothing, Choice "false" Nothing])
                    (ρ, γ) <- checkD 0 dec EmptyEnv EmptyContext

                    rho ρ "true"
                        `shouldBe` ECtor "true" Nothing

                    lookupType "true" γ
                        `shouldReturn` ESum (SumClos ([Choice "true" Nothing, Choice "false" Nothing], EmptyEnv))

                it "check declarations registers one-arg constructors in env and context" $ do
                    let dec =
                            RDecl
                                (B "Foo")
                                (Pi (B "A") (U 0) (U 0))
                                ( Abs
                                    (B "A")
                                    (Sum [Choice "S" (Just (Var "A")), Choice "C" Nothing])
                                )
                    (ρ, γ) <- checkD 0 dec EmptyEnv EmptyContext

                    rho ρ "S"
                        `shouldBe` ECtor "S" Nothing

                    lookupType "S" γ
                        `shouldReturn` ESum (SumClos ([Choice "S" (Just (Var "A")), Choice "C" Nothing], EmptyEnv))

                it "checks a recursive declaration is correct given env and empty context" $ do
                    (ρ, γ) <-
                        checkD
                            0
                            (RDecl (B "Nat") (U 0) (Sum [Choice "zero" Nothing, Choice "succ" (Just $ Var "Nat")]))
                            EmptyEnv
                            EmptyContext
                    (_, γ') <-
                        checkD
                            0
                            ( RDecl
                                (Pat (B "V") (B "T"))
                                ( Sigma
                                    (B "X")
                                    (U 0)
                                    (Pi Wildcard (Var "X") (U 0))
                                )
                                ( Pair
                                    ( Sum
                                        [ Choice "nat" Nothing
                                        , Choice
                                            "pi"
                                            ( Just $
                                                Sigma
                                                    (B "x")
                                                    (Var "V")
                                                    ( Pi
                                                        Wildcard
                                                        (Ap (Var "T") (Var "x"))
                                                        (Var "V")
                                                    )
                                            )
                                        ]
                                    )
                                    ( Case
                                        [ Clause "nat" (Abs Wildcard (Var "Nat"))
                                        , Clause
                                            "pi"
                                            ( Abs
                                                (Pat (B "x") (B "f"))
                                                ( Pi
                                                    (B "y")
                                                    (Ap (Var "T") (Var "x"))
                                                    ( Ap
                                                        (Var "T")
                                                        ( Ap
                                                            (Var "f")
                                                            (Var "y")
                                                        )
                                                    )
                                                )
                                            )
                                        ]
                                    )
                                )
                            )
                            ρ
                            γ

                    lookupType "V" γ' `shouldReturn` EU 0

                it "Check simple Bool function" $
                    check
                        0
                        ( Let
                            (Decl (B "Bool") (U 0) (Sum [Choice "true" Nothing, Choice "false" Nothing]))
                            ( Let
                                ( Decl
                                    (B "not")
                                    (Pi Wildcard (Var "Bool") (Var "Bool"))
                                    ( Case
                                        [ Clause "true" (Abs Wildcard (Var "false"))
                                        , Clause "false" (Abs Wildcard (Var "true"))
                                        ]
                                    )
                                )
                                (Ap (Var "not") (Ctor "false" Nothing))
                            )
                        )
                        (ESum (SumClos ([Choice "true" Nothing, Choice "false" Nothing], EmptyEnv)))
                        EmptyEnv
                        EmptyContext
                        `shouldReturn` ()

                it "Check Non empty list and head function" $ do
                    let e =
                            parseProgram False $
                                Text.unlines
                                    [ "let Unit : U = Sum(tt);"
                                    , "let rec NEList : Π A:U . U = λ A . Sum(S A | C (Σ a : A . NEList A));"
                                    , "let head : Π A:U . NEList A -> A = λ A . case(S a -> a | C (a,_) -> a);"
                                    , "let l : NEList Unit = C (tt, C(tt, S tt));"
                                    , "let x : Unit -> [] = case(tt -> ());"
                                    , "x (head Unit l)"
                                    ]
                    check 0 e EOne EmptyEnv EmptyContext
                        `shouldReturn` ()

                it "Check Unit and unitElim" $
                    check
                        0
                        ( Let
                            (Decl (B "Unit") (U 0) (Sum [Choice "tt" Nothing]))
                            ( Let
                                ( Decl
                                    (B "elimUnit")
                                    ( Pi
                                        (B "C")
                                        (Pi Wildcard (Var "Unit") (U 0))
                                        ( Pi
                                            Wildcard
                                            (Ap (Var "C") (Ctor "tt" Nothing))
                                            (Pi (B "x") (Var "Unit") (Ap (Var "C") (Var "x")))
                                        )
                                    )
                                    ( Abs
                                        (B "C")
                                        ( Abs
                                            (B "h")
                                            (Case [Clause "tt" (Abs Wildcard (Var "h"))])
                                        )
                                    )
                                )
                                Unit
                            )
                        )
                        EOne
                        EmptyEnv
                        EmptyContext
                        `shouldReturn` ()

                it "Check Bool and elimBool declarations followed by an expression has type EOne" $
                    check
                        0
                        ( Let
                            (Decl (B "Bool") (U 0) (Sum [Choice "true" Nothing, Choice "false" Nothing]))
                            ( Let
                                ( Decl
                                    (B "elimBool")
                                    ( Pi
                                        (B "C")
                                        (Pi Wildcard (Var "Bool") (U 0))
                                        ( Pi
                                            Wildcard
                                            (Ap (Var "C") (Ctor "false" Nothing))
                                            ( Pi
                                                Wildcard
                                                (Ap (Var "C") (Ctor "true" Nothing))
                                                ( Pi
                                                    (B "b")
                                                    (Var "Bool")
                                                    (Ap (Var "C") (Var "b"))
                                                )
                                            )
                                        )
                                    )
                                    ( Abs
                                        (B "C")
                                        ( Abs
                                            (B "h0")
                                            ( Abs
                                                (B "h1")
                                                ( Case
                                                    [ Clause "true" (Abs Wildcard (Var "h1"))
                                                    , Clause "false" (Abs Wildcard (Var "h0"))
                                                    ]
                                                )
                                            )
                                        )
                                    )
                                )
                                ( Ap
                                    ( Ap
                                        ( Ap
                                            ( Ap
                                                (Var "elimBool")
                                                ( Case
                                                    [ Clause "true" (Abs Wildcard One)
                                                    , Clause "false" (Abs Wildcard One)
                                                    ]
                                                )
                                            )
                                            Unit
                                        )
                                        Unit
                                    )
                                    (Ctor "false" Nothing)
                                )
                            )
                        )
                        EOne
                        EmptyEnv
                        EmptyContext
                        `shouldReturn` ()

                it "Check Nat and elimNat declarations followed by an expression has type EOne" $
                    check
                        0
                        ( Let
                            (RDecl (B "Nat") (U 0) (Sum [Choice "zero" Nothing, Choice "succ" (Just $ Var "Nat")]))
                            ( Let
                                ( RDecl
                                    (B "natrec")
                                    ( Pi
                                        (B "C")
                                        (Pi Wildcard (Var "Nat") (U 0))
                                        ( Pi
                                            Wildcard
                                            (Ap (Var "C") (Ctor "zero" Nothing))
                                            ( Pi
                                                Wildcard
                                                ( Pi
                                                    (B "n")
                                                    (Var "Nat")
                                                    ( Pi
                                                        Wildcard
                                                        (Ap (Var "C") (Var "n"))
                                                        ( Ap
                                                            (Var "C")
                                                            (Ctor "succ" (Just $ Var "n"))
                                                        )
                                                    )
                                                )
                                                ( Pi
                                                    (B "n")
                                                    (Var "Nat")
                                                    (Ap (Var "C") (Var "n"))
                                                )
                                            )
                                        )
                                    )
                                    ( Abs
                                        (B "C")
                                        ( Abs
                                            (B "a")
                                            ( Abs
                                                (B "g")
                                                ( Case
                                                    [ Clause "zero" (Abs Wildcard (Var "a"))
                                                    , Clause
                                                        "succ"
                                                        ( Abs
                                                            (B "n1")
                                                            ( Ap
                                                                (Ap (Var "g") (Var "n1"))
                                                                ( Ap
                                                                    ( Ap
                                                                        ( Ap
                                                                            ( Ap
                                                                                (Var "natrec")
                                                                                (Var "C")
                                                                            )
                                                                            (Var "a")
                                                                        )
                                                                        (Var "g")
                                                                    )
                                                                    (Var "n1")
                                                                )
                                                            )
                                                        )
                                                    ]
                                                )
                                            )
                                        )
                                    )
                                )
                                Unit
                            )
                        )
                        EOne
                        EmptyEnv
                        EmptyContext
                        `shouldReturn` ()
