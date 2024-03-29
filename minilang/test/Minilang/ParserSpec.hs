module Minilang.ParserSpec where

import qualified Data.Text as Text
import Minilang.Parser
import Test.Hspec
import Text.Parsec.Error
import Text.Parsec.Pos

spec :: Spec
spec = parallel $
    describe "Minilang Parser" $ do
        describe "Parsing Terms and Expressions" $ do
            it "parse Number" $ do
                parseProgram False "12" `shouldBe` I 12
                parseProgram False "12.4" `shouldBe` D 12.4
                parseProgram False "-12" `shouldBe` I (-12)
                parseProgram False "-42.3" `shouldBe` D (-42.3)

            it "parse String" $
                parseProgram False "\"123\"" `shouldBe` S "123"

            it "parse Variable" $ do
                parseProgram False "abc" `shouldBe` Var "abc"
                parseProgram False "+" `shouldBe` Var "+"
                parseProgram False "-" `shouldBe` Var "-"
                parseProgram False "*" `shouldBe` Var "*"
                parseProgram False "/" `shouldBe` Var "/"
                parseProgram False "%" `shouldBe` Var "%"
                parseProgram False "^" `shouldBe` Var "^"
                parseProgram False "^?!<~#@&=" `shouldBe` Var "^?!<~#@&="

            it "parse Universe" $ do
                parseProgram False "U" `shouldBe` U 0
                parseProgram False "U2" `shouldBe` U 2
                parseProgram False "Uabc" `shouldBe` Var "Uabc"

            it "parse Unit" $
                parseProgram False "()" `shouldBe` Unit

            it "parse One" $
                parseProgram False "[]" `shouldBe` One

            it "parse Application" $
                parseProgram False "abc fge" `shouldBe` Ap (Var "abc") (Var "fge")

            it "run application parser" $
                doParse application "C (S abc)" `shouldBe` Ap (Var "C") (Ap (Var "S") (Var "abc"))

            it "parse application as left-associative" $ do
                parseProgram False "abc fge 12" `shouldBe` Ap (Ap (Var "abc") (Var "fge")) (I 12)
                parseProgram False "abc fge 12 k" `shouldBe` parseProgram False "((abc fge) 12) k"
                parseProgram False "abc fge 12 k bool" `shouldBe` parseProgram False "(((abc fge) 12) k) bool"
                parseProgram False "abc (fge 12)" `shouldBe` Ap (Var "abc") (Ap (Var "fge") (I 12))
                parseProgram False "(g n1) (natrec C a g n1)"
                    `shouldBe` Ap
                        (Ap (Var "g") (Var "n1"))
                        (Ap (Ap (Ap (Ap (Var "natrec") (Var "C")) (Var "a")) (Var "g")) (Var "n1"))

            it "parse Abstraction" $ do
                parseProgram False "λ abc . abc" `shouldBe` Abs (B "abc") (Var "abc")
                parseProgram False "λ abc . abc ghe" `shouldBe` Abs (B "abc") (Ap (Var "abc") (Var "ghe"))
                parseProgram False "λ _ . abc" `shouldBe` Abs Wildcard (Var "abc")

            it "parse Dependent Product" $ do
                parseProgram False "Π abc : U . abc" `shouldBe` Pi (B "abc") (U 0) (Var "abc")
                parseProgram False "Πabc:U.abc" `shouldBe` Pi (B "abc") (U 0) (Var "abc")
                parseProgram True "Π a : A . C (S a)" `shouldBe` Pi (B "a") (Var "A") (Ap (Var "C") (Ap (Var "S") (Var "a")))

            it "parse Function type" $ do
                parseProgram False "() -> []" `shouldBe` Pi Wildcard Unit One
                parseProgram False "(A : U) -> A" `shouldBe` Pi (B "A") (U 0) (Var "A")
                parseProgram False "(A : U) → (b : A) → ()" `shouldBe` Pi (B "A") (U 0) (Pi (B "b") (Var "A") Unit)
                parseProgram False "(_ : A) -> B" `shouldBe` parseProgram False "A -> B"
                parseProgram False "(a : A) -> C (S a)" `shouldBe` Pi (B "a") (Var "A") (Ap (Var "C") (Ap (Var "S") (Var "a")))

            it "parse Dependent Sum" $ do
                parseProgram False "Σ abc : U . abc" `shouldBe` Sigma (B "abc") (U 0) (Var "abc")
                parseProgram False "Σabc:   U. abc" `shouldBe` Sigma (B "abc") (U 0) (Var "abc")
                parseProgram False "Σ x : V . T x → V" `shouldBe` Sigma (B "x") (Var "V") (Pi Wildcard (Ap (Var "T") (Var "x")) (Var "V"))

            it "parse Projections" $ do
                parseProgram False "π1.abc" `shouldBe` P1 (Var "abc")
                parseProgram False "π2.abc" `shouldBe` P2 (Var "abc")

            it "parse Pairing" $
                parseProgram False "(abc,12)" `shouldBe` Pair (Var "abc") (I 12)

            it "parses Constructor w/ Pairing" $
                parseProgram False "foo (abc,12)" `shouldBe` Ap (Var "foo") (Pair (Var "abc") (I 12))

            it "parses Labelled Sum" $ do
                parseProgram False "Sum(foo [] | bar Nat)" `shouldBe` Sum [Choice "foo" (Just One), Choice "bar" (Just $ Var "Nat")]
                parseProgram False "Sum(foo[] | bar (Nat, Bool))" `shouldBe` Sum [Choice "foo" (Just One), Choice "bar" (Just $ Pair (Var "Nat") (Var "Bool"))]
                parseProgram False "Sum(foo | bar)" `shouldBe` Sum [Choice "foo" Nothing, Choice "bar" Nothing]
                parseProgram False "Sum  (  foo  | bar  )" `shouldBe` Sum [Choice "foo" Nothing, Choice "bar" Nothing]

            it "parses Case choices" $ do
                parseProgram False "case (foo 12 -> h1 | bar x -> h2)" `shouldBe` Case [Clause "foo" (Abs (C $ I 12) (Var "h1")), Clause "bar" (Abs (B "x") (Var "h2"))]
                parseProgram False "case (foo -> 12 | bar x -> h2)" `shouldBe` Case [Clause "foo" (Abs Wildcard (I 12)), Clause "bar" (Abs (B "x") (Var "h2"))]

            it "parses Pattern" $ do
                parseProgram False "λ (abc, xyz) . abc" `shouldBe` Abs (Pat (B "abc") (B "xyz")) (Var "abc")
                parseProgram False "Π (abc, xyz): U . abc" `shouldBe` Pi (Pat (B "abc") (B "xyz")) (U 0) (Var "abc")

            it "parses single-line comments after an expression" $ do
                parseProgram False "() -- this is a comment" `shouldBe` Unit
                parseProgram False "[] -- this is a comment" `shouldBe` One
                parseProgram False "-- this is a comment\n()" `shouldBe` Unit
                parseProgram False "λ -- this is a comment\n(abc, xyz) . abc"
                    `shouldBe` Abs (Pat (B "abc") (B "xyz")) (Var "abc")
                parseProgram False "case -- a comment\n (foo -- a comment \n-> 12 | bar x -> h2)"
                    `shouldBe` Case [Clause "foo" (Abs Wildcard (I 12)), Clause "bar" (Abs (B "x") (Var "h2"))]

            it "parses multiline comments" $ do
                parseProgram False "case {- this is a multiline \n comment -} (foo -- a comment \n-> 12 | bar x -> h2)"
                    `shouldBe` Case [Clause "foo" (Abs Wildcard (I 12)), Clause "bar" (Abs (B "x") (Var "h2"))]
                parseProgram False "{- this is a multiline comment -}\nlet x : Unit -> [] = case (tt -> ())"
                    `shouldBe` Let (Decl (B "x") (Pi Wildcard (Var "Unit") One) (Case [Clause "tt" (Abs Wildcard Unit)])) Unit

            it "parses holes" $ do
                pending
                parseProgram False "λ x . ?hole" `shouldBe` Abs (Pat (B "abc") (B "xyz")) (Hole "hole")

        describe "Declarations" $ do
            it "parses generic id function" $
                parseDecl "id : Π A : U . Π _ : A . A = λ A . λ x . x"
                    `shouldBe` Decl
                        (B "id")
                        (Pi (B "A") (U 0) (Pi Wildcard (Var "A") (Var "A")))
                        (Abs (B "A") (Abs (B "x") (Var "x")))

            it "parses Bool declaration" $ do
                parseDecl "Bool : U = Sum (true | false)"
                    `shouldBe` Decl (B "Bool") (U 0) (Sum [Choice "true" Nothing, Choice "false" Nothing])

                parseDecl "Bool : U = Sum(true| false)"
                    `shouldBe` Decl (B "Bool") (U 0) (Sum [Choice "true" Nothing, Choice "false" Nothing])

            it "parses some declaration" $
                parseProgram False "let x : Unit -> [] = case (tt -> ());()"
                    `shouldBe` Let
                        ( Decl
                            (B "x")
                            (Pi Wildcard (Var "Unit") One)
                            (Case [Clause "tt" (Abs Wildcard Unit)])
                        )
                        Unit

            it "parses Bool elimination" $
                parseDecl "elimBool : Π C : Bool → U . C false → C true → Π b : Bool . C b  = λ C . λ h0 . λ h1 . case (true → h1 | false → h0)"
                    `shouldBe` Decl
                        (B "elimBool")
                        ( Pi
                            (B "C")
                            (Pi Wildcard (Var "Bool") (U 0))
                            ( Pi
                                Wildcard
                                (Ap (Var "C") (Var "false"))
                                ( Pi
                                    Wildcard
                                    (Ap (Var "C") (Var "true"))
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

            it "parses Nat declaration" $
                parseDecl "rec Nat : U = Sum (zero | succ Nat)"
                    `shouldBe` RDecl (B "Nat") (U 0) (Sum [Choice "zero" Nothing, Choice "succ" (Just $ Var "Nat")])

            it "parses recursive-inductive universe let" $
                parseDecl "rec (V,T) : ΣX:U.X -> U = (Sum(nat | pi (Σ x : V . T x → V)) , case (nat → Nat | pi (x, f ) → Π y : T x . T (f y)))"
                    `shouldBe` RDecl
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
                                                (Ap (Var "f") (Var "y"))
                                            )
                                        )
                                    )
                                ]
                            )
                        )

            it "parses natRec " $
                parseProgram False "let rec natrec : Π C : Nat → U . C zero → (Π n : Nat.C n → C (succ n)) → Π n : Nat . C n = λ C . λ a . λ g . case (zero → a | succ n1 → (g n1) ((((natrec C) a) g) n1)); ()"
                    `shouldBe` Let
                        ( RDecl
                            (B "natrec")
                            ( Pi
                                (B "C")
                                (Pi Wildcard (Var "Nat") (U 0))
                                ( Pi
                                    Wildcard
                                    (Ap (Var "C") (Var "zero"))
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
                                                    (Ap (Var "succ") (Var "n"))
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

            it "parses a program" $
                parseProgram False ("let rec Nat : U = Sum (zero | succ Nat) ;\n" <> "let id : Π A : U . Π _ : A . A = λ A . λ x . x; ()")
                    `shouldBe` Let
                        (RDecl (B "Nat") (U 0) (Sum [Choice "zero" Nothing, Choice "succ" (Just $ Var "Nat")]))
                        ( Let
                            ( Decl
                                (B "id")
                                (Pi (B "A") (U 0) (Pi Wildcard (Var "A") (Var "A")))
                                (Abs (B "A") (Abs (B "x") (Var "x")))
                            )
                            Unit
                        )

            it "parses another program" $
                parseProgram False "let Unit : U = Sum (tt); let elimUnit : Π C : Unit -> U. C tt -> Π x:Unit. C x = λ C . λ h . case (tt -> h); ()"
                    `shouldBe` Let
                        ( Decl
                            (B "Unit")
                            (U 0)
                            (Sum [Choice "tt" Nothing])
                        )
                        ( Let
                            ( Decl
                                (B "elimUnit")
                                ( Pi
                                    (B "C")
                                    (Pi Wildcard (Var "Unit") (U 0))
                                    ( Pi
                                        Wildcard
                                        (Ap (Var "C") (Var "tt"))
                                        (Pi (B "x") (Var "Unit") (Ap (Var "C") (Var "x")))
                                    )
                                )
                                (Abs (B "C") (Abs (B "h") (Case [Clause "tt" (Abs Wildcard (Var "h"))])))
                            )
                            Unit
                        )

            it "parses a multiline program" $
                parseProgram
                    False
                    ( Text.unlines
                        [ "let rec NEList : Π A : U . U = λ A . Sum(S A | C (Σ a : A . NEList A));"
                        , "let elimNEList : Π A : U . Π C : NEList A -> U . (Π a : A . C (S a)) -> (Π a : (Σ _ : A . NEList A) . C (C a)) -> Π b : NEList A . C b "
                        , "  = λ A . λ  C . λ  h0 . λ h1 . case (S a -> h0 a | C a -> h1 a);"
                        , "let select : NEList Bool -> U = case (S _ -> Unit | C _ -> Unit);"
                        , "()"
                        ]
                    )
                    `shouldBe` Let (RDecl (B "NEList") (Pi (B "A") (U 0) (U 0)) (Abs (B "A") (Sum [Choice "S" (Just (Var "A")), Choice "C" (Just (Sigma (B "a") (Var "A") (Ap (Var "NEList") (Var "A"))))]))) (Let (Decl (B "elimNEList") (Pi (B "A") (U 0) (Pi (B "C") (Pi Wildcard (Ap (Var "NEList") (Var "A")) (U 0)) (Pi Wildcard (Pi (B "a") (Var "A") (Ap (Var "C") (Ap (Var "S") (Var "a")))) (Pi Wildcard (Pi (B "a") (Sigma Wildcard (Var "A") (Ap (Var "NEList") (Var "A"))) (Ap (Var "C") (Ap (Var "C") (Var "a")))) (Pi (B "b") (Ap (Var "NEList") (Var "A")) (Ap (Var "C") (Var "b"))))))) (Abs (B "A") (Abs (B "C") (Abs (B "h0") (Abs (B "h1") (Case [Clause "S" (Abs (B "a") (Ap (Var "h0") (Var "a"))), Clause "C" (Abs (B "a") (Ap (Var "h1") (Var "a")))])))))) (Let (Decl (B "select") (Pi Wildcard (Ap (Var "NEList") (Var "Bool")) (U 0)) (Case [Clause "S" (Abs Wildcard (Var "Unit")), Clause "C" (Abs Wildcard (Var "Unit"))])) Unit))

        describe "Error handling" $ do
            describe "skipErrorTo" $
                it "consumes tokens until some parser succeeds then yields an Error" $ do
                    let fragment = do
                            a <- skipErrorTo [scolon]
                            b <- scolon *> term
                            pure $ Ap a b

                    doParse fragment "fooo ; bar"
                        `shouldBe` Ap
                            (Err $ newErrorMessage (Message "found 'fooo ' between (1,1) and (1,6)") (newPos "" 1 6))
                            (Var "bar")

            it "inserts an Err node in AST on error in let" $ do
                let errorNode = Err $ newErrorMessage (Message "found 'case (tt -> ()' between (1,22) and (1,36)") (newPos "" 1 36)

                parseProgram False "let x : Unit -> [] = case (tt -> ();()"
                    `shouldBe` Let
                        ( Decl
                            (B "x")
                            (Pi Wildcard (Var "Unit") One)
                            errorNode
                        )
                        Unit

            it "inserts an Err node in AST on error in case clause" $ do
                let errorNode = Err $ newErrorMessage (Message "found '-> ' between (1,31) and (1,34)") (newPos "" 1 34)

                parseProgram False "let x : Unit -> [] = case (tt -> | ff -> ());()"
                    `shouldBe` Let
                        ( Decl
                            (B "x")
                            (Pi Wildcard (Var "Unit") One)
                            ( Case
                                [ Clause "tt" errorNode
                                , Clause "ff" (Abs Wildcard Unit)
                                ]
                            )
                        )
                        Unit
