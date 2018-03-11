module Minilang.ParserSpec where

import           Minilang.Parser
import           Minilang.Pretty
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Minilang Core" $ do

  describe "Parsing Terms and Expressions" $ do

    it "parse Number" $ do
      parseProgram False "12" `shouldBe` I 12
      parseProgram False "12.4" `shouldBe` D 12.4

    it "parse Variable" $ do
      parseProgram False "abc" `shouldBe` Var "abc"

    it "parse Universe" $ do
      parseProgram False "U" `shouldBe` U
      parseProgram False "Uabc" `shouldBe` Var "Uabc"

    it "parse Universe" $ do
      parseProgram False "U" `shouldBe` U

    it "parse Unit" $ do
      parseProgram False "()" `shouldBe` Unit

    it "parse One" $ do
      parseProgram False "[]" `shouldBe` One

    it "parse Application" $
      parseProgram False "abc fge" `shouldBe` Ap (Var "abc") (Var "fge")

    it "parse application as left-associative" $ do
      parseProgram False "abc fge 12" `shouldBe` (Ap (Ap (Var "abc") (Var "fge")) (I 12))
      parseProgram False "abc fge 12 k" `shouldBe` parseProgram False "((abc fge) 12) k"
      parseProgram False "abc fge 12 k bool" `shouldBe` parseProgram False "(((abc fge) 12) k) bool"
      parseProgram False "abc (fge 12)" `shouldBe` (Ap (Var "abc") (Ap (Var "fge") (I 12)))
      parseProgram False "(g n1) (natrec C a g n1)"
        `shouldBe` (Ap (Ap (Var "g") (Var "n1"))
                     (Ap (Ap (Ap (Ap (Var "natrec")  (Var "C")) (Var "a")) (Var "g")) (Var "n1")))

    it "parse Abstraction" $ do
      parseProgram False "λ abc . abc" `shouldBe` Abs (B "abc") (Var "abc")
      parseProgram False "λ abc . abc ghe" `shouldBe` Abs (B "abc") (Ap (Var "abc") (Var "ghe"))
      parseProgram False "λ _ . abc" `shouldBe` Abs Wildcard (Var "abc")

    it "parse Dependent Product" $ do
      parseProgram False "Π abc : U . abc" `shouldBe` Pi (B "abc") U (Var "abc")
      parseProgram False "Πabc:U.abc" `shouldBe` Pi (B "abc") U (Var "abc")
      parseProgram False "Unit -> []" `shouldBe` Pi Wildcard  (Var "Unit") One

    it "parse Dependent Sum" $ do
      parseProgram False "Σ abc : U . abc" `shouldBe` Sigma (B "abc") U (Var "abc")
      parseProgram False "Σabc:   U. abc" `shouldBe` Sigma (B "abc") U (Var "abc")
      parseProgram False "Σ x : V . T x → V" `shouldBe` Sigma (B "x") (Var "V") (Pi Wildcard (Ap (Var "T") (Var "x")) (Var "V"))

    it "parse Projections" $ do
      parseProgram False "π1.abc" `shouldBe` P1 (Var "abc")
      parseProgram False "π2.abc" `shouldBe` P2 (Var "abc")

    it "parse Constructor application" $ do
      parseProgram False "$c1 abc" `shouldBe` Ctor "c1" (Var "abc")

    it "parse Pairing" $ do
      parseProgram False "(abc,12)" `shouldBe` Pair (Var "abc") (I 12)

    it "parses Constructor w/ Pairing" $ do
      parseProgram False "$foo (abc,12)" `shouldBe` (Ctor "foo" (Pair (Var "abc") (I 12)))

    it "parses Labelled Sum" $ do
      parseProgram False "Sum(foo []| bar Nat)" `shouldBe` Sum [ Choice "foo" One , Choice "bar" (Var "Nat")]
      parseProgram False "Sum(foo[]| bar (Nat, Bool))" `shouldBe` Sum [ Choice "foo" One , Choice "bar" (Pair (Var "Nat") (Var "Bool"))]
      parseProgram False "Sum(foo| bar)" `shouldBe` Sum [ Choice "foo" One , Choice "bar" One]
      parseProgram False "Sum  (  foo  | bar  )" `shouldBe` Sum [ Choice "foo" One , Choice "bar" One]

    it "parses Case choices" $ do
      parseProgram False "fun (foo 12 -> h1 | bar x -> h2)" `shouldBe` Case [ Choice "foo" (Abs (C $ I 12) (Var "h1")) , Choice "bar" (Abs (B "x") (Var "h2"))]
      parseProgram False "fun (foo -> 12 | bar x -> h2)" `shouldBe` Case [ Choice "foo" (Abs Wildcard (I 12)) , Choice "bar" (Abs (B "x") (Var "h2"))]

    it "parses Pattern" $ do
      parseProgram False "λ (abc, xyz) . abc" `shouldBe` Abs (Pat (B "abc") (B "xyz")) (Var "abc")
      parseProgram False "Π (abc, xyz): U . abc" `shouldBe` Pi (Pat (B "abc") (B "xyz")) U (Var "abc")

  describe "Declarations" $ do

    it "parses generic id function" $ do
      parseDecl "id : Π A : U . Π _ : A . A = λ A . λ x . x"
        `shouldBe` Decl (B "id")
                        (Pi (B "A") U (Pi Wildcard (Var "A") (Var "A")))
                        (Abs (B "A") (Abs (B "x") (Var "x")))

    it "parses Bool declaration" $ do
      parseDecl "Bool : U = Sum (true | false)"
        `shouldBe` Decl (B "Bool") U (Sum [ Choice "true" One, Choice "false" One])

    it "parses Bool elimination" $ do
      parseDecl "elimBool : Π C : Bool → U . C false → C true → Π b : Bool . C b  = λ C . λ h0 . λ h1 . fun (true → h1 | false → h0)"
        `shouldBe` Decl (B "elimBool")
                   (Pi (B "C")
                     (Pi Wildcard (Var "Bool") U)
                     (Pi Wildcard
                       (Ap (Var "C") (Var "false"))
                       (Pi Wildcard
                         (Ap (Var "C") (Var "true"))
                         (Pi (B "b") (Var "Bool")
                           (Ap (Var "C") (Var "b"))))))
                   (Abs (B "C")
                     (Abs (B "h0")
                       (Abs (B "h1")
                         (Case [ Choice "true" (Abs Wildcard (Var "h1"))
                               , Choice "false" (Abs Wildcard (Var "h0"))]))))

    it "parses Nat declaration" $ do
      parseDecl "rec Nat : U = Sum (zero | succ Nat)"
        `shouldBe` RDecl (B "Nat") U (Sum [Choice "zero" One, Choice "succ" (Var "Nat")])

    it "parses recursive-inductive universe def" $ do
      parseDecl "rec (V,T) : ΣX:U.X -> U = (Sum(nat | pi (Σ x : V . T x → V)) , fun (nat → Nat | pi (x, f ) → Π y : T x . T (f y)))"
       `shouldBe` RDecl (Pat (B "V") (B "T"))
                  (Sigma (B "X") U
                   (Pi Wildcard (Var "X") U))
                  (Pair
                   (Sum [ Choice "nat" One
                        , Choice "pi" (Sigma (B "x") (Var "V")
                                     (Pi Wildcard
                                      (Ap (Var "T") (Var "x"))
                                      (Var "V")))
                        ])
                    (Case [ Choice "nat" (Abs Wildcard (Var "Nat"))
                          , Choice "pi" (Abs (Pat (B "x") (B "f"))
                                          (Pi (B "y")
                                            (Ap (Var "T") (Var "x"))
                                            (Ap (Var "T")
                                              (Ap (Var "f") (Var "y")))))
                          ]))

    it "parses natRec " $ do

      parseProgram False "rec natrec : Π C : Nat → U . C $zero → (Π n : Nat.C n → C ($succ n)) → Π n : Nat . C n = λ C . λ a . λ g . fun (zero → a | succ n1 → (g n1) ((((natrec C) a) g) n1)); ()"
      `shouldBe` (Def
                  (RDecl (B "natrec")
                   (Pi (B "C")
                    (Pi Wildcard (Var "Nat") U)
                    (Pi Wildcard (Ap (Var "C") (Ctor "zero" Unit))
                     (Pi Wildcard
                      (Pi (B "n")
                       (Var "Nat")
                       (Pi Wildcard (Ap (Var "C") (Var "n"))
                        (Ap (Var "C")
                         (Ctor "succ" (Var "n")))))
                       (Pi (B "n")
                        (Var "Nat")
                        (Ap (Var "C") (Var "n"))))))
                    (Abs (B "C")
                     (Abs (B "a")
                      (Abs (B "g")
                       (Case [Choice "zero" (Abs Wildcard (Var "a"))
                             ,Choice "succ" (Abs (B "n1")
                                             (Ap (Ap (Var "g") (Var "n1"))
                                               (Ap (Ap (Ap (Ap (Var "natrec")
                                                            (Var "C"))
                                                         (Var "a"))
                                                     (Var "g"))
                                                 (Var "n1"))))])))))
                   Unit)

    it "parses a program" $ do
      parseProgram False "rec Nat : U = Sum (zero | succ Nat) ;\nid : Π A : U . Π _ : A . A = λ A . λ x . x; ()"
        `shouldBe` (Def (RDecl (B "Nat") U (Sum [Choice "zero" One,Choice "succ" (Var "Nat")]))
                    (Def (Decl (B "id")
                           (Pi (B "A") U (Pi Wildcard (Var "A") (Var "A")))
                           (Abs (B "A") (Abs (B "x") (Var "x"))))
                      Unit))

    it "" $ do
      parseProgram False "Unit : U = Sum (tt); elimUnit : Π C : Unit -> U. C $tt -> Π x:Unit. C x = λ C . λ h . fun (tt -> h); ()"
        `shouldBe` (Def (Decl (B "Unit") U (Sum [Choice "tt" One])) (Def (Decl (B "elimUnit") (Pi (B "C") (Pi Wildcard (Var "Unit") U) (Pi Wildcard (Ap (Var "C") (Ctor "tt" Unit)) (Pi (B "x") (Var "Unit") (Ap (Var "C") (Var "x"))))) (Abs (B "C") (Abs (B "h") (Case [Choice "tt" (Abs Wildcard (Var "h"))])))) Unit))

  describe "Pretty-printing Expressions" $ do

    it "pretty prints simple expressions" $ do
      show (pretty (parseProgram False "Π abc : U . abc") )
        `shouldBe` "Π abc : U . abc"
      show (pretty (parseProgram False "λ abc . π1.(abc,feg)") )
        `shouldBe` "λ abc . π1.(abc, feg)"
      show (pretty (parseProgram False "λ (abc, (x,y)) . π1.($true, $false)") )
        `shouldBe` "λ (abc, (x, y)) . π1.($true, $false)"

    it "pretty prints declarations" $ do
      show (pretty (parseProgram False "id : Π A : U . Π _ : A . A = λ A . λ x . x ;\n()") )
        `shouldBe` "id : Π A : U . A → A = λ A . λ x . x ;\n()"
      show (pretty (parseProgram False "rec Nat : U = Sum (zero | succ Nat) ;\nid : Π A : U . Π _ : A . A = λ A . λ x . x; ()"))
        `shouldBe` "rec Nat : U = Sum(zero| succ Nat) ;\nid : Π A : U . A → A = λ A . λ x . x ;\n()"
      show (pretty (parseProgram False "rec natrec : Π C : Nat → U . C $zero → (Π n : Nat.C n → C ($succ n)) → Π n : Nat . C n = λ C . λ a . λ g . fun (zero → a | succ n1 → (g n1) (natrec C a g n1)); ()"))
        `shouldBe` "rec natrec : Π C : Nat → U . (C $zero) → Π n : Nat . (C n) → (C ($succ n)) → Π n : Nat . (C n) = λ C . λ a . λ g . fun(zero → λ _ . a| succ → λ n1 . ((g n1) ((((natrec C) a) g) n1))) ;\n()"
