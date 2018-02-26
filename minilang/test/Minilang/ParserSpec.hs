module Minilang.ParserSpec where

import           Minilang.Parser
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Minilang Core" $ do

  describe "Parsing Terms and Expressions" $ do

    it "parse Number" $ do
      parseProgram "12" `shouldBe` I 12
      parseProgram "12.4" `shouldBe` D 12.4

    it "parse Variable" $ do
      parseProgram "abc" `shouldBe` Var "abc"

    it "parse Universe" $ do
      parseProgram "U" `shouldBe` U
      parseProgram "Uabc" `shouldBe` Var "Uabc"

    it "parse Universe" $ do
      parseProgram "U" `shouldBe` U

    it "parse Unit" $ do
      parseProgram "()" `shouldBe` Unit

    it "parse Application" $
      parseProgram "abc fge" `shouldBe` Ap (Var "abc") (Var "fge")

    it "parse application as left-associative" $ do
      parseProgram "abc fge 12" `shouldBe` parseProgram "(abc fge) 12"
      parseProgram "abc fge 12 k" `shouldBe` parseProgram "((abc fge) 12) k"
      parseProgram "abc fge 12 k bool" `shouldBe` parseProgram "(((abc fge) 12) k) bool"

    it "parse Abstraction" $ do
      parseProgram "λ abc . abc" `shouldBe` Abs (B "abc") (Var "abc")
      parseProgram "λ abc . abc ghe" `shouldBe` Abs (B "abc") (Ap (Var "abc") (Var "ghe"))
      parseProgram "λ _ . abc" `shouldBe` Abs Wildcard (Var "abc")

    it "parse Dependent Product" $ do
      parseProgram "Π abc : U . abc" `shouldBe` Pi (B "abc") U (Var "abc")
      parseProgram "Πabc:U.abc" `shouldBe` Pi (B "abc") U (Var "abc")

    it "parse Dependent Sum" $ do
      parseProgram "Σ abc : U . abc" `shouldBe` Sigma (B "abc") U (Var "abc")
      parseProgram "Σabc:   U. abc" `shouldBe` Sigma (B "abc") U (Var "abc")
      parseProgram "Σ x : V . T x → V" `shouldBe` Sigma (B "x") (Var "V") (Pi Wildcard (Ap (Var "T") (Var "x")) (Var "V"))

    it "parse Projections" $ do
      parseProgram "π1.abc" `shouldBe` P1 (Var "abc")
      parseProgram "π2.abc" `shouldBe` P2 (Var "abc")

    it "parse Constructor application" $ do
      parseProgram "$c1 abc" `shouldBe` Ctor "c1" (Var "abc")

    it "parse Pairing" $ do
      parseProgram "(abc,12)" `shouldBe` Pair (Var "abc") (I 12)

    it "parses Labelled Sum" $ do
      parseProgram "Sum(foo []| bar Nat)" `shouldBe` Sum [ Choice "foo" One , Choice "bar" (Var "Nat")]
      parseProgram "Sum(foo[]| bar (Nat, Bool))" `shouldBe` Sum [ Choice "foo" One , Choice "bar" (Pair (Var "Nat") (Var "Bool"))]
      parseProgram "Sum(foo| bar)" `shouldBe` Sum [ Choice "foo" One , Choice "bar" One]
      parseProgram "Sum  (  foo  | bar  )" `shouldBe` Sum [ Choice "foo" One , Choice "bar" One]

    it "parses Case choices" $ do
      parseProgram "fun (foo 12 -> h1 | bar x -> h2)" `shouldBe` Case [ Choice "foo" (Abs (C $ I 12) (Var "h1")) , Choice "bar" (Abs (B "x") (Var "h2"))]
      parseProgram "fun (foo -> 12 | bar x -> h2)" `shouldBe` Case [ Choice "foo" (Abs Wildcard (I 12)) , Choice "bar" (Abs (B "x") (Var "h2"))]

    it "parses Pattern" $ do
      parseProgram "λ (abc, xyz) . abc" `shouldBe` Abs (Pat (B "abc") (B "xyz")) (Var "abc")
      parseProgram "Π (abc, xyz): U . abc" `shouldBe` Pi (Pat (B "abc") (B "xyz")) U (Var "abc")

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
                                            (Ap
                                              (Ap (Var "T") (Var "f"))
                                              (Var "y"))))
                          ]))

    it "parses natRec " $ do

      parseProgram "rec natrec : Π C : Nat → U . C $zero → (Π n : Nat.C n → C ($succ n)) → Π n : Nat . C n = λ C . λ a . λ g . fun (zero → a | succ n1 → (g n1) ((((natrec C) a) g) n1)); ()"
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
                                             (Ap (Ap (Ap (Ap (Ap (Ap (Var "g") (Var "n1")) (Var "natrec"))
                                                          (Var "C")) (Var "a")) (Var "g")) (Var "n1")))])))))
                   Unit)

    it "parses a program" $ do
      parseProgram "rec Nat : U = Sum (zero | succ Nat) ;\nid : Π A : U . Π _ : A . A = λ A . λ x . x; ()"
        `shouldBe` (Def (RDecl (B "Nat") U (Sum [Choice "zero" One,Choice "succ" (Var "Nat")]))
                    (Def (Decl (B "id")
                           (Pi (B "A") U (Pi Wildcard (Var "A") (Var "A")))
                           (Abs (B "A") (Abs (B "x") (Var "x"))))
                      Unit))
