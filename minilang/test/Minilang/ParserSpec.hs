module Minilang.ParserSpec where

import           Minilang.Parser
import           Test.Hspec

spec :: Spec
spec = describe "Minilang Core" $ do

  describe "Parsing Terms and Expressions" $ do

    it "parse Number" $ do
      parseMLExpr "12" `shouldBe` I 12
      parseMLExpr "12.4" `shouldBe` D 12.4

    it "parse Variable" $ do
      parseMLExpr "abc" `shouldBe` Var "abc"

    it "parse Universe" $ do
      parseMLExpr "U" `shouldBe` U
      parseMLExpr "Uabc" `shouldBe` Var "Uabc"

    it "parse Universe" $ do
      parseMLExpr "U" `shouldBe` U

    it "parse Unit" $ do
      parseMLExpr "()" `shouldBe` Unit

    it "parse Application" $ do
      parseMLExpr "abc fge" `shouldBe` Ap (Var "abc") (Var "fge")
      parseMLExpr "abc fge 12" `shouldBe` Ap (Var "abc") (Ap (Var "fge") (I 12))

    it "parse Abstraction" $ do
      parseMLExpr "λ abc . abc" `shouldBe` Abs (B "abc") (Var "abc")
      parseMLExpr "λ abc . abc ghe" `shouldBe` Abs (B "abc") (Ap (Var "abc") (Var "ghe"))
      parseMLExpr "λ _ . abc" `shouldBe` Abs Wildcard (Var "abc")

    it "parse Dependent Product" $ do
      parseMLExpr "Π abc : U . abc" `shouldBe` Pi (B "abc") U (Var "abc")
      parseMLExpr "Πabc:U.abc" `shouldBe` Pi (B "abc") U (Var "abc")

    it "parse Dependent Sum" $ do
      parseMLExpr "Σ abc : U . abc" `shouldBe` Sigma (B "abc") U (Var "abc")
      parseMLExpr "Σabc:   U. abc" `shouldBe` Sigma (B "abc") U (Var "abc")

    it "parse Projections" $ do
      parseMLExpr "π1.abc" `shouldBe` P1 (Var "abc")
      parseMLExpr "π2.abc" `shouldBe` P2 (Var "abc")

    it "parse Pairing" $ do
      parseMLExpr "(abc,12)" `shouldBe` Pair (Var "abc") (I 12)

  describe "Declarations" $ do

    it "generic id function" $ do
      parseML "id : Π A : U . Π _ : A . A = λ A . λ x . x"
        `shouldBe` Decl (B "id")
                   (Pi (B "A") U (Pi Wildcard (Var "A") (Var "A")))
                   (Abs (B "A") (Abs (B "x") (Var "x")))
