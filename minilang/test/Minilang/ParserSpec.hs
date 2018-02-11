module Minilang.ParserSpec where

import           Minilang.Parser
import           Test.Hspec

spec :: Spec
spec = describe "Minilang Core" $ do

  it "parse Number" $ do
    parseML "12" `shouldBe` I 12
    parseML "12.4" `shouldBe` D 12.4

  it "parse Variable" $ do
    parseML "abc" `shouldBe` Var "abc"

  it "parse Universe" $ do
    parseML "U" `shouldBe` U

  it "parse Application" $ do
    parseML "abc fge" `shouldBe` Ap (Var "abc") (Var "fge")
    parseML "abc fge 12" `shouldBe` Ap (Var "abc") (Ap (Var "fge") (I 12))

  it "parse Abstraction" $ do
    parseML "λ abc . abc" `shouldBe` Abs (B "abc") (Var "abc")
    parseML "λ abc . abc ghe" `shouldBe` Abs (B "abc") (Ap (Var "abc") (Var "ghe"))
    parseML "λ _ . abc" `shouldBe` Abs Wildcard (Var "abc")

  it "parse Dependent Product" $ do
    parseML "Π abc : U . abc" `shouldBe` Pi (B "abc") U (Var "abc")
