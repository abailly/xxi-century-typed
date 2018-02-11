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
