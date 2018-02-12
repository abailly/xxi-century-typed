module Minilang.REPLSpec where

import           Minilang.REPL
import           Test.Hspec

replSpec :: Spec
replSpec = parallel $ describe "MiniLang REPL" $ do

  it "evaluates MiniLang terms read from Handle" $ do
    let
      out = withInput ["id : Π A : U . Π _ : A . A = λ A . λ x . x"]
            runREPL

    out `shouldBe`
      ["Decl (B \"id\") (Pi (B \"A\") U (Pi Wildcard (Var \"A\") (Var \"A\"))) (Abs (B \"A\") (Abs (B \"x\") (Var \"x\")))"]
