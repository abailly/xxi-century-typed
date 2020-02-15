module Minilang.IOSpec where

import           Data.Monoid       ((<>))
import qualified Data.Text         as Text
import           Data.Text.IO
import           Minilang.IO
import           Minilang.REPLSpec (withTempFile)
import           Prelude           hiding (lines, readFile, writeFile)
import           System.IO         (IOMode (..), withFile)
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "MiniLang I/O Evaluator" $ do

  around withTempFile $
    it "evaluates a MiniLang program and dumps result IO" $ \ fileName -> do
    let
      outputFileName = fileName <> ".out"
      program = [ "def Unit : U = Sum(tt);"
                , "def elimUnit : Π C : Unit -> U. C $tt -> Π x:Unit. C x = λ C . λ h . case (tt -> h);"
                , ""
                , "def Bool : U = Sum (true| false) ;"
                , "def elimBool : Π C : Bool → U . C $false → C $true → Π b : Bool . C b  ="
                , "  λ C . λ h0 . λ h1 . case (true → h1 | false → h0);"
                , "def not : Bool → Bool = case (true → $false | false → $true);"
                , "()"
                ]

    writeFile fileName (Text.unlines program)

    _ <- withFile fileName ReadMode      $ \ hin  ->
      withFile outputFileName AppendMode $ \ hout ->
      runEval hin hout

    out <- readFile outputFileName

    out `shouldBe` "()\n"
