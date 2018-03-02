module Minilang.REPLSpec where

import           Control.Exception (bracket)
import           Data.Monoid       ((<>))
import           Data.Text.IO
import           Minilang.REPL
import           Prelude           hiding (lines, readFile, writeFile)
import           System.Directory  (removeFile)
import           System.IO         (IOMode (..), hClose, withFile)
import           System.Posix.Temp (mkstemp)
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "MiniLang REPL" $ do

  it "evaluates MiniLang terms read from pure Monad" $ do
    let
      out = withInput [ "rec Nat : U = Sum (zero | succ Nat);"
                      , "id : Π A : U . Π _ : A . A = λ A . λ x . x;"
                      , "id Nat ($succ ($succ ($succ $zero)))"
                      ]

    out `shouldBe`
      [ "Bye!"
      , "$succ $succ $succ $zero ()::Sum(zero| succ Nat, {Nat : U ↦ Sum(zero| succ Nat), ∅})"
      , "defined id : Π A : U . A → A"
      , "defined Nat : U"]

  around withTempFile $
    it "evaluates single MiniLang term read from IO" $ \ fileName -> do
    let
      outputFileName = fileName <> ".out"

    writeFile fileName "rec Nat : U = Sum (zero | succ Nat);\nid : Π A : U . Π _ : A . A = λ A . λ x . x;\nid Nat ($succ ($succ ($succ $zero)))"

    _ <- withFile fileName ReadMode      $ \ hin  ->
      withFile outputFileName AppendMode $ \ hout ->
      withHandles hin hout

    out <- readFile outputFileName

    out `shouldBe`
      "\955\928> defined Nat : U\n\955\928> defined id : \928 A : U . A \8594 A\n\955\928> $succ $succ $succ $zero ()::Sum(zero| succ Nat, {Nat : U \8614 Sum(zero| succ Nat), \8709})\n\955\928> Bye!\n"

withTempFile :: (String -> IO ()) -> IO ()
withTempFile =
  bracket mkTempFile rmTempFile
  where
    mkTempFile    = mkstemp "test-repl" >>= \ (fp, h) -> hClose h >> pure fp
    rmTempFile fn = removeFile fn >> removeFile (fn <> ".out")
