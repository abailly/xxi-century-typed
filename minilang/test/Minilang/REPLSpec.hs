module Minilang.REPLSpec where

import           Control.Exception   (bracket)
import           Data.Monoid         ((<>))
import           Data.Text.IO
import           Minilang.Eval       (Value (EU), toList)
import           Minilang.REPL
import           Minilang.REPL.Purer
import           Minilang.REPL.Types
import           Prelude             hiding (lines, readFile, writeFile)
import           System.Directory    (getTemporaryDirectory, removeFile)
import           System.FilePath     ((<.>), (</>))
import           System.IO           (IOMode (..), hClose, withFile)
import           System.Posix.Temp   (mkstemp)
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
      , "$succ $succ $succ $zero  :: Sum(zero| succ Nat)"
      , "defined id : Π A : U . A → A"
      , "defined Nat : U"
      ]

  it "shows error when trying to load a non-existent file" $ do
    withInput [ ":load foo" ] `shouldBe` [ "Bye!"
                                         , "REPLError \"cannot load file foo in Pure interpreter\""
                                         ]

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
      "\955\928> defined Nat : U\n\955\928> defined id : \928 A : U . A \8594 A\n\955\928> $succ $succ $succ $zero  :: Sum(zero| succ Nat)\n\955\928> Bye!\n"

  describe "Purer REPL" $ do

    it "keeps handling Inputs even when they fail" $ do
      let inputs = [ In "Unit : U"
                   , In "Unit : U = Sum(tt)"
                   , Com DumpEnv
                   ]

      toList (gamma $ purerREPL (withInputs initialREPL inputs))
        `shouldBe` [("Unit",EU)]

      outputs (withInputs initialREPL inputs)
        `shouldContain` [ Msg "cannot find \"Unit\" in empty context" ]

withTempFile :: (String -> IO ()) -> IO ()
withTempFile =
  bracket mkTempFile rmTempFile
  where
    mkTempFile    = getTemporaryDirectory >>= \ dir -> mkstemp (dir </> "test-repl") >>= \ (fp, h) -> hClose h >> pure fp
    rmTempFile fn = removeFile fn >> removeFile (fn <.> "out")
