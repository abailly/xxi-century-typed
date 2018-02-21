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
      out = withInput ["id : Π A : U . Π _ : A . A = λ A . λ x . x; id Nat 12"] runREPL

    out `shouldBe`
      ["NI 12"]

  around withTempFile $
    it "evaluates single MiniLang term read from IO" $ \ fileName -> do
    let
      outputFileName = fileName <> ".out"

    writeFile fileName "id : Π A : U . Π _ : A . A = λ A . λ x . x; id Nat 12"

    _ <- withFile fileName ReadMode      $ \ hin  ->
      withFile outputFileName AppendMode $ \ hout ->
      withHandles hin hout runREPL

    out <- readFile outputFileName

    out `shouldBe`
      "NI 12\n"

withTempFile :: (String -> IO ()) -> IO ()
withTempFile =
  bracket mkTempFile rmTempFile
  where
    mkTempFile    = mkstemp "test-repl" >>= \ (fp, h) -> hClose h >> pure fp
    rmTempFile fn = removeFile fn >> removeFile (fn <> ".out")