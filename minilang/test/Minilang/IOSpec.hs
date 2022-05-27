module Minilang.IOSpec where

import qualified Data.Text as Text
import Data.Text.IO
import Minilang.IO
import Minilang.REPLSpec (withTempFile)
import System.IO (IOMode (..), withFile)
import Test.Hspec
import Prelude hiding (lines, readFile, writeFile)

spec :: Spec
spec = parallel $
    describe "MiniLang I/O Evaluator" $ do
        around withTempFile $
            it "evaluates a MiniLang program and dumps result IO" $ \fileName -> do
                let outputFileName = fileName <> ".out"
                    program =
                        [ "let Unit : U = Sum(tt);"
                        , "let elimUnit : Π C : Unit -> U. C tt -> Π x:Unit. C x = λ C . λ h . case (tt -> h);"
                        , ""
                        , "let Bool : U = Sum (true| false) ;"
                        , "let elimBool : Π C : Bool → U . C false → C true → Π b : Bool . C b  ="
                        , "  λ C . λ h0 . λ h1 . case (true → h1 | false → h0);"
                        , "let not : Bool → Bool = case (true → false | false → true);"
                        , "()"
                        ]

                writeFile fileName (Text.unlines program)

                _ <- withFile fileName ReadMode $ \hin ->
                    withFile outputFileName AppendMode $ \hout ->
                        runEval hin hout

                out <- readFile outputFileName

                out `shouldBe` "()\n"
