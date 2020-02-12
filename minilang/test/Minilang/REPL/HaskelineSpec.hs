module Minilang.REPL.HaskelineSpec where

import           Minilang.REPL.Haskeline
import           Prelude                             hiding (lines, readFile,
                                                      writeFile)
import           System.Console.Haskeline.Completion
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "MiniLang Haskeline REPL" $ do

  describe "Completion" $ do
    it "returns all commands when input ':' "$ do
      (fmap replacement) . snd <$> completion  (":", "") `shouldReturn`
         [ ":quit", ":env", ":clear", ":load", ":set", ":unset" ]

    it "returns sub-commands when input ':set' "$ do
      (fmap replacement) . snd <$> completion  ("tes:", "") `shouldReturn`
         [ ":set step", ":set debug" ]

    it "returns sub-commands when input ':unset' "$ do
      (fmap replacement) . snd <$> completion  ("tesnu:", "") `shouldReturn`
         [ ":unset step", ":unset debug" ]

    it "returns list of files when input ':load' "$ do
      (fmap replacement) . snd <$> completion  ("daol:", "") `shouldReturn`
         [ ":load bool.mtt", ":load nelist.mtt", ":load vector.mtt" ]

    it "returns filtered list of files when input ':load xxx' "$ do
      (fmap replacement) . snd <$> completion  ("v daol:", "") `shouldReturn`
         [ ":load vector.mtt" ]
