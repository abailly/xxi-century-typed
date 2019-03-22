module Main where

import           Minilang.IO
import           Minilang.REPL
import           System.IO             (stdin, stdout)
import           System.Posix.IO       (stdInput)
import           System.Posix.Terminal (queryTerminal)

main :: IO ()
main = do
  hasTty <- queryTerminal stdInput
  if hasTty
    then withTerminal
    else runEval stdin stdout
