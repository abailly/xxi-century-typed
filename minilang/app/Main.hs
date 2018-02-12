module Main where

import           Minilang.REPL
import           System.IO     (stdin, stdout)

main :: IO ()
main = go
  where
    go = do
      res <- withHandles stdin stdout runREPL
      case res of
        Exiting  -> pure ()
        Parsed _ -> go
