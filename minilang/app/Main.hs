module Main where

import           Minilang.REPL
import           System.IO     (hFlush, stdin, stdout)

main :: IO ()
main = go
  where
    go = do
      putStr "λΠ> " >> hFlush stdout
      res <- withHandles stdin stdout runREPL
      case res of
        Exiting  -> pure ()
        Parsed _ -> go
