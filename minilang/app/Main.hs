module Main where

import           Minilang.REPL
import           System.IO     (stdin, stdout)

main :: IO ()
main = withHandles stdin stdout
