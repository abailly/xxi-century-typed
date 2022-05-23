{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.REPL where

import Control.Monad.Catch.Pure (CatchT (..))
import Control.Monad.State
import Data.Text hiding (replicate)
import Minilang.REPL.Haskeline
import Minilang.REPL.IO
import Minilang.REPL.Pure
import Minilang.REPL.Purer
import Minilang.REPL.Run
import Minilang.REPL.Types
import System.Console.Haskeline (
    defaultBehavior,
    defaultSettings,
    historyFile,
    runInputTBehavior,
    setComplete,
 )
import System.IO (Handle)

-- * Haskeline REPL

withTerminal :: IO ()
withTerminal =
    runInputTBehavior defaultBehavior settings $ evalStateT (runHaskeline runREPL) initialREPL
  where
    settings = setComplete completion $ defaultSettings{historyFile = Just "~/.minilang.history"}

-- * Other REPLs

{- | Pure IO REPL
 Used for testing purposes
-}

-- |
withHandles ::
    Handle -> Handle -> IO ()
withHandles hin hout =
    evalStateT (runConsole runREPL) (IOEnv hin hout initialREPL)

-- * Pure REPL

withInput ::
    [Text] -> [Text]
withInput stream =
    outputText $ execState (runCatchT (runPure runREPL)) (PureEnv stream [] initialREPL)
-- * Purer REPL

-- This useful for rehydrating and environment from a sequence of inputs

withInputs ::
    REPLEnv -> [In] -> PurerEnv
withInputs initEnv stream =
    execState (runCatchT (runPurer runREPL)) (PurerEnv stream [] initEnv)
