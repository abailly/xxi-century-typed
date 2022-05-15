{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.REPL where

import Control.Monad.Catch
import Control.Monad.Catch.Pure (CatchT (..))
import Control.Monad.State
import Data.Text hiding (replicate)
import qualified Data.Text as Text
import Debug.Trace (trace)
import Minilang.Env
import Minilang.Eval hiding (rho)
import Minilang.Parser
import Minilang.REPL.Haskeline
import Minilang.REPL.IO
import Minilang.REPL.Pure
import Minilang.REPL.Purer
import Minilang.REPL.Types
import Minilang.Type
import System.Console.Haskeline
  ( defaultBehavior,
    defaultSettings,
    historyFile,
    runInputTBehavior,
    setComplete,
  )
import System.IO (Handle)
import Text.Parsec (runParser)

-- | Run a REPL session
runREPL ::
  (TypeChecker m, MonadCatch m, MonadREPL m) => m ()
runREPL = go
  where
    go = do
      prompt
      raw <- input
      case raw of
        EOF -> output Bye
        In txt -> handleUserInput txt >> go
        Com cmd -> handleCommand cmd >> go

handleUserInput ::
  (TypeChecker m, MonadCatch m, MonadREPL m) =>
  Text ->
  m ()
handleUserInput txt = do
  repl@REPLEnv {rho = ρ, gamma = γ, debugParser} <- getEnv
  case runParser single_decl (ParserState debugParser) "" (unpack txt) of
    Right dec -> do
      let (sym, typ) = case dec of
            Decl b t _ -> (b, t)
            RDecl b t _ -> (b, t)
      do
        γ' <- checkD 0 dec ρ γ
        let ρ' = extend dec ρ
        setEnv (repl {rho = ρ', gamma = γ'})
        output (Defined sym typ)
        `catch` \(TypingError err) -> output (Msg err)
    Left _ ->
      case runParser expr (ParserState debugParser) "" (unpack txt) of
        Left err -> output (Msg $ pack $ show err)
        Right e ->
          ( do
              t <- trace ("type checking " <> show e) $ checkI 0 e ρ γ
              let v = eval e ρ
              output (Evaluated v t)
          )
            `catch` \(TypingError err) -> output (Msg err)

handleCommand ::
  (TypeChecker m, MonadCatch m, MonadREPL m) =>
  Command ->
  m ()
handleCommand ClearEnv = getEnv >>= \e -> setEnv (e {rho = EmptyEnv, gamma = EmptyContext})
handleCommand DumpEnv =
  getEnv >>= \REPLEnv {..} ->
    output (CurrentEnv rho gamma)
handleCommand (Load (File file)) = do
  t <- load file
  case t of
    Left err -> output (Msg $ pack $ show err)
    Right prog -> loadProgramInEnv prog
handleCommand (Load (Data prog)) = loadProgramInEnv prog
handleCommand (Set (StepTypeChecker st)) = getEnv >>= \e -> setEnv (e {stepTypeCheck = st})
handleCommand (Set (DebugParser st)) = getEnv >>= \e -> setEnv (e {debugParser = st})
handleCommand Help =
  output (Msg helpText)

loadProgramInEnv ::
  (TypeChecker m, MonadCatch m, MonadREPL m) =>
  Text ->
  m ()
loadProgramInEnv prog = do
  repl@REPLEnv {rho = ρ, gamma = γ, debugParser} <- getEnv
  case runParser program (ParserState debugParser) "" (unpack prog) of
    Left err -> output (Msg $ pack $ show err)
    Right e ->
      ( do
          (ρ', γ') <- loadProgram e ρ γ
          setEnv (repl {rho = ρ', gamma = γ'})
          output (Msg makeMessage)
      )
        `catch` \(TypingError err) -> output (Msg err)
  where
    makeMessage =
      "Loaded \""
        <> ( if Text.length prog > 15
               then Text.take 15 prog <> "..."
               else prog
           )
        <> "\""

-- * Haskeline REPL

withTerminal :: IO ()
withTerminal =
  runInputTBehavior defaultBehavior settings $ evalStateT (runHaskeline runREPL) initialREPL
  where
    settings = setComplete completion $ defaultSettings {historyFile = Just "~/.minilang.history"}

-- * Other REPLs

-- | Pure IO REPL
-- Used for testing purposes

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
