{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.REPL where

import           Control.Monad.Catch
import           Control.Monad.Catch.Pure              (CatchT (..))
import           Control.Monad.State
import           Data.Text                             hiding (replicate)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Minilang.Env
import           Minilang.Eval                         hiding (rho)
import           Minilang.Parser
import           Minilang.REPL.Haskeline
import           Minilang.REPL.IO
import           Minilang.REPL.Pure
import           Minilang.REPL.Types
import           Minilang.Type
import           System.Console.Haskeline              (defaultBehavior,
                                                        defaultSettings,
                                                        historyFile,
                                                        runInputTBehavior,
                                                        setComplete)
import           System.IO                             (Handle)
import           Text.Parsec                           (runParser)

-- | Run a REPL session
runREPL
  :: (TypeChecker m, MonadCatch m, MonadREPL m) => m ()
runREPL = go
  where
    go = do
      prompt
      raw <- input
      case raw of
        EOF     -> output "Bye!"
        In txt  -> handleUserInput txt >> go
        Com cmd -> handleCommand cmd >> go

handleUserInput
  :: (TypeChecker m, MonadCatch m, MonadREPL m)
  => Text -> m ()
handleUserInput txt = do
  env@REPLEnv{rho=ρ,gamma=γ,debugParser} <- getEnv
  case runParser single_decl (ParserState debugParser) "" (unpack txt) of
    Right dec -> do
      let
        (sym, typ) = case dec of
                       Decl b t _  -> (b,t)
                       RDecl b t _ -> (b,t)
      (do
          γ' <- checkD 0 dec ρ γ
          let ρ' = extend dec ρ
          setEnv (env { rho = ρ', gamma = γ' })
          output (renderStrict $ layoutPretty defaultLayoutOptions $ "defined " <> pretty sym <> " : " <> pretty typ))
        `catch` \ (TypingError err) -> output err
    Left _ ->
      case runParser expr (ParserState debugParser) "" (unpack txt) of
        Left err   -> output (pack $ show err)
        Right e -> do
          (do
              t <- checkI 0 e ρ γ
              let v = eval e ρ
              output (renderStrict $ layoutPretty defaultLayoutOptions $ (pretty v <+> "::" <+> pretty t)))
            `catch` \ (TypingError err) -> output err

handleCommand
  :: (TypeChecker m, MonadCatch m, MonadREPL m)
  => Command -> m ()
handleCommand ClearEnv = getEnv >>= \e -> setEnv (e { rho = EmptyEnv, gamma =  EmptyContext })
handleCommand DumpEnv  = getEnv >>= \ REPLEnv{..} -> do
  output (renderStrict $ layoutPretty defaultLayoutOptions $ "Environment: " <> pretty rho)
  output (renderStrict $ layoutPretty defaultLayoutOptions $ "Context: " <> pretty gamma)
handleCommand (Load file) = do
  t <- load file
  case t of
    Left err -> output (pack $ show err)
    Right prog -> do
      env@REPLEnv{rho=ρ,gamma=γ,debugParser} <- getEnv
      case runParser program (ParserState debugParser) "" (unpack prog) of
        Left err   -> output (pack $ show err)
        Right e -> do
          (do
              (ρ',γ') <- loadProgram e ρ γ
              setEnv (env { rho = ρ', gamma = γ' }))
            `catch` \ (TypingError err) -> output err

handleCommand (Set (StepTypeChecker st)) = getEnv >>= \e -> setEnv (e { stepTypeCheck = st })
handleCommand (Set (DebugParser st)) = getEnv >>= \e -> setEnv (e { debugParser = st })
handleCommand Help =
  output helpText

-- * Haskeline REPL

withTerminal :: IO ()
withTerminal =
  runInputTBehavior defaultBehavior settings $ evalStateT (runHaskeline runREPL) initialREPL
  where
    settings = setComplete completion $ defaultSettings { historyFile = Just "~/.minilang.history" }


-- * Other REPLs

-- | Pure IO REPL
-- Used for testing purposes

-- |
withHandles
  :: Handle -> Handle -> IO ()
withHandles hin hout =
  evalStateT (runConsole runREPL) (IOEnv hin hout initialREPL)


-- * Pure REPL

withInput
  :: [Text] -> [Text]
withInput stream =
  outputText $ execState (runCatchT (runPure runREPL)) (PureEnv stream [] initialREPL)
