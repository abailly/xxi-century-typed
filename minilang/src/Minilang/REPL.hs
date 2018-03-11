{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.REPL where

import           Control.Arrow                           ((&&&))
import           Control.Exception                       (throw)
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure                (CatchT (..))
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans                     (lift)
import           Data.Text                               hiding (replicate)
import           Data.Text.IO
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Minilang.Eval                           hiding (rho)
import           Minilang.Parser
import           Minilang.Type
import           System.Console.Haskeline                (InputT,
                                                          defaultBehavior,
                                                          defaultSettings,
                                                          getInputLineWithInitial,
                                                          historyFile,
                                                          outputStrLn,
                                                          runInputTBehavior)
import qualified System.Console.Haskeline.MonadException as Exc
import           System.IO                               (Handle, hFlush)
import           System.IO.Error                         (isEOFError)
import           Text.Parsec                             (runParser)



class (Monad m) => MonadREPL m where
  -- | Clears the current environment for this session and initializes with
  -- `EmptyEnv` and `EmptyContext`
  clear  :: m ()

  -- | Returns the current environment for this session
  -- Environment is made of an `Env`ironment binding names to `Decl`arations
  -- and typing `Context` binding names to `Value`s denoting evaluated types.
  getEnv :: m (Env, Context)

  -- | Updates the current environment for this session.
  setEnv :: Env -> Context -> m ()

  -- | Handle some input, parsing, typechecking and possibly evaluating it.
  input  :: m In

  -- | Produces some output
  output :: Text -> m ()

  -- | Prompt for some input
  prompt :: m ()

-- | Input
data In = EOF
        | In Text

-- | Run a REPL session
runREPL
  :: (TypeChecker m, MonadCatch m, MonadREPL m) => m ()
runREPL = go
  where
    go = do
      prompt
      raw <- input
      case raw of
        EOF -> output "Bye!"
        In txt ->
          case runParser single_decl (ParserState False) "" (unpack txt) of
            Right dec -> do
              let
                (sym, typ) = case dec of
                               Decl b t _  -> (b,t)
                               RDecl b t _ -> (b,t)
              (ρ, γ) <- getEnv
              (do
                  γ' <- checkD 0 dec ρ γ
                  let ρ' = extend dec ρ
                  setEnv ρ' γ'
                  output (renderStrict $ layoutPretty defaultLayoutOptions $ "defined " <> pretty sym <> " : " <> pretty typ))
                `catch` \ (TypingError err) -> output err
              go
            Left _ ->
              case runParser expr (ParserState False) "" (unpack txt) of
                Left err   -> output (pack $ show err) >> go
                Right e -> do
                  (ρ, γ) <- getEnv
                  (do
                      t <- checkI 0 e ρ γ
                      let v = eval e ρ
                      output (renderStrict $ layoutPretty defaultLayoutOptions $ (pretty v <> "::" <> pretty t)))
                    `catch` \ (TypingError err) -> output err
                  go



-- * IO REPL

data REPLEnv = REPLEnv { inputHandle  :: Handle
                       , outputHandle :: Handle
                       , rho          :: Env
                       , gamma        :: Context
                       }

newtype CONSOLE m a = CONSOLE { runConsole :: StateT REPLEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState REPLEnv)

instance MonadREPL (CONSOLE IO) where
  input      = get >>= lift . (\ h -> (In <$> hGetLine h) `catch` \ (isEOFError -> True) -> pure EOF) . inputHandle
  output a   = get >>= lift . flip hPutStrLn a . outputHandle
  prompt     = get >>= lift . (\ h -> hPutStr h "λΠ> " >> hFlush h) . outputHandle
  clear      = modify $ \ e  -> e { rho = EmptyEnv, gamma = EmptyContext }
  getEnv     = get >>= pure . (rho &&& gamma)
  setEnv ρ γ = modify $ \ e -> e { rho = ρ, gamma = γ }

instance (MonadThrow m) => MonadThrow (CONSOLE m)  where
  throwM = lift . throwM

instance (MonadCatch m) => MonadCatch (CONSOLE m)  where
  CONSOLE m `catch` f = CONSOLE $ m `catch` \ e -> runConsole (f e)

instance TypeChecker (CONSOLE IO) where
  emit = const $ pure()

withHandles
  :: Handle -> Handle -> IO ()
withHandles hin hout =
  evalStateT (runConsole runREPL) (REPLEnv hin hout EmptyEnv EmptyContext)

-- * Haskeline REPL

data ConsoleEnv = ConsoleEnv { rho'          :: Env
                             , gamma'        :: Context
                             , curIndent     :: Int
                             , stepTypeCheck :: Bool
                             }

newtype Haskeline a = Haskeline { runHaskeline :: StateT ConsoleEnv (InputT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState ConsoleEnv)

hoist :: InputT IO a -> Haskeline a
hoist m = Haskeline $ StateT $ \ s -> m >>= \ a -> pure (a,s)

instance MonadREPL Haskeline where
  input      = maybe EOF (In . pack) <$> hoist (getInputLineWithInitial "λΠ> " ("",""))
  output     = hoist . outputStrLn . unpack
  prompt     = pure ()

  clear      = modify $ \ e  -> e { rho' = EmptyEnv, gamma' = EmptyContext }
  getEnv     = get >>= pure . (rho' &&& gamma')
  setEnv ρ γ = modify $ \ e -> e { rho' = ρ, gamma' = γ }

instance MonadThrow (InputT IO) where
  throwM = Exc.throwIO

instance MonadCatch (InputT IO) where
  m `catch` f = m `Exc.catch` f

instance MonadCatch Haskeline where
  Haskeline m `catch` f =  Haskeline $ m `catch` \ e -> runHaskeline (f e)

instance MonadThrow Haskeline where
  throwM e = throw e

instance TypeChecker Haskeline where
  emit e = get >>= hoist . (printE e) >>= put

printE
  :: Event -> ConsoleEnv -> InputT IO ConsoleEnv
printE e env@ConsoleEnv{..} =
  let
    prefix depth = replicate (2 * depth) ' ' <> display e
    doPrint depth = void (getInputLineWithInitial (prefix depth) ("", ""))
  in
    if stepTypeCheck
    then case e of
      (CheckD CheckingDecl{})      -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (CheckD BoundType{})         -> doPrint (curIndent - 1) >> pure (env { curIndent = curIndent - 1})
      (CheckT CheckingIsType{})    -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (CheckT CheckedIsType {})    -> doPrint (curIndent - 1) >> pure (env { curIndent = curIndent - 1})
      (Check  CheckingHasType{})   -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (Check  CheckedHasType{} )   -> doPrint (curIndent - 1) >> pure (env { curIndent = curIndent - 1})
      (CheckI InferringType {})    -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (CheckI ResolvingVariable{}) -> doPrint curIndent >> pure (env { curIndent = curIndent + 1})
      (CheckI InferredType{})      -> doPrint (curIndent - 1) >> pure (env { curIndent = curIndent - 1})
    else pure env

withTerminal :: IO ()
withTerminal =
  runInputTBehavior defaultBehavior settings $ evalStateT (runHaskeline runREPL) (ConsoleEnv EmptyEnv EmptyContext 0 False)
  where
    settings = defaultSettings { historyFile = Just "~/.minilang.history" }

-- * Pure REPL

data PureEnv = PureEnv { inputText  :: [Text]
                       , outputText :: [Text]
                       , env        :: Env
                       , context    :: Context
                       }

newtype PureREPL a = PureREPL { runPure :: CatchT (State PureEnv) a }
  deriving (Functor, Applicative, Monad, MonadState PureEnv, MonadCatch)

instance MonadREPL PureREPL where
  input = do
    ins <- inputText <$> get
    case ins of
      []     -> pure EOF
      (t:ts) -> modify (\ e -> e { inputText = ts }) >> pure (In t)

  output t = modify $ \ e -> e { outputText = t : outputText e }
  prompt     = pure ()
  clear      = modify $ \ e  -> e { env = EmptyEnv, context = EmptyContext }
  getEnv     = get >>= pure . (env &&& context)
  setEnv ρ γ = modify $ \ e -> e { env = ρ, context = γ }


instance MonadThrow PureREPL where
  throwM e = throw e

instance TypeChecker PureREPL where
  emit _ = pure ()

withInput
  :: [Text] -> [Text]
withInput stream =
  outputText $ execState (runCatchT (runPure runREPL)) (PureEnv stream [] EmptyEnv EmptyContext)
