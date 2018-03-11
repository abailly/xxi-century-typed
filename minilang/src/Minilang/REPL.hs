{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.REPL where

import           Control.Exception                       (throw)
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure                (CatchT (..))
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans                     (lift)
import           Data.Text                               hiding (replicate)
import           Data.Text.IO                            as Text
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
  -- | Returns the current environment for this session
  -- Environment is made of an `Env`ironment binding names to `Decl`arations
  -- and typing `Context` binding names to `Value`s denoting evaluated types.
  getEnv :: m REPLEnv

  -- | Updates the current environment for this session.
  setEnv :: REPLEnv -> m ()

  -- | Handle some input, parsing, typechecking and possibly evaluating it.
  input  :: m In

  -- | Produces some output
  output :: Text -> m ()

  -- | Prompt for some input
  prompt :: m ()

  -- | Read given file
  load :: FilePath -> m Text

-- | REPL state
data REPLEnv = REPLEnv { rho           :: Env
                       , gamma         :: Context
                       , curIndent     :: Int
                       , stepTypeCheck :: Bool
                       , debugParser   :: Bool
                       }

initialREPL :: REPLEnv
initialREPL = REPLEnv EmptyEnv EmptyContext 0 False False

-- | Input
data In = EOF
        | In Text
        | Com Command
        -- ^A REPL command

data Command = ClearEnv
             | DumpEnv
             | Set Flag
             | Load FilePath

data Flag = StepTypeChecker Bool
          | DebugParser Bool

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
              output (renderStrict $ layoutPretty defaultLayoutOptions $ (pretty v <> "::" <> pretty t)))
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
  env@REPLEnv{rho=ρ,gamma=γ,debugParser} <- getEnv
  case runParser program (ParserState debugParser) "" (unpack t) of
    Left err   -> output (pack $ show err)
    Right e -> do
      (do
          (ρ',γ') <- loadProgram e ρ γ
          setEnv (env { rho = ρ', gamma = γ' }))
        `catch` \ (TypingError err) -> output err




handleCommand (Set (StepTypeChecker st)) = getEnv >>= \e -> setEnv (e { stepTypeCheck = st })
handleCommand (Set (DebugParser st)) = getEnv >>= \e -> setEnv (e { debugParser = st })


-- * IO REPL

data IOEnv = IOEnv { inputHandle  :: Handle
                   , outputHandle :: Handle
                   , repl         :: REPLEnv
                   }

newtype CONSOLE m a = CONSOLE { runConsole :: StateT IOEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState IOEnv)

instance MonadREPL (CONSOLE IO) where
  input     = get >>= lift . (\ h -> (In <$> hGetLine h) `catch` \ (isEOFError -> True) -> pure EOF) . inputHandle
  output a  = get >>= lift . flip hPutStrLn a . outputHandle
  prompt    = get >>= lift . (\ h -> hPutStr h "λΠ> " >> hFlush h) . outputHandle
  getEnv    = get >>= pure . repl
  setEnv e' = modify $ \ e -> e { repl = e' }
  load      = lift . Text.readFile

instance (MonadThrow m) => MonadThrow (CONSOLE m)  where
  throwM = lift . throwM

instance (MonadCatch m) => MonadCatch (CONSOLE m)  where
  CONSOLE m `catch` f = CONSOLE $ m `catch` \ e -> runConsole (f e)

instance TypeChecker (CONSOLE IO) where
  emit = const $ pure()

withHandles
  :: Handle -> Handle -> IO ()
withHandles hin hout =
  evalStateT (runConsole runREPL) (IOEnv hin hout initialREPL)

-- * Haskeline REPL


newtype Haskeline a = Haskeline { runHaskeline :: StateT REPLEnv (InputT IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState REPLEnv)

hoist :: InputT IO a -> Haskeline a
hoist m = Haskeline $ StateT $ \ s -> m >>= \ a -> pure (a,s)

instance MonadREPL Haskeline where
  input  = do
    i <- hoist (getInputLineWithInitial "λΠ> " ("",""))
    case i of
      Nothing          -> pure EOF
      Just (pack -> t) -> pure $ interpret t
  output = hoist . outputStrLn . unpack
  prompt = pure ()
  load   = hoist . lift . Text.readFile

  getEnv = get
  setEnv = put

interpret
  :: Text -> In
interpret ":q"                        = EOF
interpret ":quit"                     = EOF
interpret ":e"                        = Com DumpEnv
interpret ":env"                      = Com DumpEnv
interpret ":c"                        = Com ClearEnv
interpret ":clear"                    = Com ClearEnv
interpret ":set step"                 = Com $ Set $ StepTypeChecker True
interpret ":unset step"               = Com $ Set $ StepTypeChecker False
interpret ":set debug"                = Com $ Set $ DebugParser True
interpret ":unset debug"              = Com $ Set $ DebugParser False
interpret (unpack -> (':':'l':' ':file)) = Com $ Load file
interpret (unpack -> (':':'l':'o':'a':'d':' ':file)) = Com $ Load file
interpret t                           = In t

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
  :: Event -> REPLEnv -> InputT IO REPLEnv
printE e env@REPLEnv{..} =
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
  runInputTBehavior defaultBehavior settings $ evalStateT (runHaskeline runREPL) initialREPL
  where
    settings = defaultSettings { historyFile = Just "~/.minilang.history" }

-- * Pure REPL

data PureEnv = PureEnv { inputText  :: [Text]
                       , outputText :: [Text]
                       , pureREPL   :: REPLEnv
                       }

newtype PureREPL a = PureREPL { runPure :: CatchT (State PureEnv) a }
  deriving (Functor, Applicative, Monad, MonadState PureEnv, MonadCatch)

instance MonadREPL PureREPL where
  input = do
    ins <- inputText <$> get
    case ins of
      []     -> pure EOF
      (t:ts) -> modify (\ e -> e { inputText = ts }) >> pure (In t)

  output t  = modify $ \ e -> e { outputText = t : outputText e }
  prompt    = pure ()
  getEnv    = get >>= pure . pureREPL
  setEnv e' = modify $ \ e -> e { pureREPL = e' }
  load      = undefined

instance MonadThrow PureREPL where
  throwM e = throw e

instance TypeChecker PureREPL where
  emit _ = pure ()

withInput
  :: [Text] -> [Text]
withInput stream =
  outputText $ execState (runCatchT (runPure runREPL)) (PureEnv stream [] initialREPL)
