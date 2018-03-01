{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Minilang.REPL where

import           Control.Arrow                         ((&&&))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans                   (lift)
import           Data.Text
import           Data.Text.IO
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Minilang.Eval                         hiding (rho)
import           Minilang.Parser
import           Minilang.Type
import           System.IO                             (Handle, hFlush)
import           Text.Parsec

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
  input  :: m Text

  -- | Produces some output
  output :: Text -> m ()

  -- | Prompt for some input
  prompt :: m ()


-- | Run a REPL session
runREPL
  :: (TypeChecker m, MonadCatch m, MonadREPL m) => m ()
runREPL = go ""
  where
    -- | @buf@ accumulates input to be parsed
    go buf = do
      prompt
      txt <- (buf <>) <$> input
      case runParser single_decl () "" (unpack txt) of
        Right dec -> do
          let
            (sym, typ) = case dec of
                           Decl b t _  -> (b,t)
                           RDecl b t _ -> (b,t)
          (ρ, γ) <- getEnv
          γ'     <- checkD 0 dec ρ γ
          let ρ' = extend dec ρ
          setEnv ρ' γ'
          output (renderStrict $ layoutPretty defaultLayoutOptions $ "defined " <> pretty sym <> " : " <> pretty typ)
          go ""
        Left _ -> do
          case runParser expr () "" (unpack txt) of
            Left err   -> output (pack $ show err) >> go ""
            Right e -> do
              (ρ, γ) <- getEnv
              (do
                  t <- checkI 0 e ρ γ
                  let v = eval e ρ
                  output (renderStrict $ layoutPretty defaultLayoutOptions $ (pretty v <> "::" <> pretty t)))
                `catch` \ (TypingError err) -> output err
              go ""



-- * IO REPL

data REPLEnv = REPLEnv { inputHandle  :: Handle
                       , outputHandle :: Handle
                       , rho          :: Env
                       , gamma        :: Context
                       }

newtype CONSOLE m a = CONSOLE { runConsole :: StateT REPLEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState REPLEnv)

instance MonadREPL (CONSOLE IO) where
  input    = get >>= lift . hGetLine . inputHandle
  output a = get >>= lift . flip hPutStrLn a . outputHandle
  clear    = modify $ \ e  -> e { rho = EmptyEnv, gamma = EmptyContext }
  getEnv   = get >>= pure . (rho &&& gamma)
  setEnv ρ γ = modify $ \ e -> e { rho = ρ, gamma = γ }
  prompt =  get >>= lift . (\ h -> hPutStr h "λΠ> " >> hFlush h) . outputHandle

instance (MonadThrow m) => MonadThrow (CONSOLE m)  where
  throwM = lift . throwM

instance (MonadCatch m) => MonadCatch (CONSOLE m)  where
  CONSOLE m `catch` f = CONSOLE $ m `catch` \ e -> runConsole (f e)

instance TypeChecker (CONSOLE IO) where
  emit = lift . emit

withHandles
  :: Handle -> Handle -> IO ()
withHandles hin hout =
  evalStateT (runConsole runREPL) (REPLEnv hin hout EmptyEnv EmptyContext)

-- -- * Pure REPL

-- instance MonadREPL (WriterT [Text] (State [Text])) where
--   input = do
--     ins <- get
--     case ins of
--       []     -> pure Nothing
--       (t:ts) -> put ts >> pure (Just t)

--   output t = tell [pack $ show t]

-- type InMemREPL a = (WriterT [Text] (State [Text])) a

-- withInput
--   :: [Text] -> InMemREPL a -> [Text]
-- withInput stream act =
--   case runState (runWriterT act) stream of
--     ((_, o), []) -> o
--     ((_, o), s)  -> o <> withInput s act
