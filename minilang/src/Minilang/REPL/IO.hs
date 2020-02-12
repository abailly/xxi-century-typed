{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module Minilang.REPL.IO where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans  (lift)
import           Data.Text            (unpack)
import           Data.Text.IO         as Text
import           Minilang.REPL.Types
import           Minilang.Type
import           System.IO            (Handle, hFlush)
import           System.IO.Error      (isEOFError)

data IOEnv = IOEnv
    { inputHandle  :: Handle
    , outputHandle :: Handle
    , repl         :: REPLEnv
    }

newtype CONSOLE m a = CONSOLE { runConsole :: StateT IOEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState IOEnv)

instance MonadREPL (CONSOLE IO) where
  input     = get >>= lift . (\ h -> (interpret <$> hGetLine h) `catch` \ (isEOFError -> True) -> pure EOF) . inputHandle
  output a  = get >>= lift . flip hPutStrLn a . outputHandle
  prompt    = get >>= lift . (\ h -> hPutStr h "λΠ> " >> hFlush h) . outputHandle
  getEnv    = get >>= pure . repl
  setEnv e' = modify $ \ e -> e { repl = e' }
  load      = lift . try . Text.readFile . unpack

instance (MonadThrow m) => MonadThrow (CONSOLE m)  where
  throwM = lift . throwM

instance (MonadCatch m) => MonadCatch (CONSOLE m)  where
  CONSOLE m `catch` f = CONSOLE $ m `catch` \ e -> runConsole (f e)

instance TypeChecker (CONSOLE IO) where
  emit = const $ pure()
