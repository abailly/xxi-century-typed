{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minilang.REPL.Pure where


import           Control.Exception        (throw)
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure (CatchT (..))
import           Control.Monad.State
import           Data.Text                hiding (replicate)
import           Minilang.Pretty
import           Minilang.REPL.Types
import           Minilang.Type


data PureEnv = PureEnv
    { inputText  :: [Text]
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
      (t:ts) -> modify (\ e -> e { inputText = ts }) >> pure (interpret t)

  output t  = modify $ \ e -> e { outputText = render t : outputText e }
  prompt    = pure ()
  getEnv    = get >>= pure . pureREPL
  setEnv e' = modify $ \ e -> e { pureREPL = e' }
  load f    = pure $ Left $ REPLError $ "cannot load file " <> f <> " in Pure interpreter"

instance MonadThrow PureREPL where
  throwM e = throw e

instance TypeChecker PureREPL where
  emit _ = pure ()
