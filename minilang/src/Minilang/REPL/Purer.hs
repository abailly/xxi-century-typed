{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minilang.REPL.Purer where


import           Control.Monad.Catch
import           Control.Monad.Catch.Pure (CatchT (..))
import           Control.Monad.State
import           Minilang.REPL.Types
import           Minilang.Type


data PurerEnv = PurerEnv
    { inputs    :: [In]
    , outputs   :: [Out]
    , purerREPL :: REPLEnv
    }

newtype PurerREPL a = PurerREPL { runPurer :: CatchT (State PurerEnv) a }
  deriving (Functor, Applicative, Monad, MonadState PurerEnv, MonadCatch, MonadThrow)

instance MonadREPL PurerREPL where
  input = do
    ins <- inputs <$> get
    case ins of
      []     -> pure EOF
      (t:ts) -> modify (\ e -> e { inputs = ts }) >> pure t

  output t  = modify $ \ e -> e { outputs = t : outputs e }
  prompt    = pure ()
  getEnv    = get >>= pure . purerREPL
  setEnv e' = modify $ \ e -> e { purerREPL = e' }
  load f    = pure $ Left $ REPLError $ "cannot load file " <> f <> " in Pure interpreter"

instance TypeChecker PurerREPL where
  emit _ = pure ()
