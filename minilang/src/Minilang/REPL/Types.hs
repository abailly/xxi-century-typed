module Minilang.REPL.Types where

import           Data.Text     hiding (replicate)
import           Minilang.Env
import           Minilang.Eval hiding (rho)


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
