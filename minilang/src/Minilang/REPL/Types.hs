{-# LANGUAGE ViewPatterns #-}
module Minilang.REPL.Types where

import           Control.Exception (Exception)
import           Data.Text         hiding (replicate)
import           Minilang.Env
import           Minilang.Eval     hiding (rho)


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
  load :: FilePath -> m (Either REPLError Text)

-- | Possible error in REPL interactions
data REPLError = FileError IOError
    | REPLError Text
    deriving (Eq, Show)

instance Exception REPLError

-- | REPL state
data REPLEnv = REPLEnv
    { rho           :: Env
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
    | Help


data Flag = StepTypeChecker Bool
    | DebugParser Bool


-- | "Parse" given `Text` into some input for the REPL
interpret
  :: Text -> In
interpret ":q"                        = EOF
interpret ":quit"                     = EOF
interpret ":h"                        = Com Help
interpret ":help"                     = Com Help
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
