{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns   #-}
module Minilang.REPL.Types where

import           Control.Exception         (Exception)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text                 as Text hiding (replicate)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics
import           Minilang.Env
import           Minilang.Eval             hiding (rho)
import           Minilang.Parser           (AST, Binding)
import           Minilang.Pretty           ()


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
  output :: Out -> m ()

  -- | Prompt for some input
  prompt :: m ()

  -- | Read given file
  load :: Text -> m (Either REPLError Text)

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
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Command = ClearEnv
    | DumpEnv
    | Set Flag
    | Load Text
    | Help
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Out = Defined Binding AST
    | Evaluated Value Value
    | CurrentEnv Env Context
    | Msg Text
    | Bye
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Pretty Out where
  pretty (Defined b a)      = "defined" <+> pretty b <+> colon <+> pretty a
  pretty (Msg txt)          = pretty txt
  pretty (Evaluated v t)    = pretty v <+> "::" <+> pretty t
  pretty (CurrentEnv e c)   = vcat [ "Environment:" <+> pretty e
                                   , "Context:" <+> pretty c
                                   ]
  pretty Bye                = "Bye!"

data Flag = StepTypeChecker Bool
    | DebugParser Bool
    deriving (Eq, Show, Generic, ToJSON, FromJSON)


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
interpret (unpack -> (':':'l':' ':file)) = Com $ Load $ strip $ pack file
interpret (unpack -> (':':'l':'o':'a':'d':' ':file)) = Com $ Load $ strip $ pack file
interpret t                           = In t


helpText :: Text
helpText =
  Text.unlines [ "REPL Commands Help:"
               , "   :h(elp)          : Display this help"
               , "   :q(uit)          : Exit the interpreter"
               , "   :e(nv)           : Displays the current interpreter's environment"
               , "                      This dumps both the Environment, where values are defined,"
               , "                      and the typing Context, where types are defined. "
               , "   :c(lear)         : Clear the current interpreter's environment"
               , "   :set <option>    : Set an option modifying the interpreter's behaviour"
               , "   :unset <option>  : Remove an option that's been set previously"
               , "   :l(oad) <file>   : load content of <file> into the interpreter, parsing and type-checking it"
               , ""
               , "Options: "
               , "   step             : Run the typechecker step-by-step, pausing at each rule application and"
               , "                      waiting for user to hit <enter> key to continue"
               , "   debug            : Debug parser, displaying the trail of rules tried and triggered"
               ]
