{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Minilang.REPL.Haskeline where


import           Control.Exception                       (throw)
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans                     (lift)
import           Data.Text                               hiding (replicate)
import           Data.Text.IO                            as Text
import           Data.Text.Prettyprint.Doc
import           Minilang.REPL.Types
import           Minilang.Type
import           System.Console.Haskeline                (InputT, getInputLineWithInitial,
                                                          outputStrLn)
import qualified System.Console.Haskeline.MonadException as Exc


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
