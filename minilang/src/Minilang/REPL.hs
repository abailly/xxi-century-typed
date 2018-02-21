{-# LANGUAGE TypeSynonymInstances #-}
module Minilang.REPL where

import           Control.Exception    (catch, throwIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans  (lift)
import           Control.Monad.Writer
import           Data.Text
import           Data.Text.IO
import           Minilang.Eval
import           Minilang.Normalize
import           Minilang.Parser
import           System.IO            (Handle)
import           System.IO.Error      (isEOFError)

class (Monad m) => MonadREPL m where
  input  :: m (Maybe Text)
  output :: (Show a) => a -> m ()

data REPLResult a = Exiting
                  | Parsed a
  deriving (Eq, Show)

runREPL
  :: (MonadREPL m) => m (REPLResult Normal)
runREPL = do
  e <- input
  case e of
    Nothing -> output (Exiting :: REPLResult Normal) >> pure Exiting
    Just t  -> let normal = normalize 0 $ flip eval emptyEnv $  parseProgram t
               in  output normal >> pure (Parsed normal)

-- * IO REPL

type IOREPL = ReaderT (Handle,Handle) IO

instance MonadREPL IOREPL where
  input   = ask >>= lift . readLine . fst
    where
      readLine hdl = (Just <$> hGetLine hdl)
                     `catch` ( \ e -> if isEOFError e
                                      then pure Nothing
                                      else throwIO e)

  output a = ask >>= lift . flip hPutStrLn (pack $ show a) . snd

withHandles
  :: Handle -> Handle -> IOREPL a -> IO a
withHandles hin hout act =
  runReaderT act (hin, hout)

-- * Pure REPL

instance MonadREPL (WriterT [Text] (State [Text])) where
  input = do
    ins <- get
    case ins of
      []     -> pure Nothing
      (t:ts) -> put ts >> pure (Just t)

  output t = tell [pack $ show t]

type InMemREPL a = (WriterT [Text] (State [Text])) a

withInput
  :: [Text] -> InMemREPL a -> [Text]
withInput stream act =
  case runState (runWriterT act) stream of
    ((_, o), []) -> o
    ((_, o), s)  -> o <> withInput s act
