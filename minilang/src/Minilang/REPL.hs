{-# LANGUAGE TypeSynonymInstances #-}
module Minilang.REPL where

import           Control.Exception    (catch, throwIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans  (lift)
import           Control.Monad.Writer
import           Data.Text
import           Data.Text.IO
import           Minilang.Parser
import           System.IO            (Handle)
import           System.IO.Error      (isEOFError)

class (Monad m) => MonadREPL m where
  input  :: m (Maybe Text)
  output :: (Show a) => a -> m ()

data REPLInfo = Exiting
  deriving (Eq, Show, Read)

runREPL
  :: (MonadREPL m) => m ()
runREPL = do
  e <- input
  case e of
    Nothing -> output Exiting
    Just t  -> output (parseML t)

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
  :: Handle -> Handle -> IOREPL () -> IO ()
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
  :: [Text] -> InMemREPL () -> [Text]
withInput stream act =
  case runState (runWriterT act) stream of
    ((_, o), []) -> o
    ((_, o), s)  -> o <> withInput s act
