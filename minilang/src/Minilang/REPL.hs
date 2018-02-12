
module Minilang.REPL where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Text
import           Minilang.Parser

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
