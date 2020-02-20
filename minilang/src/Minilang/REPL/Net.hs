{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A version of REPL that interacts through a websocket.
module Minilang.REPL.Net where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch
import           Control.Monad.State
import           Control.Monad.Trans            (lift)
import           Data.Aeson                     (eitherDecode, encode)
import           Data.ByteString                (ByteString)
import           Data.Map                       as Map
import           Minilang.REPL                  (runREPL)
import           Minilang.REPL.Types
import           Minilang.Type
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets
import qualified Network.WebSockets             as WS

-- | Configuration of the WS server
data ServerConfig = ServerConfig
    { configHost :: String
    -- ^The host/IP to listen on. If 0.0.0.0, then the server listens
    , configPort :: Int
    -- ^The port to listen on. If 0, a random port will be assigned.
    }
    deriving (Eq, Show)

data NetEnv = NetEnv
    { connection :: WS.Connection
    , repl       :: TVar REPLEnv
    }

newtype Net m a = Net { runNet :: StateT NetEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState NetEnv)

instance MonadREPL (Net IO) where
  input     = gets connection >>= lift . wsReceive
  output a  = gets connection >>= lift . wsSend a
  prompt    = pure ()
  getEnv    = gets repl >>= lift . atomically . readTVar
  setEnv e' = gets repl >>= lift . atomically . flip writeTVar e'
  load      = const (pure (Right "") )

instance (MonadThrow m) => MonadThrow (Net m)  where
  throwM = lift . throwM

instance (MonadCatch m) => MonadCatch (Net m)  where
  Net m `catch` f = Net $ m `catch` \ e -> runNet (f e)

instance TypeChecker (Net IO) where
  emit = const $ pure()

wsReceive :: WS.Connection -> IO In
wsReceive cnx =
  either (const $ In "") id . eitherDecode <$> WS.receiveData cnx

wsSend :: Out -> WS.Connection -> IO ()
wsSend out cnx =
  WS.sendBinaryData cnx (encode out)

clientHandler :: TVar (Map ByteString (TVar REPLEnv)) -> WS.PendingConnection -> IO ()
clientHandler envs cnx = do
  let path = WS.requestPath $ WS.pendingRequest cnx
  replEnv <- findOrCreateREPL path
  conn <- WS.acceptRequest cnx
  evalStateT (runNet runREPL) (NetEnv conn replEnv)
  where
    findOrCreateREPL path = atomically $ do
      envMaps <- readTVar envs
      newEnv <- case Map.lookup path envMaps of
        Nothing -> newTVar initialREPL
        Just e  -> pure e
      writeTVar envs (Map.insert path newEnv envMaps)
      pure newEnv

runNetREPL :: TVar (Map ByteString (TVar REPLEnv)) -> Application -> Application
runNetREPL envs = websocketsOr WS.defaultConnectionOptions (clientHandler envs)
