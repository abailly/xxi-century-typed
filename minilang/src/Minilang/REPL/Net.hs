{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A version of REPL that interacts through a websocket.
module Minilang.REPL.Net where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch
import           Control.Monad.State
import           Control.Monad.Trans            (lift)
import           Data.Aeson                     (ToJSON, eitherDecode, encode,
                                                 object, (.=))
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as LBS
import           Data.Map                       as Map
import           Data.Serialize                 hiding (encode)
import           Data.Text                      (Text, pack, unpack)
import           Data.Text.Encoding             (decodeUtf8With)
import           Data.Text.Encoding.Error       (lenientDecode)
import           HStore                         (StorageResult (..),
                                                 Versionable (..), store)
import           HStore.FileOps
import           Minilang.Log
import           Minilang.REPL                  (runREPL)
import           Minilang.REPL.Types
import           Minilang.Type
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets
import qualified Network.WebSockets             as WS
import           System.Directory               (getCurrentDirectory)
import           System.FilePath                (takeFileName, (</>))

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
    , logger     :: LoggerEnv IO
    , repl       :: TVar REPLEnv
    , storage    :: FileStorage
    }

newtype Net m a = Net { runNet :: StateT NetEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadState NetEnv)

instance Serialize Out where
  put out = do
    let bs = encode out
    putWord64be (fromIntegral $ LBS.length bs)
    putLazyByteString bs

  get = do
    len <- getWord64be
    bs <- getLazyByteString (fromIntegral len)
    case eitherDecode bs of
      Right out -> pure out
      Left err  -> fail err

instance Versionable Out

doStore :: Out -> Net IO Out
doStore out = do
  fs <- gets storage
  res <- liftIO $ store fs (pure $ Right out) handleStorageResult
  case res of
    WriteSucceed a -> pure a
    err            -> pure $ Msg (pack $ show err)
  where
    handleStorageResult (Right (WriteSucceed a)) = pure a
    handleStorageResult (Right err)              = pure $ Msg $ pack $ show err
    handleStorageResult (Left err)               = pure $ Msg (pack err)

netLog :: (ToJSON a) => a -> Net IO ()
netLog logEntry = gets logger >>= liftIO . flip logInfo logEntry

instance MonadREPL (Net IO) where
  input     = do
    inp <- gets connection >>= lift . wsReceive
    netLog (object [ "action" .= ("input" :: Text), "content" .= inp ])
    pure inp

  output a  = do
    doStore a >> gets connection >>= lift . wsSend a
    netLog (object [ "action" .= ("output" :: Text), "content" .= a ])

  prompt    = pure ()
  getEnv    = gets repl >>= lift . atomically . readTVar
  setEnv e' = gets repl >>= lift . atomically . flip writeTVar e'
  -- TODO what does this mean? perhaps it coiuld be used to load some
  -- virtual files edited by user in their env?
  load      = const (pure (Right "") )

instance (MonadThrow m) => MonadThrow (Net m)  where
  throwM = lift . throwM

instance (MonadCatch m) => MonadCatch (Net m)  where
  Net m `catch` f = Net $ m `catch` \ e -> runNet (f e)

instance TypeChecker (Net IO) where
  emit (CheckD ev) = gets logger >>= \ env -> liftIO $ logInfo env ev
  emit _           = pure ()

wsReceive :: WS.Connection -> IO In
wsReceive cnx =
  either (const $ In "") id . eitherDecode <$> WS.receiveData cnx

wsSend :: Out -> WS.Connection -> IO ()
wsSend out cnx =
  WS.sendBinaryData cnx (encode out)

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = decodeUtf8With lenientDecode

clientHandler :: LoggerEnv IO -> TVar (Map ByteString (TVar REPLEnv)) -> WS.PendingConnection -> IO ()
clientHandler loggerEnv envs cnx = do
  let path = WS.requestPath $ WS.pendingRequest cnx
  logInfo loggerEnv $ object [ "action" .= ("StartedREPL" :: Text), "path" .= safeDecodeUtf8 path ]
  replEnv <- findOrCreateREPL path
  conn <- WS.acceptRequest cnx
  storageFile <- mkStorageFile path
  withStorage storageFile $ \ fileStorage ->
    WS.withPingThread conn 30 (pure ()) $
    evalStateT (runNet runREPL) (NetEnv conn loggerEnv replEnv fileStorage)
  where
    findOrCreateREPL path = atomically $ do
      envMaps <- readTVar envs
      newEnv <- case Map.lookup path envMaps of
        Nothing -> newTVar initialREPL
        Just e  -> pure e
      writeTVar envs (Map.insert path newEnv envMaps)
      pure newEnv

    mkStorageFile path = do
      let f = takeFileName (unpack (safeDecodeUtf8 path))
      dir <- getCurrentDirectory
      pure $ defaultOptions { storageFilePath = dir </> f }

runNetREPL :: LoggerEnv IO -> TVar (Map ByteString (TVar REPLEnv)) -> Application -> Application
runNetREPL loggerEnv envs = websocketsOr WS.defaultConnectionOptions (clientHandler loggerEnv envs)
