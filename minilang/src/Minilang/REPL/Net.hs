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
import qualified HStore                         as H
import           HStore.FileOps
import           Minilang.Log
import           Minilang.REPL                  (runREPL, withInputs)
import           Minilang.REPL.Purer            (purerREPL)
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

instance Serialize In where
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

instance Versionable In

doStore :: In -> Net IO (Either Text ())
doStore out = do
  fs  <- gets storage
  res <- liftIO $ store fs (pure $ Right out) handleStorageResult
  case res of
    WriteSucceed _ -> pure $ Right ()
    err            -> pure $ Left (pack $ show err)
  where
    handleStorageResult (Right (WriteSucceed _)) = pure $ Right ()
    handleStorageResult (Right err)              = pure $ Left $ pack $ show err
    handleStorageResult (Left err)               = pure $ Left $ pack err

netLog :: (ToJSON a) => a -> Net IO ()
netLog logEntry = gets logger >>= liftIO . flip logInfo logEntry

instance MonadREPL (Net IO) where
  input     = do
    inp <- gets connection >>= lift . wsReceive
    res <- doStore inp
    case res of
      Right () -> netLog (object [ "action" .= ("input" :: Text), "content" .= inp ])
      Left txt -> netLog (object [ "action" .= ("storageError" :: Text), "content" .= txt ])
    pure inp

  output a  = do
    gets connection >>= lift . wsSend a
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
  conn        <- WS.acceptRequest cnx
  storageFile <- mkStorageFile path
  withStorage storageFile $ \ fileStorage -> do
    replEnv     <- findOrCreateREPL fileStorage path
    WS.withPingThread conn 30 (pure ()) $
      evalStateT (runNet runREPL) (NetEnv conn loggerEnv replEnv fileStorage)
  where
    findOrCreateREPL :: FileStorage -> ByteString -> IO (TVar REPLEnv)
    findOrCreateREPL storageFile path = do
      (env, shouldLoad) <- atomically $ do
        envMaps <- readTVar envs
        (newEnv, shouldLoad) <- case Map.lookup path envMaps of
          Nothing -> newTVar initialREPL >>= \ v -> pure (v, True)
          Just e  -> pure (e, False)
        writeTVar envs (Map.insert path newEnv envMaps)
        pure (newEnv, shouldLoad)
      -- FIXME there is a race condition here
      if shouldLoad
        then replay storageFile env
        else pure env

    mkStorageFile path = do
      let f = takeFileName (unpack (safeDecodeUtf8 path))
      dir <- getCurrentDirectory
      pure $ defaultOptions { storageFilePath = dir </> f }

    replay :: FileStorage -> TVar REPLEnv -> IO (TVar REPLEnv)
    replay storage envVar = do
      res <- H.load storage
      case res of
        LoadSucceed coms -> do
          atomically $ do
            env <- readTVar envVar
            let newEnv = withInputs env coms
            writeTVar envVar (purerREPL newEnv)
          logInfo loggerEnv $ object [ "action" .= ("Loaded" :: Text)
                                     , "numCommands" .= length coms
                                     ]
          pure envVar

        err -> do
          logInfo loggerEnv $ object [ "action" .= ("ErrorLoading" :: Text), "error" .= pack (show err) ]
          pure envVar


runNetREPL :: LoggerEnv IO -> TVar (Map ByteString (TVar REPLEnv)) -> Application -> Application
runNetREPL loggerEnv envs = websocketsOr WS.defaultConnectionOptions (clientHandler loggerEnv envs)
