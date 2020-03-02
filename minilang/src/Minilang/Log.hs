{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

{-| A simple logger that reads `LogEntry` from a `TBChan` and dumps JSON-formatted
strings to `stdout`. -}
module Minilang.Log
  ( -- * Types
    LoggerEnv
    -- * Constructor & Destructor
  , newLog, stopLogger
    -- * Logging functions
  , logInfo, logError, withLog
  )where

import           Control.Concurrent.Async      (Async, async, cancel)
import           Control.Concurrent.Chan.Unagi (InChan, OutChan, newChan,
                                                readChan, writeChan)
import           Control.Monad                 (forever)
import           Control.Monad.Trans           (MonadIO (..))
import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                     (Text)
import           Data.Time.Clock               (UTCTime, diffUTCTime,
                                                getCurrentTime)
import           System.IO                     (Handle, stdout)

-- | Environment to control a logger thread.
data LoggerEnv m = LoggerEnv
    { logger     :: Maybe Logger
    , loggerId   :: Text
    , logInfo    :: forall a . (ToJSON a, MonadIO m) => a -> m ()
    , logError   :: forall a . (ToJSON a, MonadIO m) => a -> m ()
    , withLog    :: forall a b . (ToJSON a, MonadIO m) => a -> m b -> m b
    , stopLogger ::   (MonadIO m) => m ()
    }

type Logger = InChan BS.ByteString

-- |Starts an asynchronous log-processing thread and returns an initialised `LoggerEnv`.
newLog :: (MonadIO m) => Text -> m (LoggerEnv m)
newLog loggerId = liftIO $ do
  (inchan,outchan) <- newChan
  loggerThread <- async $ runLog outchan stdout
  let logger = Just inchan
      logInfo a = logEvent' inchan loggerId a
      logError a = logError' inchan loggerId a
      withLog a act = withLog' inchan loggerId a act
      stopLogger = stopLogger' loggerThread
  return $ LoggerEnv {..}

runLog :: OutChan BS.ByteString -> Handle -> IO a
runLog chan hdl = forever $ do
  toLog <- readChan chan
  BS.hPutStr hdl . (<> "\n") $ toLog

stopLogger' :: (MonadIO m) => Async () -> m ()
stopLogger' = liftIO . cancel

logEvent' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m ()
logEvent' chan logId message = do
  ts <- liftIO getCurrentTime
  liftIO $ writeChan chan $ LBS.toStrict $ encode $
    object [ "timestamp" .= ts
           , "loggerId" .= logId
           , "message" .= message
           ]

withLog' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m b -> m b
withLog' chan logId message  act = do
  startTime <- liftIO getCurrentTime
  logStart chan logId startTime message
  b <- act
  endTime <- liftIO getCurrentTime
  logEnd chan logId startTime endTime message
  pure b

logStart :: (ToJSON a, MonadIO m) => Logger -> Text -> UTCTime -> a -> m ()
logStart chan logId ts command = liftIO $ writeChan chan $ LBS.toStrict $ encode $
                                           object [ "timestamp" .= ts
                                                  , "servicer" .= logId
                                                  , "message" .= command
                                                  ]

logEnd :: (ToJSON a, MonadIO m) => Logger -> Text -> UTCTime -> UTCTime -> a -> m ()
logEnd chan logId ts en outcome = liftIO $ writeChan chan $ LBS.toStrict $ encode $
                                     object [ "timestamp" .= en
                                            , "durationMs" .= ((diffUTCTime en ts) * 1000)
                                            , "servicer" .= logId
                                            , "message" .= outcome
                                            ]

logError' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m ()
logError' chan logId err = liftIO $ do
  ts <- liftIO getCurrentTime
  liftIO $ writeChan chan $ LBS.toStrict $ encode $
    object [ "timestamp" .= ts
           , "servicer" .= logId
           , "error" .= err
           ]
