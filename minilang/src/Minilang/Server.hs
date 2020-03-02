{-# LANGUAGE TypeApplications #-}
{-| An HTTP server providing a basic REPL and text editor to work
collaboratively on Minilang code. -}
module Minilang.Server where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Data.Aeson                                (Value, decode,
                                                            object, (.=))
import           Data.ByteString.Lazy                      (fromStrict)
import           Data.Default
import           Data.Text                                 (Text)
import           Minilang.Log
import           Minilang.REPL.Net
import           Minilang.Server.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           System.Log.FastLogger                     (fromLogStr)

-- |Starts HTTP server on given port.
-- This server serves static files from public/ directory and also
-- exposes a WebSocket-based REPL under `/repl` path.
startServer :: Int -> IO Server
startServer serverPort = do
  envs <- newTVarIO mempty
  logger <- newLog "minilang"
  loggerMiddleware <- runHTTPLog logger
  let app = loggerMiddleware $ runNetREPL envs staticResources
  (port, action) <- startWarp serverPort app
  thread <- async action
  logInfo logger $ object [ "action" .= ("Started" :: Text), "port" .= port ]
  pure $ Server (Just thread) port
  where
    startWarp 0 app = do
      (port, socket) <- openFreePort
      pure (port , Warp.runSettingsSocket defaultSettings socket app)
    startWarp port app = pure (port, Warp.run port app)

    runHTTPLog logger = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON
                                              , destination = Callback (\ str -> logInfo logger (decode @Value $ fromStrict $ fromLogStr str))
                                              }


stopServer :: Server -> IO ()
stopServer (Server (Just th) _) = cancel th
stopServer _                    = pure ()

waitServer :: Server -> IO ()
waitServer (Server (Just th) _) = wait th
waitServer _                    = pure ()

-- | Serve static resources under `public/` directory
staticResources :: Application
staticResources = staticApp (defaultFileServerSettings "public")
