{-| An HTTP server providing a basic REPL and text editor to work
collaboratively on Minilang code. -}
module Minilang.Server where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Data.Default
import           Minilang.REPL.Net
import           Minilang.Server.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON

-- |Starts HTTP server on given port.
-- This server serves static files from public/ directory and also
-- exposes a WebSocket-based REPL under `/repl` path.
startServer :: Int -> IO Server
startServer serverPort = do
  envs <- newTVarIO mempty
  logger <- doLog
  let app = logger $ runNetREPL envs staticResources
  (port, action) <- startWarp serverPort app
  thread <- async action
  pure $ Server (Just thread) port
  where
    startWarp 0 app = do
      (port, socket) <- openFreePort
      pure (port , Warp.runSettingsSocket defaultSettings socket app)
    startWarp port app = pure (port, Warp.run port app)

    doLog = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }


stopServer :: Server -> IO ()
stopServer (Server (Just th) _) = cancel th
stopServer _                    = pure ()

-- | Serve static resources under `public/` directory
staticResources :: Application
staticResources = staticApp (defaultFileServerSettings "public")
