module Minilang.REPL.NetSpec where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception        (bracket)
import           Control.Monad            (forM)
import           Data.Aeson               (decode, encode)
import           Minilang.Parser
import           Minilang.REPL.Net
import           Minilang.REPL.Types
import           Network.HTTP.Types       (status400)
import           Network.Wai              (responseLBS)
import           Network.Wai.Handler.Warp as Warp
import           Network.WebSockets       as WS
import           Prelude                  hiding (lines, readFile, writeFile)
import           Test.Hspec

startServer :: IO Server
startServer = do
  (port, socket) <- openFreePort
  let app = runNetREPL (\ _ resp -> resp $ responseLBS status400 [] "Not a WebSocket request")
  thread <- async $ Warp.runSettingsSocket defaultSettings socket app
  threadDelay 1000000
  pure $ Server (Just thread) port

stopServer :: Server -> IO ()
stopServer (Server (Just th) _) = cancel th
stopServer _                    = pure ()

withServer :: (Server -> IO c) -> IO c
withServer =
  bracket startServer stopServer

runTestClient :: Int -> [In] -> IO [Maybe Out]
runTestClient port inputs =
  runClient "127.0.0.1" port "/repl" client
  where
    client cnx =
      forM inputs $
      \ inp -> do
        WS.sendBinaryData cnx (encode inp)
        decode <$> WS.receiveData cnx


spec :: Spec
spec = around withServer $ describe "MiniLang Network REPL" $ do

    it "can retrieve empty environment when user connects" $ \ Server{serverPort} -> do
      let inp = [ In "Unit : U = Sum(tt)" ]
          expectedOutput = [ Just $ Defined (B "Unit") U ]
      res <- runTestClient serverPort inp

      res `shouldBe` expectedOutput
