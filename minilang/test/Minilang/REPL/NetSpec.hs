module Minilang.REPL.NetSpec where

import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Exception           (bracket)
import           Control.Monad               (forM)
import           Data.Aeson                  (eitherDecode, encode)
import           Minilang.Env
import           Minilang.Eval               (SumClos (..), Value (..),
                                              emptyContext)
import           Minilang.Parser
import           Minilang.REPL.Net
import           Minilang.REPL.Types
import           Minilang.Server.Types
import           Network.HTTP.Types          (status400)
import           Network.Wai                 (responseLBS)
import           Network.Wai.Handler.Warp    as Warp
import           Network.WebSockets          as WS
import           Prelude                     hiding (lines, readFile, writeFile)
import           Test.Hspec

startServer :: IO Server
startServer = do
  (port, socket) <- openFreePort
  envs <- newTVarIO mempty
  let app = runNetREPL envs (\ _ resp -> resp $ responseLBS status400 [] "Not a WebSocket request")
  thread <- async $ Warp.runSettingsSocket defaultSettings socket app
  threadDelay 300000
  pure $ Server (Just thread) port

stopServer :: Server -> IO ()
stopServer (Server (Just th) _) = cancel th
stopServer _                    = pure ()

withServer :: (Server -> IO c) -> IO c
withServer =
  bracket startServer stopServer

runTestClient :: Int -> String -> [In] -> IO [Either String Out]
runTestClient port envId inputs =
  runClient "127.0.0.1" port ("/repl/" <> envId) client
  where
    client cnx = do
      outs <- forM inputs $ \ inp -> do
        WS.sendBinaryData cnx (encode inp)
        eitherDecode <$> WS.receiveData cnx
      WS.sendClose cnx (encode EOF)
      pure outs

spec :: Spec
spec = around withServer $ describe "MiniLang Network REPL" $ do

    it "evaluates definition and returns defined symbol" $ \ Server{serverPort} -> do
      let inp = [ In "Unit : U = Sum(tt)" ]
          expectedOutput = [ Right $ Defined (B "Unit") U ]
      res <- runTestClient serverPort "newenv" inp

      res `shouldBe` expectedOutput

    it "allows retrieving empty initial env" $ \ Server{serverPort} -> do
      let inp = [ Com DumpEnv ]
          expectedOutput = [ Right $ CurrentEnv EmptyEnv emptyContext ]
      res <- runTestClient serverPort "newenv" inp

      res `shouldBe` expectedOutput

    it "can reconnect to existing environment" $ \ Server{serverPort} -> do
      let inp = [ In "Unit : U = Sum(tt)" ]
      _ <- runTestClient serverPort  "newenv" inp
      res <- runTestClient serverPort  "newenv" [ In "Unit" ]

      res `shouldBe` [ Right $ Evaluated (ESum (SumClos ([Choice "tt" Nothing], EmptyEnv))) EU ]
