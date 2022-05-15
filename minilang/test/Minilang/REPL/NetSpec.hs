module Minilang.REPL.NetSpec where

--import           Control.Concurrent          (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Aeson (eitherDecode, encode)
import Minilang.Env
import Minilang.Eval (Value (EU), emptyContext)
import Minilang.Log
import Minilang.Parser
import Minilang.REPL.Net
import Minilang.REPL.Types
import Minilang.Server.Types
import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp as Warp
import Network.WebSockets as WS
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose)
import System.Posix.Temp (mkstemp)
import Test.Hspec
import Prelude hiding (lines, readFile, writeFile)

startServer :: IO Server
startServer = do
    (port, socket) <- openFreePort
    envs <- newTVarIO mempty
    logger <- newLog "test"
    let app = runNetREPL logger envs (\_ resp -> resp $ responseLBS status400 [] "Not a WebSocket request")
        settings = setGracefulShutdownTimeout (Just 0) defaultSettings
    thread <- async $ Warp.runSettingsSocket settings socket app
    pure $ Server (Just thread) port

stopServer :: Server -> IO ()
stopServer (Server (Just th) _) = cancel th
stopServer _ = pure ()

withServer :: (Server -> IO c) -> IO c
withServer =
    bracket startServer stopServer

runTestClient :: Int -> String -> [In] -> IO [Either String Out]
runTestClient port envId inputs =
    runClient "127.0.0.1" port ("/repl/" <> envId) client
  where
    client cnx = do
        outs <- forM inputs $ \inp -> do
            WS.sendBinaryData cnx (encode inp)
            eitherDecode <$> WS.receiveData cnx
        WS.sendBinaryData cnx (encode EOF)
        WS.sendClose cnx (encode ("" :: String))
        pure outs

temporaryFile :: (FilePath -> IO c) -> IO c
temporaryFile = bracket newTempFile removeFile
  where
    newTempFile = do
        dir <- getTemporaryDirectory
        (f, h) <- mkstemp (dir </> "repl.test")
        hClose h
        pure f

spec :: Spec
spec = around withServer $
    describe "MiniLang Network REPL" $ do
        it "evaluates definition and returns defined symbol" $ \Server{serverPort} -> do
            let inp = [In "Unit : U = Sum(tt)"]
                expectedOutput = [Right $ Defined (B "Unit") (U 0)]
            res <- runTestClient serverPort ".newenv1" inp

            res `shouldBe` expectedOutput

        it "allows retrieving empty initial env" $ \Server{serverPort} -> do
            let inp = [Com DumpEnv]
                expectedOutput = [Right $ CurrentEnv EmptyEnv emptyContext]
            res <- runTestClient serverPort ".newenv2" inp

            res `shouldBe` expectedOutput

        it "can reconnect to existing environment" $ \Server{serverPort} -> do
            let inp = [In "Unit : U = Sum(tt)"]
            _ <- runTestClient serverPort ".newenv3" inp
            res <- runTestClient serverPort ".newenv3" [In "Unit"]

            res `shouldSatisfy` isEvaluated

isEvaluated :: [Either a Out] -> Bool
isEvaluated [Right (Evaluated _ EU{})] = True
isEvaluated _ = False
