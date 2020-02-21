module Minilang.ServerSpec where

import           Control.Exception     (bracket)
import           Data.String           (fromString)
import           Minilang.Server
import           Minilang.Server.Types
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Test.Hspec

withServer :: (Server -> IO c) -> IO c
withServer =
  bracket (startServer 0) stopServer

spec :: Spec
spec = around withServer $ describe "MiniLang Server" $ do

    it "serves index.html page" $ \ Server{serverPort} -> do
      response <- httpBS (fromString $ "http://localhost:" <> show serverPort <> "/index.html")

      getResponseStatus response `shouldBe` ok200
