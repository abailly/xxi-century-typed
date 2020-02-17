-- | A version of REPL that interacts through a websocket.
module Minilang.REPL.Net where

import           Control.Concurrent.Async
import           Data.Aeson                     (encode)
import           Data.Text                      (Text)
import           Minilang.Parser
import           Minilang.REPL.Types
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

data Server = Server
    { serverThread :: Maybe (Async ())
    -- ^If the server is running, this will contain the underlying
    , serverPort   :: Int
    -- ^The actual port this server is listening on. Useful when
    }

clientHandler :: WS.PendingConnection -> IO ()
clientHandler cnx = do
  conn <- WS.acceptRequest cnx
  WS.sendBinaryData conn (encode $ Defined (B "Unit") U)
  WS.sendClose conn ("Bye!" :: Text)

runNetREPL :: Application -> Application
runNetREPL = websocketsOr WS.defaultConnectionOptions clientHandler
