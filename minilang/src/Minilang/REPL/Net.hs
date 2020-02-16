-- | A version of REPL that interacts through a websocket.
module Minilang.REPL.Net where

import qualified Network.WebSockets as WS

application :: WS.PendingConnection -> IO ()
application = undefined

runNetwork :: IO ()
runNetwork =
  WS.runServer "127.0.0.1" 9160 application
