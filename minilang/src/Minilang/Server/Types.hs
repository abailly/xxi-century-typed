module Minilang.Server.Types where

import           Control.Concurrent.Async

data Server = Server
    { serverThread :: Maybe (Async ())
    -- ^If the server is running, this will contain the underlying
    , serverPort   :: Int
    -- ^The actual port this server is listening on. Useful when
    }
