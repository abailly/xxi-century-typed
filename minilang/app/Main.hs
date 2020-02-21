module Main where

import           Minilang.IO
import           Minilang.REPL
import           Minilang.Server
import           Options.Applicative
import           System.IO             (stdin, stdout)
import           System.Posix.IO       (stdInput)
import           System.Posix.Terminal (queryTerminal)

data RunMode = Local
    | Server
    deriving (Eq, Show)

data Options = Options
    { serverPort :: Int
    , runMode    :: RunMode
    }
    deriving (Eq, Show)

optionsParser :: ParserInfo Options
optionsParser =
  info (helper <*> minilangOptions)
  ( header "Minilang"
    <> fullDesc
  )

minilangOptions :: Parser Options
minilangOptions =
  Options <$> portOption <*> runModeOption

portOption :: Parser Int
portOption =
  option auto (long "port"
               <> short 'p'
               <> value 7890
               <> metavar "INT"
               <> help "port to listent on (default: 7890)"
              )

runModeOption :: Parser RunMode
runModeOption =
  flag Local Server (long "server"
                     <> short 'S'
                     <> help "run in server mode, listening for client REPLs over HTTP")

main :: IO ()
main = do
  Options{..} <- execParser optionsParser
  case runMode of
    Local -> do
      hasTty <- queryTerminal stdInput
      if hasTty
        then withTerminal
        else runEval stdin stdout
    Server -> startServer serverPort >>= waitServer
