{-# LANGUAGE DeriveGeneric #-}
module Azure.Functions.Internal.Options
where

-- import           Data.Text
import GHC.Generics        (Generic)
import Options.Applicative

data WorkerOptions = WorkerOptions
  { host      :: String
  , port      :: Int
  , workerId  :: String
  , requestId :: String
  } deriving (Generic)

workerOptionsParser :: Parser WorkerOptions
workerOptionsParser = WorkerOptions
  <$> strOption
    (  long "host"
    <> metavar "HOST"
    <> help "Run on the specified host"
    )
  <*> option auto
    (  long "port"
    <> metavar "PORT"
    <> help "Run gRPC channel on the specified port"
    )
  <*> strOption
    (  long "workerId"
    <> help "Worker ID"
    )
  <*> strOption
    (  long "requestId"
    <> help "Request ID"
    )

workerOptParserInfo :: ParserInfo WorkerOptions
workerOptParserInfo = info (helper <*> workerOptionsParser)
  (  fullDesc
  <> progDesc "Starts Azure Functions Worker"
  <> header "Azure Functions Worker"
  )

parseWorkerOptions :: IO WorkerOptions
parseWorkerOptions = execParser workerOptParserInfo
