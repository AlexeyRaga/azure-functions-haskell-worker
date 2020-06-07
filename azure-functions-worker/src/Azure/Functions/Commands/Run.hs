{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Azure.Functions.Commands.Run
where

import qualified Azure.Functions.Internal.Templates as Tpl
import qualified Data.Text                          as Text
import           GHC.Generics                       (Generic)
import           Options.Applicative
import           System.Directory                   as Dir
import           System.Environment                 (getExecutablePath)
import           System.FilePath                    ((</>))

data Options = Options
  { host      :: String
  , port      :: Int
  , workerId  :: String
  , requestId :: String
  } deriving (Show, Generic)

optionsParser :: Parser Options
optionsParser = Options
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

runCommand :: Parser (IO ())
runCommand = runRunCommand <$> optionsParser

runRunCommand :: Options -> IO ()
runRunCommand opts = do
  print opts
  undefined
