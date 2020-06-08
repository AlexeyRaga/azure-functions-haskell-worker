{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Azure.Functions.Commands.Run
where

import qualified Azure.Functions.Internal.Templates as Tpl
import qualified Data.Text                          as Text
import           GHC.Generics                       (Generic)
import           Network.GRPC.HTTP2.ProtoLens       (RPC (..))
import           Options.Applicative
import           System.Directory                   as Dir
import           System.Environment                 (getArgs, getExecutablePath)
import           System.FilePath                    ((</>))

import Network.GRPC.Server
import Network.Wai.Handler.Warp    (HostPreference, defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Handler.WarpTLS (defaultTlsSettings, tlsSettingsMemory)
import Proto.FunctionRpc

data Options = Options
  { host      :: HostPreference
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
  <* option (auto @Int)
    (  long "grpcMaxMessageLength"
    <> help "The maximum message length to use for gRPC messages"
    )

runCommand :: Parser (IO ())
runCommand = runRunCommand <$> optionsParser

runRunCommand :: Options -> IO ()
runRunCommand opts = do
  let settings = setPort (port opts) . setHost (host opts) $ defaultSettings
  runSettings settings (grpcApp [] handlers)

handlers :: [ServiceHandler]
handlers =
  [ generalStream (RPC :: RPC FunctionRpc "eventStream") eventStreamHandler
  ]

eventStreamHandler :: GeneralStreamHandler IO StreamingMessage StreamingMessage Int ()
eventStreamHandler req = do
  print req
  pure (0, incoming, (), outgoing)
  where
    incoming = IncomingStream handleMessage handleEof
    outgoing = OutgoingStream (\_ -> pure Nothing)

    handleEof _ = pure ()

    handleMessage n msg = do
      let fileName = "/tmp/msg-" <> show n
      writeFile fileName (show msg)
      pure (n+1)

