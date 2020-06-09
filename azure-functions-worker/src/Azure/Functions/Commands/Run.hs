{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Azure.Functions.Commands.Run
where

import qualified Azure.Functions.Internal.Templates as Tpl
import           Control.Monad                      (void)
import           Control.Monad.Except               (runExceptT)
import           Control.Monad.IO.Class             (liftIO)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           GHC.Generics                       (Generic)
import           Lens.Family                        ((&), (.~), (^.))
import           Network.GRPC.HTTP2.ProtoLens       (RPC (..))
import           Options.Applicative
import           System.Directory                   as Dir
import           System.Environment                 (getArgs, getExecutablePath)
import           System.FilePath                    ((</>))

import Network.GRPC.Client             as GRPC
import Network.GRPC.Client.Helpers     as GRPC
import Network.HTTP2.Client            (HostName, PortNumber, TooMuchConcurrency)
import Network.HTTP2.Client.Exceptions (ClientIO)

import Data.ProtoLens.Runtime.Data.ProtoLens as PL

import           Proto.FunctionRpc
import qualified Proto.FunctionRpc_Fields as Fields

import Paths_azure_functions_worker (version)

type RequestId = Text
type WorkerId  = Text

data Options = Options
  { host      :: HostName
  , port      :: PortNumber
  , workerId  :: WorkerId
  , requestId :: RequestId
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

data WorkerState = Unitialised | Started

runRunCommand :: Options -> IO ()
runRunCommand opts = do
  let cfg = GRPC.grpcClientConfigSimple (host opts) (port opts) False

  let startMsg  = PL.defMessage @StartStream & Fields.workerId .~ (workerId opts)
  let initMsg   = PL.defMessage @StreamingMessage
                    & Fields.requestId .~ (requestId opts)
                    & Fields.maybe'content .~ Just (StreamingMessage'StartStream startMsg)

  let rpc = RPC :: RPC FunctionRpc "eventStream"

  cres <- runExceptT $ do
    client <- GRPC.setupGrpcClient cfg
    -- void . notTooMuch $ GRPC.rawUnary rpc client initMsg

    notTooMuch $ rawSteppedBidirectional rpc client [initMsg] $ \case
      []      -> pure ([], WaitOutput (\_ _ -> liftIO . handleEnvelope) (\_ s _ -> pure s))
      (x:xs)  -> pure (xs, SendInput Uncompressed x)


  print "-----------------------------------------------------------------------------"
  print cres

handleEnvelope :: StreamingMessage -> IO [StreamingMessage]
handleEnvelope msg = do
  -- appendFile "/tmp/msg" (show msg)
  -- appendFile "/tmp/msg" "\n\n"

  let rid = msg ^. Fields.requestId
  case msg ^. Fields.maybe'content of
    Nothing -> pure []
    Just c  -> handleMessage rid c


handleMessage :: RequestId -> StreamingMessage'Content -> IO [StreamingMessage]

handleMessage rid (StreamingMessage'WorkerInitRequest msg) = handleInitRequest rid msg

handleMessage rid (StreamingMessage'FunctionLoadRequest msg)   = do
  let status = defMessage @StatusResult
                & Fields.status .~ StatusResult'Success
  let resp = defMessage @FunctionLoadResponse
                & Fields.functionId .~ (msg ^. Fields.functionId)
                & Fields.result .~ status
  pure
    [ defMessage @StreamingMessage
        & Fields.requestId .~ rid
        & Fields.maybe'content .~ Just (StreamingMessage'FunctionLoadResponse resp)
    ]

handleMessage rid msg =
  print msg >> pure []

handleInitRequest :: RequestId -> WorkerInitRequest -> IO [StreamingMessage]
handleInitRequest rid msg = do
  let status = defMessage @StatusResult
                & Fields.status .~ StatusResult'Success
  let resp = defMessage @WorkerInitResponse
                & Fields.workerVersion .~ Text.pack (show version)
                & Fields.maybe'result .~ Just status

  pure
    [ defMessage @StreamingMessage
        & Fields.requestId .~ rid
        & Fields.maybe'content .~ Just (StreamingMessage'WorkerInitResponse resp)
    ]

-------------------------------------------------------------------------------
notTooMuch :: ClientIO (Either TooMuchConcurrency a) -> ClientIO a
notTooMuch f = do
  ma <- f
  case ma of
    Left _  -> error "Too much concurrency. TODO: do something smarter"
    Right a -> pure a
