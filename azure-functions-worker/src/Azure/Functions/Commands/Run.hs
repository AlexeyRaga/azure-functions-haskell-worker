{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Azure.Functions.Commands.Run
where

import qualified Azure.Functions.Internal.Templates as Tpl
import           Control.Monad                      (void)
import           Control.Monad.Except               (runExceptT)
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Map.Strict                    as Map
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
import qualified Proto.FunctionRpc_Fields  as Fields
import           Proto.FunctionRpc_Helpers (failureStatus, rpcLogError, rpcLogInfo, toResponse, toResponseLogError')

import Data.Version                 (showVersion)
import Paths_azure_functions_worker (version)

import Azure.Functions.Bindings.Class (fromInvocationRequest, toInvocationResponse)
import Azure.Functions.Bindings.HTTP  (HttpRequest (..), HttpResponse (..))

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
handleEnvelope req = do
  let rid = req ^. Fields.requestId
  resp <- case req ^. Fields.maybe'content of
            Nothing -> pure []
            Just c -> case c of
              StreamingMessage'WorkerInitRequest msg   -> sequence [toResponse req <$> handleWorkerInit msg]
              StreamingMessage'FunctionLoadRequest msg -> sequence [toResponse req <$> handleFunctionLoad msg]
              StreamingMessage'InvocationRequest msg   -> sequence [toResponse req <$> handleInvocation msg]
              msg                                      -> pure []

  appendFile "/tmp/msg" ("Request:\n------------------------------------------------------------\n")
  appendFile "/tmp/msg" (show req <> "\n\n")
  appendFile "/tmp/msg" ("Response:\n------------------------------------------------------------\n")
  appendFile "/tmp/msg" (show resp <> "\n\n")
  pure resp


handleFunctionLoad :: FunctionLoadRequest -> IO FunctionLoadResponse
handleFunctionLoad req = do
  let status = defMessage @StatusResult
                & Fields.status .~ StatusResult'Success
  let resp = defMessage @FunctionLoadResponse
                & Fields.functionId .~ (req ^. Fields.functionId)
                & Fields.result .~ status
  pure resp

handleWorkerInit :: WorkerInitRequest -> IO WorkerInitResponse
handleWorkerInit msg = do
  let status = defMessage @StatusResult
                & Fields.status .~ StatusResult'Success
                & Fields.logs .~ [ rpcLogInfo ("Haskell worker loaded, version: " <>  Text.pack (showVersion version))]
  let resp = defMessage @WorkerInitResponse
                & Fields.workerVersion .~ Text.pack (showVersion version)
                & Fields.result .~ status

  pure resp


handleInvocation :: InvocationRequest -> IO InvocationResponse
handleInvocation req = do

  let httpReq = fromInvocationRequest @HttpRequest req
  case httpReq of
    Nothing -> pure $
      defMessage @InvocationResponse
        & Fields.invocationId .~ (req ^. Fields.invocationId)
        & Fields.result .~ failureStatus "Unable to parse HTTP request"

    Just req' -> do
      httpResp <- toInvocationResponse <$> fakeHttpFunction req'
      pure $ httpResp & Fields.invocationId .~ (req ^. Fields.invocationId)

-------------------------------------------------------------------------------

fakeHttpFunction :: HttpRequest -> IO HttpResponse
fakeHttpFunction req = pure HttpResponse
  { httpResponseStatus = 200
  , httpResponseBody = httpRequestBody req
  , httpResponseHeaders = Map.fromList [("X-Powered-By", "Azure Function Haskell Worker")]
  }

notTooMuch :: ClientIO (Either TooMuchConcurrency a) -> ClientIO a
notTooMuch f = do
  ma <- f
  case ma of
    Left _  -> error "Too much concurrency. TODO: do something smarter"
    Right a -> pure a
