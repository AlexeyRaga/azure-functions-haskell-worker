{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Azure.Functions.Commands.Run
where

import           Azure.Functions.Internal.Runtime   (Runtime, createRuntime, getRuntimeFunction, loadRuntimeFunction, runningFunction)
import qualified Azure.Functions.Internal.Templates as Tpl
import           Azure.Functions.Registry           (RegisteredFunction (..), Registry (..))
import qualified Azure.Functions.Registry           as Registry
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

import Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)

import           Proto.FunctionRpc
import qualified Proto.FunctionRpc_Fields  as Fields
import           Proto.FunctionRpc_Helpers (failureStatus, successStatus, toResponse)

import Data.Version                 (showVersion)
import Paths_azure_functions_worker (version)

import Azure.Functions.Bindings.Class      (fromInvocationRequest, toInvocationResponse)
import Azure.Functions.Bindings.HTTP       (HttpRequest (..), HttpResponse (..))
import Azure.Functions.Bindings.ServiceBus (ReceivedMessage (..))

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

runCommand :: Registry -> Parser (IO ())
runCommand registry = runRunCommand registry <$> optionsParser

data WorkerState = Unitialised | Started

runRunCommand :: Registry -> Options -> IO ()
runRunCommand registry opts = do
  runtime <- createRuntime
  let cfg = GRPC.grpcClientConfigSimple (host opts) (port opts) False

  let startMsg  = defMessage @StartStream & Fields.workerId .~ (workerId opts)
  let initMsg   = defMessage @StreamingMessage
                    & Fields.requestId .~ (requestId opts)
                    & Fields.maybe'content .~ Just (StreamingMessage'StartStream startMsg)

  let rpc = RPC :: RPC FunctionRpc "eventStream"

  cres <- runExceptT $ do
    client <- GRPC.setupGrpcClient cfg
    notTooMuch $ rawSteppedBidirectional rpc client [initMsg] $ \case
      []      -> pure ([], WaitOutput (\_ _ -> liftIO . handleEnvelope registry runtime) (\_ s _ -> pure s))
      (x:xs)  -> pure (xs, SendInput Uncompressed x)


  print "-----------------------------------------------------------------------------"
  print cres

handleEnvelope :: Registry -> Runtime -> StreamingMessage -> IO [StreamingMessage]
handleEnvelope registry runtime req = do
  appendFile "/tmp/msg" ("Request:\n============================================================\n")
  appendFile "/tmp/msg" (show req <> "\n\n")

  let rid = req ^. Fields.requestId
  resp <- case req ^. Fields.maybe'content of
            Nothing -> pure []
            Just c -> case c of
              StreamingMessage'WorkerInitRequest msg   -> sequence [toResponse req <$> handleWorkerInit msg]
              StreamingMessage'FunctionLoadRequest msg -> sequence [toResponse req <$> handleFunctionLoad registry runtime msg]
              StreamingMessage'InvocationRequest msg   -> sequence [toResponse req <$> handleInvocation runtime msg]
              msg                                      -> pure []

  appendFile "/tmp/msg" ("Response:\n------------------------------------------------------------\n")
  appendFile "/tmp/msg" (show resp <> "\n\n")
  pure resp

handleWorkerInit :: WorkerInitRequest -> IO WorkerInitResponse
handleWorkerInit msg = do
  let resp = defMessage @WorkerInitResponse
                & Fields.workerVersion .~ Text.pack (showVersion version)
                & Fields.result .~ successStatus ("Haskell worker loaded, version: " <>  Text.pack (showVersion version))

  pure resp

handleFunctionLoad :: Registry -> Runtime -> FunctionLoadRequest -> IO FunctionLoadResponse
handleFunctionLoad registry runtime req = do
  let funcName = req ^. Fields.metadata . Fields.name
  let funcId   = req ^. Fields.functionId

  let resp = defMessage @FunctionLoadResponse
                & Fields.functionId .~ (req ^. Fields.functionId)

  case Registry.getFunction registry funcName of
    Nothing ->
      pure $ resp & Fields.result .~ failureStatus ("Unable to find function: " <> funcName)
    Just func -> do
      loadRuntimeFunction runtime funcId func
      pure $ resp & Fields.result .~ successStatus ("Loaded function: " <> funcName)

handleInvocation :: Runtime -> InvocationRequest -> IO InvocationResponse
handleInvocation runtime req = do
  let funcId   = req ^. Fields.functionId
  mbFunction <- getRuntimeFunction runtime funcId
  case mbFunction of
    Nothing -> pure $
      defMessage @InvocationResponse
        & Fields.invocationId .~ (req ^. Fields.invocationId)
        & Fields.result .~ failureStatus ("Unable to find running function with ID: " <> funcId)
    Just rfunc -> do
      resp <- runningFunction rfunc req
      pure $ resp & Fields.invocationId .~ (req ^. Fields.invocationId)

-------------------------------------------------------------------------------

notTooMuch :: ClientIO (Either TooMuchConcurrency a) -> ClientIO a
notTooMuch f = do
  ma <- f
  case ma of
    Left _  -> error "Too much concurrency. TODO: do something smarter"
    Right a -> pure a
