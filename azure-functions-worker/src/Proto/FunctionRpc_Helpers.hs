{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Proto.FunctionRpc_Helpers
where

import Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import Data.Text                             (Text)
import Lens.Family                           ((&), (.~), (^.))
import Proto.FunctionRpc
import Proto.FunctionRpc_Fields

class ToStreamingMessageContent a where
  toStreamingMessageContent :: a  -> StreamingMessage'Content

instance ToStreamingMessageContent WorkerInitResponse where
  toStreamingMessageContent = StreamingMessage'WorkerInitResponse

instance ToStreamingMessageContent FunctionLoadResponse where
  toStreamingMessageContent = StreamingMessage'FunctionLoadResponse

instance ToStreamingMessageContent InvocationResponse where
  toStreamingMessageContent = StreamingMessage'InvocationResponse

instance ToStreamingMessageContent FunctionEnvironmentReloadResponse where
  toStreamingMessageContent = StreamingMessage'FunctionEnvironmentReloadResponse

instance ToStreamingMessageContent WorkerStatusResponse where
  toStreamingMessageContent = StreamingMessage'WorkerStatusResponse

instance ToStreamingMessageContent WorkerActionResponse where
  toStreamingMessageContent = StreamingMessage'WorkerActionResponse

instance ToStreamingMessageContent WorkerHeartbeat where
  toStreamingMessageContent = StreamingMessage'WorkerHeartbeat

instance ToStreamingMessageContent RpcLog where
  toStreamingMessageContent = StreamingMessage'RpcLog

toResponse :: ToStreamingMessageContent a
  => StreamingMessage
  -> a
  -> StreamingMessage
toResponse req resp =
  defMessage
    & requestId .~ (req ^. requestId)
    & maybe'content .~ Just (toStreamingMessageContent resp)

toResponseLogError :: ToStreamingMessageContent a
  => StreamingMessage
  -> Either Text a
  -> StreamingMessage
toResponseLogError req resp = toResponseLogError' req id resp

toResponseLogError' :: ToStreamingMessageContent a
  => StreamingMessage
  -> (RpcLog -> RpcLog)
  -> Either Text a
  -> StreamingMessage
toResponseLogError' req updateLog resp =
  let content = either (toStreamingMessageContent . updateLog . rpcLogError) toStreamingMessageContent resp
  in defMessage
      & requestId .~ (req ^. requestId)
      & maybe'content .~ Just content

rpcLogMessage :: RpcLog'Level ->  Text -> RpcLog
rpcLogMessage lvl msg =
  defMessage
    & level   .~ lvl
    & message .~ msg

rpcLogInfo :: Text -> RpcLog
rpcLogInfo = rpcLogMessage RpcLog'Information

rpcLogError :: Text -> RpcLog
rpcLogError = rpcLogMessage RpcLog'Error

failureStatus :: Text -> StatusResult
failureStatus msg =
  defMessage @StatusResult
    & status .~ StatusResult'Failure
    & result .~ msg
    & logs .~ [
      rpcLogError msg
    ]
    & exception .~ (
      defMessage @RpcException
        & message .~ msg
    )
