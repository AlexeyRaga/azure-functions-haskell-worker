{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}
module Azure.Functions.Bindings.HTTP
( HttpRequest(..)
, HttpResponse(..)
, module Azure.Functions.Bindings.Class
)
where

import           Azure.Functions.Bindings.Class
import           Data.ByteString                       (ByteString)
import qualified Data.Map                              as Map
import           Data.Map.Strict                       (Map)
import           Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text
import           GHC.Generics                          (Generic)
import           Lens.Family                           ((&), (.~), (^.))
import           Lens.Family.Stock                     (at)
import           Network.URI                           (URI, parseURI)
import           Proto.FunctionRpc
import           Proto.FunctionRpc_Fields

data HttpRequest = HttpRequest
  { httpRequestMethod  :: Text
  , httpRequestUrl     :: URI
  , httpRequestQuery   :: Map Text Text
  , httpRequestHeaders :: Map Text Text
  , httpRequestBody    :: Maybe ByteString
  } deriving (Show, Generic)

data HttpResponse = HttpResponse
  { httpResponseStatus  :: Int
  , httpResponseBody    :: Maybe ByteString
  , httpResponseHeaders :: Map Text Text
  } deriving (Show, Generic)

instance FromInvocationRequest HttpRequest where
  fromInvocationRequest req = do
    td <- req ^. triggerMetadata . at "$request"
    td ^. maybe'http >>= fromRpcHttp

instance ToInvocationResponse HttpResponse where
  toInvocationResponse resp =
    let
      td = defMessage @TypedData
              & maybe'data' .~ fmap TypedData'Bytes (httpResponseBody resp)

      ht = defMessage @RpcHttp
              & headers .~ httpResponseHeaders resp
              & body .~ td

      stts = defMessage @StatusResult
              & status .~ StatusResult'Success

    in defMessage @InvocationResponse
        & returnValue .~ (defMessage @TypedData & http .~ ht)
        & result .~ stts

fromRpcHttp :: RpcHttp -> Maybe HttpRequest
fromRpcHttp req = do
  uri <- parseURI $ Text.unpack (req ^. url)

  pure HttpRequest
        { httpRequestMethod   = req ^. method
        , httpRequestUrl      = uri
        , httpRequestQuery    = req ^. query
        , httpRequestHeaders  = req ^. headers
        , httpRequestBody     = (req ^. maybe'rawBody >>= fromTypedData)
        }

fromTypedData :: TypedData -> Maybe ByteString
fromTypedData td =
  case td ^. maybe'data' of
    Just (TypedData'String v) -> Just (Text.encodeUtf8 v)
    Just (TypedData'Json v)   -> Just (Text.encodeUtf8 v)
    Just (TypedData'Bytes v)  -> Just v
    Just (TypedData'Stream v) -> Just v
    _                         -> Nothing

