{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Azure.Functions.Bindings.HTTP
( HttpRequest(..)
, HttpResponse(..)
, HttpBinding(..)
, module Azure.Functions.Bindings.Class
)
where

import           Azure.Functions.Bindings.Class
import           Azure.Functions.Internal.Lens         (toEither)
import           Data.Aeson                            ((.=))
import qualified Data.Aeson                            as Aeson
import           Data.ByteString                       (ByteString)
import qualified Data.Map                              as Map
import           Data.Map.Strict                       (Map)
import           Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text
import           GHC.Generics                          (Generic)
import           Lens.Family                           (LensLike, Phantom, to, view, (&), (.~), (^.))
import           Lens.Family.Stock                     (at)
import           Network.URI                           (URI, parseURI)
import           Proto.FunctionRpc
import           Proto.FunctionRpc_Fields

data HttpBinding  = HttpBinding

data HttpRequest = HttpRequest
  { httpRequestMethod  :: Text
  , httpRequestUrl     :: URI
  , httpRequestQuery   :: Map Text Text
  , httpRequestHeaders :: Map Text Text
  , httpRequestBody    :: Maybe ByteString
  } deriving (Show, Eq, Generic)

data HttpResponse = HttpResponse
  { httpResponseStatus  :: Int
  , httpResponseBody    :: Maybe ByteString
  , httpResponseHeaders :: Map Text Text
  } deriving (Show, Eq, Generic)

instance TriggerMessage HttpRequest where
  type Trigger HttpRequest = HttpBinding
  fromTriggerInvocationRequest req =
    req ^. triggerMetadata . at "$request" . toEither "Unable to find $request parameter"
        >>= view (maybe'http . toEither "Unexpected payload, RpcHttp is expected")
        >>= fromRpcHttp

instance OutMessage HttpResponse where
  type OutBinding HttpResponse = HttpBinding
  toInvocationResponse resp =
    let
      td = defMessage @TypedData
              & maybe'data' .~ fmap TypedData'Bytes (httpResponseBody resp)

      ht = defMessage @RpcHttp
              & headers .~ httpResponseHeaders resp
              & body .~ td

    in [defMessage & http .~ ht]

instance ToTrigger HttpBinding where
  toTriggerJSON _ = Aeson.object
        [ "type"      .= ("httpTrigger" :: Text)
        , "direction" .= ("in"          :: Text)
        , "name"      .= ("req"         :: Text)
        ]

instance ToOutBinding HttpBinding where
  toOutBindingJSON _ =
    [ Aeson.object
      [ "type"      .= ("http"    :: Text)
      , "direction" .= ("out"     :: Text)
      , "name"      .= ("$return" :: Text)
      ]
    ]

fromRpcHttp :: RpcHttp -> Either Text HttpRequest
fromRpcHttp req = do
  uri <- req ^. url . to (Text.unpack) . to parseURI . toEither "Unable to parse URI"

  pure HttpRequest
        { httpRequestMethod   = req ^. method
        , httpRequestUrl      = uri
        , httpRequestQuery    = req ^. query
        , httpRequestHeaders  = req ^. headers
        , httpRequestBody     = (req ^. maybe'rawBody >>= fromTypedData)
        }

fromTypedData :: TypedData -> Maybe ByteString
fromTypedData td =
  td ^. maybe'data' >>= \case
    TypedData'String v -> Just (Text.encodeUtf8 v)
    TypedData'Json v   -> Just (Text.encodeUtf8 v)
    TypedData'Bytes v  -> Just v
    TypedData'Stream v -> Just v
    _                  -> Nothing

