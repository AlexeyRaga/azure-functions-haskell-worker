{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Azure.Functions.Bindings.HTTP
( HttpMethod(..)
, HttpRequest(..)
, HttpResponse(..)
, HttpTrigger(..)
, HttpBinding(..)
, module Azure.Functions.Bindings.Class
)
where

import           Azure.Functions.Bindings.Class
import           Azure.Functions.Bindings.Shared       (objectWithoutNulls)
import           Azure.Functions.Internal.Lens         (toEither)
import           Data.Aeson                            ((.=))
import qualified Data.Aeson                            as Aeson
import           Data.ByteString                       (ByteString)
import qualified Data.List                             as List
import qualified Data.Map                              as Map
import           Data.Map.Strict                       (Map)
import           Data.Maybe                            (fromMaybe)
import           Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import           Data.String
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text
import           GHC.Generics                          (Generic)
import           Lens.Family                           (LensLike, Phantom, to, view, (&), (.~), (^.))
import           Lens.Family.Stock                     (at)
import           Network.URI                           (URI, parseURI)
import           Proto.FunctionRpc
import           Proto.FunctionRpc_Fields

data HttpMethod
  = Get
  | Put
  | Post
  | Delete
  | Head
  | HttpMethod Text
  deriving (Eq, Show)

instance IsString HttpMethod where
  fromString = httpMethodFromText . Text.pack

httpMethodToText :: HttpMethod -> Text
httpMethodToText = \case
  Get               -> "get"
  Put               -> "put"
  Post              -> "post"
  Delete            -> "delete"
  Head              -> "head"
  HttpMethod other  -> other

httpMethodFromText :: Text -> HttpMethod
httpMethodFromText txt =
  case Text.toLower txt of
      "get"    -> Get
      "put"    -> Put
      "post"   -> Post
      "delete" -> Delete
      "head"   -> Head
      other    -> HttpMethod other

data HttpTrigger  = HttpTrigger
  { httpRoute   :: Maybe Text
  , httpMethods :: [HttpMethod]
  } deriving (Show, Eq, Generic)

data HttpBinding  = HttpBinding
  {
  } deriving (Show, Eq, Generic)

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

instance Trigger HttpRequest where
  type TriggerBinding HttpRequest = HttpTrigger
  fromInvocationRequest req =
    req ^. triggerMetadata . at "$request" . toEither "Unable to find $request parameter"
        >>= view (maybe'http . toEither "Unexpected payload, RpcHttp is expected")
        >>= fromRpcHttp

instance Output HttpResponse where
  type OutBinding HttpResponse = HttpBinding
  toOutputData resp =
    let
      td = defMessage @TypedData
              & maybe'data' .~ fmap TypedData'Bytes (httpResponseBody resp)

      ht = defMessage @RpcHttp
              & headers .~ httpResponseHeaders resp
              & body .~ td

    in [defMessage & http .~ ht]

instance ToTrigger HttpTrigger where
  toTriggerJSON a = objectWithoutNulls
        [ "type"      .= ("httpTrigger" :: Text)
        , "direction" .= ("in"          :: Text)
        , "name"      .= ("req"         :: Text)
        , "route"     .= httpRoute a
        , "methods"   .= nonEmpty (fmap httpMethodToText (httpMethods a))
        ]
    where
      nonEmpty as = if List.null as then Nothing else Just as


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

