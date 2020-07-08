{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Azure.Functions.Bindings.Blob
( ConnectionName(..)
, BlobBinding(..)
, ReceivedBlob(..)
, Blob(..)
)
where

import           Azure.Functions.Bindings.Class
import           Azure.Functions.Bindings.Shared
import           Azure.Functions.Internal.Lens         (orError)
import           Control.Arrow                         ((&&&))
import           Data.Aeson                            (FromJSON, ToJSON (..), Value (Null), decodeStrict', object, (.=))
import           Data.ByteString                       (ByteString)
import           Data.Coerce                           (coerce)
import           Data.Functor                          ((<&>))
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import qualified Data.Set                              as Set
import           Data.Text                             (Text)
import qualified Data.Text.Encoding                    as Text
import           GHC.Generics                          (Generic)
import           Lens.Family                           (view, (&), (.~), (^.))
import           Lens.Family.Stock                     (at)
import           Network.URI                           (URI, parseURI)
import           Proto.FunctionRpc
import           Proto.FunctionRpc_Fields

data BlobBinding = BlobBinding
  { blobBindingConnectionName :: ConnectionName
  , blobBindingPathPattern    :: Text
  } deriving (Generic)

data ReceivedBlob = ReceivedBlob
  { receivedBlobContent         :: ByteString
  , receivedBlobName            :: Text
  , receivedBlobUri             :: URI
  , receivedBlobMetadata        :: Map Text Text
  , receivedBlobTriggerMetadata :: Map Text Text
  , receivedBlobProperties      :: Value
  } deriving (Show, Generic)

data Blob = Blob
  { sentBlobContent  :: ByteString
  } deriving (Show, Generic)

instance ToInBinding BlobBinding where
  toInBindingJSON v =
    [ object
      [ "type"        .= ("blobTrigger" :: Text)
      , "direction"   .= ("in" :: Text)
      , "name"        .= ("blobData" :: Text)
      , "path"        .= blobBindingPathPattern v
      , "connection"  .= coerce @_ @Text (blobBindingConnectionName v)
      ]
    ]

instance ToTrigger BlobBinding where
  toTriggerJSON v = object
    [ "type"        .= ("blob" :: Text)
    , "direction"   .= ("in" :: Text)
    , "name"        .= ("blobData" :: Text)
    , "path"        .= blobBindingPathPattern v
    , "connection"  .= coerce @_ @Text (blobBindingConnectionName v)
    ]

instance ToOutBinding BlobBinding where
  toOutBindingJSON v =
    [ object
      [ "type"        .= ("blob" :: Text)
      , "direction"   .= ("out" :: Text)
      , "name"        .= ("$return" :: Text)
      , "path"        .= blobBindingPathPattern v
      , "connection"  .= coerce @_ @Text (blobBindingConnectionName v)
      ]
    ]

instance InMessage ReceivedBlob where
  type InBinding ReceivedBlob = BlobBinding
  fromInvocationRequest req = do
    let idata = req ^. inputData <&> (view name &&& view data') & Map.fromList
    let tmeta = req ^. triggerMetadata

    let orMissing fld = orError ("Unable to parse " <> fld)

    content <- idata ^. at "blobData"    >>= getBytes         & orMissing "blobData"
    name    <- tmeta ^. at "BlobTrigger" >>= getText          & orMissing "BlobTrigger"
    uri     <- tmeta ^. at "Uri" >>= decodeJson >>= parseURI  & orMissing "Uri"
    props   <- tmeta ^. at "Properties" >>= decodeJson        & maybe (pure Null) pure
    bmeta   <- tmeta ^. at "Metadata" >>= decodeJson          & maybe (pure mempty) pure

    pure ReceivedBlob
      { receivedBlobContent         = content
      , receivedBlobName            = name
      , receivedBlobUri             = uri
      , receivedBlobMetadata        = bmeta
      , receivedBlobTriggerMetadata = Map.withoutKeys tmeta (Set.fromList ["Properties", "sys", "Metadata", "BlobTrigger"]) & Map.mapMaybe getText
      , receivedBlobProperties      = props
      }

instance OutMessage Blob where
  type OutBinding Blob = BlobBinding
  toInvocationResponse resp =
    [ defMessage @TypedData
              & maybe'data' .~ Just (TypedData'Bytes (sentBlobContent resp))
    ]

