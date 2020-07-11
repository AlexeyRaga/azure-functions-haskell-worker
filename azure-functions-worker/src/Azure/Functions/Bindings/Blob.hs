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
, BlobContent(..)
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

data Blob = Blob
  { receivedBlobContent         :: BlobContent
  , receivedBlobName            :: Text
  , receivedBlobUri             :: URI
  , receivedBlobMetadata        :: Map Text Text
  , receivedBlobTriggerMetadata :: Map Text Text
  , receivedBlobProperties      :: Value
  } deriving (Show, Generic)

newtype BlobContent = BlobContent
  { unBlobContent  :: ByteString
  } deriving (Show, Generic)

instance ToInBinding BlobBinding where
  toInBindingJSON v =
    [ object
      [ "type"        .= ("blob" :: Text)
      , "direction"   .= ("in" :: Text)
      , "name"        .= ("blobData" :: Text)
      , "path"        .= blobBindingPathPattern v
      , "connection"  .= coerce @_ @Text (blobBindingConnectionName v)
      ]
    ]

instance ToTrigger BlobBinding where
  toTriggerJSON v = object
    [ "type"        .= ("blobTrigger" :: Text)
    , "direction"   .= ("in" :: Text)
    , "name"        .= ("blobTrigger" :: Text)
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

instance Trigger Blob where
  type TriggerBinding Blob = BlobBinding
  fromInvocationRequest req = do
    let idata = req ^. inputData <&> (view name &&& view data') & Map.fromList
    let tmeta = req ^. triggerMetadata

    let orMissing fld = orError ("Unable to parse " <> fld)

    content <- idata ^. at "blobTrigger" >>= getBytes         & orMissing "blobTrigger"
    name    <- tmeta ^. at "BlobTrigger" >>= getText          & orMissing "BlobTrigger"
    uri     <- tmeta ^. at "Uri" >>= decodeJson >>= parseURI  & orMissing "Uri"
    props   <- tmeta ^. at "Properties" >>= decodeJson        & maybe (pure Null) pure
    bmeta   <- tmeta ^. at "Metadata" >>= decodeJson          & maybe (pure mempty) pure

    pure Blob
      { receivedBlobContent         = BlobContent content
      , receivedBlobName            = name
      , receivedBlobUri             = uri
      , receivedBlobMetadata        = bmeta
      , receivedBlobTriggerMetadata = Map.withoutKeys tmeta (Set.fromList ["Properties", "sys", "Metadata", "BlobTrigger"]) & Map.mapMaybe getText
      , receivedBlobProperties      = props
      }

instance Input BlobContent where
  type InBinding BlobContent = BlobBinding
  fromInputData []     = Left "Unvalid inputs: No input bindings for BlobBinding"
  fromInputData (a:as) =
    case getBytes a of
      Nothing -> Left "Unepected blob data, unable to convert to ByteString"
      Just bs -> Right (BlobContent bs, as)


instance Output BlobContent where
  type OutBinding BlobContent = BlobBinding
  toOutputData (BlobContent bs) =
    [ defMessage & maybe'data' .~ Just (TypedData'Bytes bs)
    ]

