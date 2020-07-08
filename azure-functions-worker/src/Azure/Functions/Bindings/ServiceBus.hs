{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Azure.Functions.Bindings.ServiceBus
( QueueName(..)
, ConnectionName(..)
, ServiceBusBinding(..)
, ReceivedMessage(..)
)
where

import           Azure.Functions.Bindings.Class
import           Azure.Functions.Bindings.Shared
import           Azure.Functions.Internal.Lens   (orError)
import           Control.Applicative             (Alternative, (<|>))
import           Control.Arrow                   ((&&&))
import           Data.Aeson                      (FromJSON, ToJSON (..), Value (Null), decodeStrict', object, (.=))
import           Data.ByteString                 (ByteString)
import           Data.Coerce                     (coerce)
import           Data.Functor                    ((<&>))
import           Data.Int                        (Int32, Int64)
import qualified Data.List                       as List
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe)
import           Data.String                     (IsString)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Data.Time                       (UTCTime)
import           GHC.Generics                    (Generic)
import           Lens.Family                     (view, (&), (^.))
import           Lens.Family.Stock               (at)
import           Proto.FunctionRpc
import           Proto.FunctionRpc_Fields
import           Text.Read                       (readMaybe)

deliveryCountKey    = "DeliveryCount"
deadLetterSourceKey = "DeadLetterSource"
expirationTimeKey   = "ExpiresAtUtc"
enqueuedTimeKey     = "EnqueuedTimeUtc"
messageIdKey        = "MessageId"
contentTypeKey      = "ContentType"
replyToKey          = "ReplyTo"
sequenceNumberKey   = "SequenceNumber"
toKey               = "To"
labelKey            = "Label"
correlationIdKey    = "CorrelationId"
userPropertiesKey   = "UserProperties"

newtype QueueName       = QueueName Text deriving (Show, Eq, IsString, Generic)

data ServiceBusBinding = ServiceBusBinding
  { serviceBusConnectionName :: ConnectionName
  , serviceBusQueueName      :: QueueName
  }

instance ToTrigger ServiceBusBinding where
  toTriggerJSON v = object
    [ "name"          .= ("queueTrigger" :: Text)
    , "type"          .= ("serviceBusTrigger" :: Text)
    , "direction"     .= ("in" :: Text)
    , "queueName"     .= coerce @_ @Text (serviceBusQueueName v)
    , "connection"    .= coerce @_ @Text (serviceBusConnectionName v)
    , "accessRights"  .= ("Listen" :: Text)
    ]

data ReceivedMessage = ReceivedMessage
  { receivedMessageId               :: Text            -- ^ The user-defined value that Service Bus can use to identify duplicate messages, if enabled.
  , receivedMessageBody             :: ByteString      -- ^ The message that triggered the function.
  , receivedMessageDeliveryCount    :: Int32           -- ^ The number of deliveries.
  , receivedMessageDeadLetterSource :: Maybe Text      -- ^ The dead letter source.
  , receivedMessageEnqueuedTime     :: UTCTime         -- ^ The time that the message was enqueued.
  , receivedMessageExpirationTime   :: UTCTime         -- ^ The time that the message expires.
  , receivedMessageContentType      :: Maybe Text      -- ^ The content type identifier utilized by the sender and receiver for application specific logic.
  , receivedMessageReplyTo          :: Maybe Text      -- ^ The reply to queue address.
  , receivedMessageSequenceNumber   :: Int64           -- ^ The unique number assigned to a message by the Service Bus.
  , receivedMessageTo               :: Maybe Text      -- ^ The send to address.
  , receivedMessageLabel            :: Maybe Text      -- ^ The application specific label.
  , receivedMessageCorrelationId    :: Maybe Text      -- ^ The correlation ID.
  , receivedMessageUserProperties   :: Map Text Text   -- ^ The application specific message properties.
  } deriving (Show, Eq, Generic)

instance TriggerMessage ReceivedMessage where
  type Trigger ReceivedMessage = ServiceBusBinding
  fromTriggerInvocationRequest req = do
    let idata = req ^. inputData <&> (view name &&& view data') & Map.fromList
    let metadata = req ^. triggerMetadata

    let orMissing fld = orError ("Unable to parse " <> fld)

    mid    <- metadata ^. at messageIdKey       >>= view maybe'string & orMissing messageIdKey
    seq    <- metadata ^. at sequenceNumberKey  >>= decodeJson & orMissing sequenceNumberKey
    body   <- idata    ^. at "queueTrigger"     >>= getText    & orMissing "queueTrigger"
    delCnt <- metadata ^. at deliveryCountKey   >>= decodeJson & orMissing deliveryCountKey
    queued <- metadata ^. at enqueuedTimeKey    >>= decodeJson & orMissing enqueuedTimeKey
    expire <- metadata ^. at expirationTimeKey  >>= decodeJson & orMissing expirationTimeKey

    pure ReceivedMessage
          { receivedMessageId               = mid
          , receivedMessageBody             = Text.encodeUtf8 body
          , receivedMessageDeliveryCount    = delCnt
          , receivedMessageDeadLetterSource = metadata ^. at deadLetterSourceKey >>= getText
          , receivedMessageExpirationTime   = expire
          , receivedMessageEnqueuedTime     = queued
          , receivedMessageContentType      = metadata ^. at contentTypeKey >>= getText
          , receivedMessageReplyTo          = metadata ^. at replyToKey >>= getText
          , receivedMessageSequenceNumber   = seq
          , receivedMessageTo               = metadata ^. at toKey >>= getText
          , receivedMessageLabel            = metadata ^. at labelKey >>= getText
          , receivedMessageCorrelationId    = metadata ^. at correlationIdKey >>= getText
          , receivedMessageUserProperties   = metadata ^. at userPropertiesKey >>= decodeJson & fromMaybe mempty
          }
