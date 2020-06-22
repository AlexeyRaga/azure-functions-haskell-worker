{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
module Azure.Functions.Bindings.ServiceBus
where

import           Azure.Functions.Bindings.Class
import           Azure.Functions.Internal.Lens  (orError)
import           Control.Applicative            (Alternative, (<|>))
import           Control.Arrow                  ((&&&))
import           Data.Aeson                     (FromJSON, ToJSON (..), Value (Null), decodeStrict', object, (.=))
import           Data.ByteString                (ByteString)
import           Data.Functor                   ((<&>))
import           Data.Int                       (Int32, Int64)
import qualified Data.List                      as List
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           Data.Time                      (UTCTime)
import           GHC.Generics                   (Generic)
import           Lens.Family                    (view, (&), (^.))
import           Lens.Family.Stock              (at)
import           Proto.FunctionRpc
import           Proto.FunctionRpc_Fields
import           Text.Read                      (readMaybe)

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

data ServiceBusQueue = ServiceBusQueue
  { serviceBusQueueName :: Text
  }

instance InBinding ServiceBusQueue ReceivedMessage

instance ToInBinding ServiceBusQueue where
  toInBindingJSON v = object
    [ "name"          .= ("queueTrigger" :: Text)
    , "type"          .= ("serviceBusTrigger" :: Text)
    , "direction"     .= ("in" :: Text)
    , "queueName"     .= serviceBusQueueName v
    , "connection"    .= ("ServiceBus" :: Text)
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

instance FromInvocationRequest ReceivedMessage where
  fromInvocationRequest req = do
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

readPrimitiveData :: Read a => TypedData -> Maybe a
readPrimitiveData d =
  case (d ^. maybe'data') of
    Just (TypedData'String v) -> readMaybe (Text.unpack v)
    Just (TypedData'Json v)   -> readMaybe (Text.unpack v)
    _                         -> Nothing

getText :: TypedData -> Maybe Text
getText d =
  d ^. maybe'data' >>= \case
    TypedData'String v  -> Just v
    TypedData'Json v    -> Just v
    _                   -> Nothing

readText :: Read a => TypedData -> Maybe a
readText d =
  d ^.  maybe'string >>= readMaybe . Text.unpack

decodeJson :: FromJSON a => TypedData -> Maybe a
decodeJson d =
  d ^. maybe'json >>= decodeStrict' . Text.encodeUtf8

infixl 3 <||>
(<||>) :: Alternative f => (a -> f x) -> (a -> f x) -> a -> f x
f <||> g = \a -> f a <|> g a
{-# INLINE (<||>) #-}
