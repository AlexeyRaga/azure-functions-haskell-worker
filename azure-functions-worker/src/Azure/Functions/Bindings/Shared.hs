{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Azure.Functions.Bindings.Shared
where

import           Azure.Functions.Bindings.Class
import           Control.Applicative            (Alternative, (<|>))
import           Data.Aeson                     (FromJSON, Value (Null), decodeStrict', object)
import           Data.Aeson.Types               (Pair (..))
import           Data.ByteString                (ByteString)
import           Data.String                    (IsString)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           GHC.Generics                   (Generic)
import           Lens.Family                    ((^.))
import           Proto.FunctionRpc
import           Proto.FunctionRpc_Fields

newtype ConnectionName  = ConnectionName Text deriving (Show, Eq, IsString, Generic)

objectWithoutNulls :: [Pair] -> Value
objectWithoutNulls = object . Prelude.filter (not . isNull . snd)
  where
    isNull Null = True
    isNull _    = False

decodeJson :: FromJSON a => TypedData -> Maybe a
decodeJson d =
  d ^. maybe'json >>= decodeStrict' . Text.encodeUtf8
{-# INLINE decodeJson #-}

getText :: TypedData -> Maybe Text
getText d =
  d ^. maybe'data' >>= \case
    TypedData'String v  -> Just v
    TypedData'Json v    -> Just v
    _                   -> Nothing

getBytes :: TypedData -> Maybe ByteString
getBytes td =
  td ^. maybe'data' >>= \case
    TypedData'Bytes v  -> Just v
    TypedData'Stream v -> Just v
    TypedData'String v -> Just (Text.encodeUtf8 v)
    _                  -> Nothing

infixl 3 <||>
(<||>) :: Alternative f => (a -> f x) -> (a -> f x) -> a -> f x
f <||> g = \a -> f a <|> g a
{-# INLINE (<||>) #-}
