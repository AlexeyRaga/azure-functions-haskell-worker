{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Azure.Functions.Bindings.Shared
where

import Data.String  (IsString)
import Data.Text    as Text
import GHC.Generics (Generic)

newtype ConnectionName  = ConnectionName Text deriving (Show, Eq, IsString, Generic)
