{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Azure.Functions.Registry
where

import           Azure.Functions.Function
import           Azure.Functions.Bindings.Class
import           Data.Aeson (Value)
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Lens.Family                           ((&), (.~), (^.))
import           Proto.FunctionRpc
import qualified Proto.FunctionRpc_Fields              as Fields
import           Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import           Proto.FunctionRpc_Helpers             (failureStatus)

data RegisteredFunction = forall env. RegisteredFunction
  { registeredInBinding   :: Value
  , registeredOutBinding  :: Value
  , registeredEnvFactory  :: IO env
  , registeredFunction    :: env -> InvocationRequest -> IO InvocationResponse
  }

newtype Registry = Registry
  { registeredFunctions :: Map Text RegisteredFunction
  } deriving (Generic, Semigroup)
    deriving Monoid via (Map Text RegisteredFunction)

getFunction :: Registry -> Text -> Maybe RegisteredFunction
getFunction registry name =
  Map.lookup name (registeredFunctions registry)

register :: (InBinding ctxIn i, OutBinding ctxOut o)
  => Text
  -> Function ctxIn ctxOut env i o
  -> Registry
register functionName function =
  Registry $ Map.singleton functionName RegisteredFunction
    { registeredInBinding      = toInBindingJSON (inBinding function)
    , registeredOutBinding     = toOutBindingJSON (outBinding function)
    , registeredEnvFactory  = initEnv function
    , registeredFunction       = invoke
    }
  where
    invoke ctx req = do
      case fromInvocationRequest req of
        Left err -> pure $
          defMessage
            & Fields.invocationId .~ (req ^. Fields.invocationId)
            & Fields.result .~ failureStatus ("Unable to parse request: " <> err)
        Right req' -> do
          resp <- toInvocationResponse <$> (func function ctx) req'
          pure $ resp & Fields.invocationId .~ (req ^. Fields.invocationId)
