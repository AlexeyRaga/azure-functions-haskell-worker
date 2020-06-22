{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeApplications       #-}
module Azure.Functions.Bindings.Class
where

import Data.Aeson                            (ToJSON, Value (Null))
import Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import Data.Text                             (Text)
import GHC.Generics                          (Generic)
import Lens.Family                           ((&), (.~))
import Proto.FunctionRpc
import Proto.FunctionRpc_Fields

class FromInvocationRequest a where
  fromInvocationRequest :: InvocationRequest -> Either Text a

class ToInvocationResponse a where
  toInvocationResponse :: a -> InvocationResponse

class ToInBinding ctx where
  toInBindingJSON :: ctx -> Value

class ToOutBinding ctx where
  toOutBindingJSON :: ctx -> Value

class (ToInBinding ctx, FromInvocationRequest value) => InBinding ctx value  | ctx -> value where
class (ToOutBinding ctx, ToInvocationResponse value) => OutBinding ctx value | ctx -> value where

instance ToInvocationResponse () where
  toInvocationResponse _ =
    let stts = defMessage @StatusResult & status .~ StatusResult'Success
    in defMessage @InvocationResponse & result .~ stts

instance ToOutBinding () where
  toOutBindingJSON _ = Null

instance OutBinding () ()
