{-# LANGUAGE TypeApplications #-}
module Azure.Functions.Bindings.Class
where

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

instance ToInvocationResponse () where
  toInvocationResponse _ =
    let stts = defMessage @StatusResult & status .~ StatusResult'Success
    in defMessage @InvocationResponse & result .~ stts
