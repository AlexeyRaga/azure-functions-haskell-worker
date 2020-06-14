module Azure.Functions.Bindings.Class
where

import Data.Text         (Text)
import GHC.Generics      (Generic)
import Proto.FunctionRpc (InvocationRequest, InvocationResponse)

class FromInvocationRequest a where
  fromInvocationRequest :: InvocationRequest -> Maybe a

class ToInvocationResponse a where
  toInvocationResponse :: a -> InvocationResponse
