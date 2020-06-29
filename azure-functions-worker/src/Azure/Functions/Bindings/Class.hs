-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Azure.Functions.Bindings.Class
where

import Data.Aeson                            (ToJSON, Value (Null))
import Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import Data.Proxy                            (Proxy)
import Data.Text                             (Text)
import GHC.Generics                          (Generic)
import Lens.Family                           ((&), (.~))
import Proto.FunctionRpc
import Proto.FunctionRpc_Fields

class ToInBinding ctx where
  toInBindingJSON :: ctx -> Value

class ToOutBinding ctx where
  toOutBindingJSON :: ctx -> Value

class (ToInBinding (InBinding a)) => InMessage a where
  type InBinding a :: *
  fromInvocationRequest :: InvocationRequest -> Either Text a

class (ToOutBinding (OutBinding a)) => OutMessage a where
  type OutBinding a
  toInvocationResponse :: a -> InvocationResponse

instance OutMessage () where
  type OutBinding () = ()
  toInvocationResponse _ =
    defMessage @InvocationResponse
      & result .~ (defMessage & status .~ StatusResult'Success)

instance ToOutBinding () where
  toOutBindingJSON _ = Null

