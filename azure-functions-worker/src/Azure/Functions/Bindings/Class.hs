-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Azure.Functions.Bindings.Class
where

import           Data.Aeson                            (Object (..), ToJSON, Value (Null, Object, String))
import           Data.Foldable                         (foldr')
import qualified Data.HashMap.Strict                   as HashMap
import           Data.ProtoLens.Runtime.Data.ProtoLens (defMessage)
import           Data.Proxy                            (Proxy)
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           GHC.Generics                          (Generic)
import           GHC.Generics                          (Generic)
import           Lens.Family                           ((&), (.~))
import           Proto.FunctionRpc
import           Proto.FunctionRpc_Fields

class ToTrigger a where
  toTriggerJSON :: a -> Value

class (ToTrigger (Trigger a)) => TriggerMessage a where
  type Trigger a :: *
  fromTriggerInvocationRequest :: InvocationRequest -> Either Text a

class ToInBinding a where
  toInBindingJSON :: a -> [Value]

class ToOutBinding a where
  toOutBindingJSON :: a -> [Value]

class (ToInBinding (InBinding a)) => InMessage a where
  type InBinding a :: *
  fromInvocationRequest :: InvocationRequest -> Either Text a

class (ToOutBinding (OutBinding a)) => OutMessage a where
  type OutBinding a
  toInvocationResponse :: a -> [TypedData]

instance OutMessage () where
  type OutBinding () = ()
  toInvocationResponse _ = []

instance ToOutBinding () where
  toOutBindingJSON _ = []

instance InMessage () where
  type InBinding () = ()
  fromInvocationRequest _ = Right ()

instance ToInBinding () where
  toInBindingJSON _ = []

------------------------------ TWO TUPLE --------------------------------------
instance (ToInBinding a, ToInBinding b) => ToInBinding (a, b) where
  toInBindingJSON (a, b) = mconcat
    [ toInBindingJSON a
    , toInBindingJSON b
    ]

instance (InMessage a, InMessage b) => InMessage (a, b) where
  type InBinding (a, b) = (InBinding a, InBinding b)
  fromInvocationRequest req = undefined

instance (ToOutBinding a, ToOutBinding b) => ToOutBinding (a, b) where
  toOutBindingJSON (a, b) = mconcat
    [ toOutBindingJSON a
    , toOutBindingJSON b
    ]

instance (OutMessage a, OutMessage b) => OutMessage (a, b) where
  type OutBinding (a, b) = (OutBinding a, OutBinding b)
  toInvocationResponse (a, b) = mconcat
    [ toInvocationResponse a
    , toInvocationResponse b
    ]

------------------------------ THREE TUPLE ------------------------------------
instance (ToOutBinding a, ToOutBinding b, ToOutBinding c) => ToOutBinding (a, b, c) where
  toOutBindingJSON (a, b, c) = mconcat
    [ toOutBindingJSON a
    , toOutBindingJSON b
    , toOutBindingJSON c
    ]

instance (OutMessage a, OutMessage b, OutMessage c) => OutMessage (a, b, c) where
  type OutBinding (a, b, c) = (OutBinding a, OutBinding b, OutBinding c)
  toInvocationResponse (a, b, c) = mconcat
    [ toInvocationResponse a
    , toInvocationResponse b
    , toInvocationResponse c
    ]

------------------------------ FOUR TUPLE -------------------------------------
instance (ToOutBinding a, ToOutBinding b, ToOutBinding c, ToOutBinding d) => ToOutBinding (a, b, c, d) where
  toOutBindingJSON (a, b, c, d) = mconcat
    [ toOutBindingJSON a
    , toOutBindingJSON b
    , toOutBindingJSON c
    , toOutBindingJSON d
    ]

instance (OutMessage a, OutMessage b, OutMessage c, OutMessage d) => OutMessage (a, b, c, d) where
  type OutBinding (a, b, c, d) = (OutBinding a, OutBinding b, OutBinding c, OutBinding d)
  toInvocationResponse (a, b, c, d) = mconcat
    [ toInvocationResponse a
    , toInvocationResponse b
    , toInvocationResponse c
    , toInvocationResponse d
    ]

mkInvocationResponse :: OutMessage a => a -> InvocationResponse
mkInvocationResponse msg =
  defMessage @InvocationResponse
    & maybe'returnValue .~ res
    & outputData .~ parms
    & result .~ (defMessage & status .~ StatusResult'Success)
  where
    datas = toInvocationResponse msg
    mkParamName i = Text.pack ("out_" <> show i)
    mkParameterBinding n v = defMessage @ParameterBinding & name .~ n & data' .~ v
    (_, res, parms) = foldr' (\x (i, res, out) ->
      if i == 0
        then (1, Just x, out)
        else (i+1, res, mkParameterBinding (mkParamName i) x : out)) (0, Nothing, []) datas

encodeInputBindings :: ToInBinding a => a -> [Value]
encodeInputBindings a =
  zip [0..] (toInBindingJSON a)
    & fmap (\(i, x) -> setName (mkName i) x)
  where
    setName name (Object vs) = Object $ HashMap.insert "name" (String name) vs
    mkName i = Text.pack ("in_" <> show i)

encodeOutputBindings :: ToOutBinding a => a -> [Value]
encodeOutputBindings a =
  snd $ foldr' (\x (i, res) -> (i+1, setName (mkName i) x : res)) (0, []) (toOutBindingJSON a)
  where
    setName name (Object vs) = Object $ HashMap.insert "name" (String name) vs
    mkName i = if i == 0 then "$return" else Text.pack ("out_" <> show i)

