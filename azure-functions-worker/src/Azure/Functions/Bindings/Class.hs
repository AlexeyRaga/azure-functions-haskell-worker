{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
import           Text.Printf                           (printf)

class ToTrigger a where
  toTriggerJSON :: a -> Value

class (ToTrigger (TriggerBinding a)) => Trigger a where
  type TriggerBinding a :: *
  fromInvocationRequest :: InvocationRequest -> Either Text a

class ToInBinding a where
  toInBindingJSON :: a -> [Value]

class ToOutBinding a where
  toOutBindingJSON :: a -> [Value]

class (ToInBinding (InBinding a)) => Input a where
  type InBinding a :: *
  fromInputData :: [TypedData] -> Either Text (a, [TypedData])

class (ToOutBinding (OutBinding a)) => Output a where
  type OutBinding a
  toOutputData :: a -> [TypedData]

instance Output () where
  type OutBinding () = ()
  toOutputData _ = []

instance ToOutBinding () where
  toOutBindingJSON _ = []

instance Input () where
  type InBinding () = ()
  fromInputData xs = Right ((), xs)

instance ToInBinding () where
  toInBindingJSON _ = []

------------------------------ TWO TUPLE --------------------------------------
instance (ToInBinding a, ToInBinding b) => ToInBinding (a, b) where
  toInBindingJSON (a, b) = mconcat
    [ toInBindingJSON a
    , toInBindingJSON b
    ]

instance (Input a, Input b) => Input (a, b) where
  type InBinding (a, b) = (InBinding a, InBinding b)
  fromInputData as = do
      (a, as1) <- fromInputData as
      (b, as2) <- fromInputData as1
      pure ((a, b), as2)

instance (ToOutBinding a, ToOutBinding b) => ToOutBinding (a, b) where
  toOutBindingJSON (a, b) = mconcat
    [ toOutBindingJSON a
    , toOutBindingJSON b
    ]

instance (Output a, Output b) => Output (a, b) where
  type OutBinding (a, b) = (OutBinding a, OutBinding b)
  toOutputData (a, b) = mconcat
    [ toOutputData a
    , toOutputData b
    ]

------------------------------ THREE TUPLE ------------------------------------
instance (ToInBinding a, ToInBinding b, ToInBinding c) => ToInBinding (a, b, c) where
  toInBindingJSON (a, b, c) = mconcat
    [ toInBindingJSON a
    , toInBindingJSON b
    , toInBindingJSON c
    ]

instance (Input a, Input b, Input c) => Input (a, b, c) where
  type InBinding (a, b, c) = (InBinding a, InBinding b, InBinding c)
  fromInputData as = do
      (a, as1) <- fromInputData as
      (b, as2) <- fromInputData as1
      (c, as3) <- fromInputData as2
      pure ((a, b, c), as3)

instance (ToOutBinding a, ToOutBinding b, ToOutBinding c) => ToOutBinding (a, b, c) where
  toOutBindingJSON (a, b, c) = mconcat
    [ toOutBindingJSON a
    , toOutBindingJSON b
    , toOutBindingJSON c
    ]

instance (Output a, Output b, Output c) => Output (a, b, c) where
  type OutBinding (a, b, c) = (OutBinding a, OutBinding b, OutBinding c)
  toOutputData (a, b, c) = mconcat
    [ toOutputData a
    , toOutputData b
    , toOutputData c
    ]

------------------------------ FOUR TUPLE -------------------------------------
instance (ToInBinding a, ToInBinding b, ToInBinding c, ToInBinding d) => ToInBinding (a, b, c, d) where
  toInBindingJSON (a, b, c, d) = mconcat
    [ toInBindingJSON a
    , toInBindingJSON b
    , toInBindingJSON c
    , toInBindingJSON d
    ]

instance (Input a, Input b, Input c, Input d) => Input (a, b, c, d) where
  type InBinding (a, b, c, d) = (InBinding a, InBinding b, InBinding c, InBinding d)
  fromInputData as = do
      (a, as1) <- fromInputData as
      (b, as2) <- fromInputData as1
      (c, as3) <- fromInputData as2
      (d, as4) <- fromInputData as3
      pure ((a, b, c, d), as4)

instance (ToOutBinding a, ToOutBinding b, ToOutBinding c, ToOutBinding d) => ToOutBinding (a, b, c, d) where
  toOutBindingJSON (a, b, c, d) = mconcat
    [ toOutBindingJSON a
    , toOutBindingJSON b
    , toOutBindingJSON c
    , toOutBindingJSON d
    ]

instance (Output a, Output b, Output c, Output d) => Output (a, b, c, d) where
  type OutBinding (a, b, c, d) = (OutBinding a, OutBinding b, OutBinding c, OutBinding d)
  toOutputData (a, b, c, d) = mconcat
    [ toOutputData a
    , toOutputData b
    , toOutputData c
    , toOutputData d
    ]

-------------------------------------------------------------------------------

mkInvocationResponse :: Output a => a -> InvocationResponse
mkInvocationResponse msg =
  defMessage @InvocationResponse
    & maybe'returnValue .~ res
    & outputData .~ parms
    & result .~ (defMessage & status .~ StatusResult'Success)
  where
    datas = toOutputData msg
    mkParamName i = Text.pack ("out_" <> show i)
    mkParameterBinding n v = defMessage @ParameterBinding & name .~ n & data' .~ v
    (_, res, parms) = foldr' (\x (i, res, out) ->
      if i == 0
        then (1, Just x, out)
        else (i+1, res, mkParameterBinding (mkParamName i) x : out)) (0, Nothing, []) datas

encodeInputBindings :: ToInBinding a => a -> [Value]
encodeInputBindings a =
  zip [(0::Int) ..] (toInBindingJSON a)
    & fmap (\(i, x) -> setName (mkName i) x)
  where
    setName name (Object vs) = Object $ HashMap.insert "name" (String name) vs
    mkName i = Text.pack ("in_" <> printf "%02d" i)

encodeOutputBindings :: ToOutBinding a => a -> [Value]
encodeOutputBindings a =
  snd $ foldr' (\x (i, res) -> (i+1, setName (mkName i) x : res)) (0::Int, []) (toOutBindingJSON a)
  where
    setName name (Object vs) = Object $ HashMap.insert "name" (String name) vs
    mkName i = if i == 0 then "$return" else Text.pack ("out_" <> printf "%02d" i)

