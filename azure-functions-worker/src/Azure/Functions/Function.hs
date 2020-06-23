{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StrictData             #-}
module Azure.Functions.Function
where

import           Azure.Functions.Bindings.Class
import           Data.Aeson                            (Value)
import           Data.Functor                          ((<&>))
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.ProtoLens.Runtime.Data.ProtoLens as PL
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Lens.Family                           ((&), (.~), (^.))
import           Proto.FunctionRpc
import qualified Proto.FunctionRpc_Fields              as Fields
import           Proto.FunctionRpc_Helpers             (failureStatus, rpcLogError, rpcLogInfo, toResponse, toResponseLogError')

data Function ctxIn ctxOut i o = Function
  { inBinding  :: ctxIn
  , outBinding :: ctxOut
  , func       :: i -> IO o
  } deriving (Generic)

mkFunction :: (InBinding ctxIn i, OutBinding ctxOut o)
  => ctxIn
  -> ctxOut
  -> (i -> IO o)
  -> Function ctxIn ctxOut i o
mkFunction inBinding outBinding func = Function{..}
