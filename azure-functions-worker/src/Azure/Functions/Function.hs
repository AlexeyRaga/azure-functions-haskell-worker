{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StrictData                #-}
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

data Function env i o = Function
  { inBinding  :: InBinding i
  , outBinding :: OutBinding o
  , initEnv    :: IO env
  , func       :: env -> i -> IO o
  }
