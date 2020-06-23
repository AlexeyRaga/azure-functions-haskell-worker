{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Azure.Functions.Internal.Runtime
where

import           Azure.Functions.Registry    (RegisteredFunction (..))
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Control.Monad.STM           (atomically)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Proto.FunctionRpc

type FunctionId = Text

data RunningFunction = RunningFunction
  { runningFunction     :: InvocationRequest -> IO InvocationResponse
  } deriving (Generic)

data Runtime = Runtime
  { runtimeFunctions :: TVar (Map Text RunningFunction)
  }

createRuntime :: IO Runtime
createRuntime = Runtime <$> newTVarIO mempty

loadRuntimeFunction :: Runtime -> FunctionId -> RegisteredFunction -> IO ()
loadRuntimeFunction runtime fid f =
  atomically $ do
    modifyTVar' (runtimeFunctions runtime) $ Map.insert fid (RunningFunction $ adaptedFunction f)

getRuntimeFunction :: Runtime -> FunctionId -> IO (Maybe RunningFunction)
getRuntimeFunction runtime fid = do
  Map.lookup fid <$> readTVarIO (runtimeFunctions runtime)
