module Azure.Functions.Worker
where

import Azure.Functions.Internal.Options (WorkerOptions (..), parseWorkerOptions)

import Proto.FunctionRpc

runWorker :: IO ()
runWorker = do
  opts <- parseWorkerOptions
  pure ()
