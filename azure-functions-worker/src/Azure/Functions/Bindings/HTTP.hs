module Azure.Functions.Bindings.HTTP
where

import Proto.FunctionRpc

newtype HttpRequest = HttpRequest RpcHttp deriving (Show, Eq)
