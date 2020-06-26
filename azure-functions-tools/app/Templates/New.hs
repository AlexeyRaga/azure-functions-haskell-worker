{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.New
where

import           Data.Either.Combinators (fromRight')
import           Data.Text               (Text)
import           Templates.Utils
import           Text.Glabrous           (Template)
import qualified Text.Glabrous           as Tpl
import           Text.RawString.QQ

httpFunction :: Template
httpFunction = toTemplate "Http Function" [r|
module Functions.{{moduleName}}
( function
)
where

import Azure.Functions.Function
import Azure.Functions.Bindings.HTTP

-- | A function is parametrised with:
--
-- * An input binding type  (how to gen an input message)
-- * An output binding type (how to use the result of the function)
-- * An envitonment initialisation action (how to create a context that is shared between functions' invocations)
-- * An input message type (relevant to the input context type via 'InBinding' instance)
-- * An output message type (relevant to the output context type via 'OutBinding' instance)
function :: Function HttpBinding HttpBinding () HttpRequest HttpResponse
function = Function
  { inBinding   = HttpBinding           -- Input binding
  , outBinding  = HttpBinding           -- Output binding
  , initEnv     = pure ()               -- A function to initialize the environment for a given function
  , func        = const execute         -- A function that takes an environment and an input and produces the result
  }

execute :: HttpRequest -> IO HttpResponse
execute request =
  pure HttpResponse
        { httpResponseStatus  = 200
        , httpResponseBody    = httpRequestBody request   -- echo the request
        , httpResponseHeaders = mempty
        }
|]


serviceBusFunction :: Template
serviceBusFunction = toTemplate "ServiceBus Function" [r|
{-# LANGUAGE OverloadedStrings #-}
module Functions.{{moduleName}}
( function
)
where

import Azure.Functions.Function
import Azure.Functions.Bindings.ServiceBus

-- | A function is parametrised with:
--
-- * An input binding type (how to gen an input message)
-- * An output binding type (how to use the result of the function)
-- * An envitonment initialisation action (how to create a context that is shared between functions' invocations)
-- * An input message type (relevant to the input context type via 'InBinding' instance)
-- * An output message type (relevant to the output context type via 'OutBinding' instance)
function :: Function ServiceBusBinding () () ReceivedMessage ()
function = Function
  { inBinding   = ServiceBusBinding (ConnectionName "{{connectionName}}") (QueueName "{{queueName}}")
  , outBinding  = ()
  , initEnv     = pure ()
  , func        = const execute
  }

execute :: ReceivedMessage -> IO ()
execute message = putStrLn "Got Message!"
|]

blobFunction :: Template
blobFunction = toTemplate "Blob Function" [r|
{-# LANGUAGE OverloadedStrings #-}
module Functions.{{moduleName}}
( function
)
where

import Azure.Functions.Function
import Azure.Functions.Bindings.Blob

-- | A function is parametrised with:
--
-- * An input binding type (how to gen an input message)
-- * An output binding type (how to use the result of the function)
-- * An envitonment initialisation action (how to create a context that is shared between functions' invocations)
-- * An input message type (relevant to the input context type via 'InBinding' instance)
-- * An output message type (relevant to the output context type via 'OutBinding' instance)
function :: Function BlobBinding () () ReceivedBlob ()
function = Function
  { inBinding   = BlobBinding (ConnectionName "{{connectionName}}") "{{namePattern}}"
  , outBinding  = ()
  , initEnv     = pure ()
  , func        = const execute
  }

execute :: ReceivedBlob -> IO ()
execute message = putStrLn "Got Blob!"
|]
