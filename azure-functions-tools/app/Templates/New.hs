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
import Data.Text (Text)

-- | A function is parametrised with:
--
-- * An envitonment initialisation action (how to create a context that is shared between functions' invocations)
-- * An input message type (relevant to the input binding type via 'InBinding')
-- * An output message type (relevant to the output binding type via 'OutBinding')
function :: Function () HttpRequest () HttpResponse
function = Function
  { trigger  = HttpTrigger Nothing [Get]    -- What triggers the function
  , inputs   = ()                           -- Input bindings
  , outputs  = HttpBinding                  -- Output bindings
  , initEnv  = pure ()                      -- A function to initialize the environment for a given function
  , func     = \_env t _ins -> execute t    -- A function that takes an environment, trigger and inputs, and produces the result
  }

execute :: HttpRequest -> IO (Either Text HttpResponse)
execute request = pure . Right $
  HttpResponse
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
import Data.Text (Text)

-- | A function is parametrised with:
--
-- * An envitonment initialisation action (how to create a context that is shared between functions' invocations)
-- * An input message type (relevant to the input binding type via 'InBinding')
-- * An output message type (relevant to the output binding type via 'OutBinding')
function :: Function () ReceivedMessage () ()
function = Function
  { trigger  = ServiceBusBinding (ConnectionName "{{connectionName}}") (QueueName "{{queueName}}")
  , inputs   = ()
  , outputs  = ()
  , initEnv  = pure ()
  , func     = \_env t _ins -> execute t
  }

execute :: ReceivedMessage -> IO (Either Text ())
execute message = Right <$> putStrLn "Got Message!"
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
import Data.Text (Text)

-- | A function is parametrised with:
--
-- * An envitonment initialisation action (how to create a context that is shared between functions' invocations)
-- * An input message type (relevant to the input binding type via 'InBinding' instance)
-- * An output message type (relevant to the output binding type via 'OutBinding' instance)
function :: Function () ReceivedBlob () ()
function = Function
  { trigger  = BlobBinding (ConnectionName "{{connectionName}}") "{{namePattern}}"
  , inputs   = ()
  , outputs  = ()
  , initEnv  = pure ()
  , func     = \_env t _ins -> execute t
  }

execute :: ReceivedBlob -> IO (Either Text ())
execute message = Right <$> putStrLn "Got Blob!"
|]
