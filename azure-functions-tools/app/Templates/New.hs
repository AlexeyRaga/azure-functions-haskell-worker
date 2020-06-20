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

Azure.Functions.Function
import Azure.Funtions.Bindings.Http

function :: Function HttpBinding HttpBinding HttpRequest HttpResponse
function = Function
  { inBinding   = HttpBinding
  , outBinding  = HttpBinding
  , func        = execute
  }

execute :: HttpRequest -> IO HttpResponse
execute request =
  pure HttpResponse
        { httpResponseStatus  = 200
        , httpResponseBody    = httpRequestBody request
        , httpResponseHeaders = mempty
        }
|]
