{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Project
where

import           Data.Either.Combinators (fromRight')
import           Data.Text               (Text)
import           Text.Glabrous           (Template)
import qualified Text.Glabrous           as Tpl
import           Text.RawString.QQ

hostJson :: Template
hostJson = fromRight' $ Tpl.fromText [r|
  {
      "version": "2.0",
      "extensionBundle": {
          "id": "Microsoft.Azure.Functions.ExtensionBundle",
          "version": "[1.*, 2.0.0)"
      }
  }
|]

workerConfigJson :: Template
workerConfigJson = fromRight' $ Tpl.fromText [r|
  {
      "description": {
          "arguments": [
              "run"
          ],
          "defaultExecutablePath": "{{ execPath }}",
          "extensions": [
              ".hs"
          ],
          "language": "haskell"
      }
  }
|]

localSettingsJson :: Template
localSettingsJson = fromRight' $ Tpl.fromText [r|
  {
      "IsEncrypted": false,
      "Values": {
          "FUNCTIONS_WORKER_RUNTIME": "haskell",
          "languageWorkers:workersDirectory": "workers"
      }
  }
|]

