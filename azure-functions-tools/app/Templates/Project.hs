{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Project
where

import Data.Text         (Text)
import Text.RawString.QQ

hostJson :: Text
hostJson = [r|
  {
      "version": "2.0",
      "extensionBundle": {
          "id": "Microsoft.Azure.Functions.ExtensionBundle",
          "version": "[1.*, 2.0.0)"
      }
  }
|]

workerConfigJson :: Text
workerConfigJson = [r|
  {
      "description": {
          "arguments": [
              "run"
          ],
          "defaultExecutablePath": "{{ execPath }}",
          "extensions": [
              ".swift"
          ],
          "language": "swift"
      }
  }
|]

localSettingsJson :: Text
localSettingsJson = [r|
  {
      "IsEncrypted": false,
      "Values": {
          "FUNCTIONS_WORKER_RUNTIME": "swift",
          "languageWorkers:workersDirectory": "workers"
      }
  }
|]
