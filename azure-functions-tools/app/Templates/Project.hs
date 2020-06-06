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
hostJson = toTemplate "host.json" [r|
{
  "version": "2.0",
  "extensionBundle": {
    "id": "Microsoft.Azure.Functions.ExtensionBundle",
    "version": "[1.*, 2.0.0)"
  }
}
|]

workerConfigJson :: Template
workerConfigJson = toTemplate "worker.config.json" [r|
{
  "description": {
    "arguments": [
      "run"
    ],
    "defaultExecutablePath": "{{execPath}}",
    "extensions": [
      ".hs"
    ],
    "language": "haskell"
  }
}
|]

localSettingsJson :: Template
localSettingsJson = toTemplate "local.settings.json" [r|
{
  "IsEncrypted": false,
  "Values": {
      "FUNCTIONS_WORKER_RUNTIME": "haskell",
      "languageWorkers:workersDirectory": "workers"
  }
}
|]

packageYaml :: Template
packageYaml = toTemplate "package.yaml" [r|
name: {{name}}
version: 0.1.0.0
synopsis: Azure Function
description: Azure Function
ghc-options: -Wall

dependencies:
  - base >= 4.9 && < 5
  - aeson
  - azure-functions-worker
  - bytestring
  - text

executable:
  main: Main.hs
  source-dirs: src
|]

mainHs :: Template
mainHs = toTemplate "Main.hs" [r|
module Main where

import Azure.Functions.Worker

main :: IO ()
main = runWorker
|]
--------------------------------------------------------------------------------

toTemplate :: String -> Text -> Template
toTemplate name tpl =
  case Tpl.fromText tpl of
    Left err   -> error $ "Unable to create template: " <> name <> ". Error: " <> err
    Right tpl' -> tpl'


