{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Project
where

import           Data.Either.Combinators (fromRight')
import           Data.Text               (Text)
import           Templates.Utils         (toTemplate)
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

cabalFile :: String -> Template
cabalFile name = toTemplate (name <> ".cabal") [r|cabal-version: 2.24

-- This file has been generated by Azure Functions Haskell Worker.

name:           {{name}}
version:        0.1.0.0
-- synopsis:
-- description:
build-type:     Simple

-- Do NOT remove this stanza.
-- it is used by the tooling to expose automatically generated functions.
common functions
  other-modules:

executable {{name}}
  import: functions
  default-language: Haskell2010
  main-is: Main.hs
  hs-source-dirs: src
  ghc-options: -Wall
  other-modules:
      Exports
  build-depends:
      base                      >= 4.9 && < 5
    , aeson
    , azure-functions-worker
    , bytestring
    , containers
    , text
|]

exportsHs :: Template
exportsHs = toTemplate "Exports.hs" [r|
{-# LANGUAGE OverloadedStrings #-}
module Exports
( functions
)
where

import Azure.Functions.Registry

functions :: Registry
functions = mempty
|]

mainHs :: Template
mainHs = toTemplate "Main.hs" [r|
module Main where

import           Azure.Functions.Worker
import qualified Exports as Exports

main :: IO ()
main = runWorker (Exports.functions)
|]
--------------------------------------------------------------------------------




