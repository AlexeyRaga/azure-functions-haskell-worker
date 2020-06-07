{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Azure.Functions.Internal.Templates
where

import           Control.Monad     (unless)
import           Data.Text         (Text)
import qualified Data.Text.IO      as Text
import           Prelude           hiding (writeFile)
import           System.Directory  (createDirectoryIfMissing, doesFileExist)
import           System.FilePath   (takeDirectory)
import           Text.Glabrous     (Template)
import qualified Text.Glabrous     as Tpl
import           Text.RawString.QQ


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

writeFile :: FilePath -> Template -> [(Text, Text)] -> IO ()
writeFile file tpl vars = do
  createDirectoryIfMissing True (takeDirectory file)
  let content = Tpl.process tpl (Tpl.fromList vars)
  Text.writeFile file content

writeFileIfNotExist :: FilePath -> Template -> [(Text, Text)] -> IO ()
writeFileIfNotExist file tpl vars = do
  ok <- doesFileExist file
  unless ok $ writeFile file tpl vars

toTemplate :: String -> Text -> Template
toTemplate name tpl =
  case Tpl.fromText tpl of
    Left err   -> error $ "Unable to create template: " <> name <> ". Error: " <> err
    Right tpl' -> tpl'
