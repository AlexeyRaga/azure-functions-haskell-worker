module Templates.Utils
where

import           Control.Monad           (unless, when)
import           Data.Either.Combinators (fromRight')
import           Data.Text               (Text)
import qualified Data.Text.IO            as Text
import           Prelude                 hiding (writeFile)
import           System.Directory        (createDirectoryIfMissing, doesFileExist)
import           System.FilePath         (takeDirectory)
import           Text.Glabrous           (Template)
import           Text.Glabrous           as Tpl

writeFile :: FilePath -> Template -> [(Text, Text)] -> IO ()
writeFile file tpl vars = do
  createDirectoryIfMissing True (takeDirectory file)
  let content = Tpl.process tpl (Tpl.fromList vars)
  Text.writeFile file content

writeFileIfNotExist :: FilePath -> Template -> [(Text, Text)] -> IO ()
writeFileIfNotExist file tpl vars = do
  ok <- doesFileExist file
  unless ok $ writeFile file tpl vars

writeNewFile :: FilePath -> Template -> [(Text, Text)] -> IO ()
writeNewFile file tpl vars =do
  alreadyExists <- doesFileExist file
  when alreadyExists $
    error $ "File already exists, will not overwrite: " <> file
  writeFile file tpl vars

toTemplate :: String -> Text -> Template
toTemplate name tpl =
  case Tpl.fromText tpl of
    Left err   -> error $ "Unable to create template: " <> name <> ". Error: " <> err
    Right tpl' -> tpl'
