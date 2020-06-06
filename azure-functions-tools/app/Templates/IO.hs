module Templates.IO
where

import           Control.Monad           (unless)
import           Data.Either.Combinators (fromRight')
import           Data.Text               (Text)
import qualified Data.Text.IO            as Text
import           Prelude                 hiding (writeFile)
import           System.Directory        (doesFileExist)
import           Text.Glabrous           (Template)
import           Text.Glabrous           as Tpl

writeFile :: FilePath -> Template -> [(Text, Text)] -> IO ()
writeFile file tpl vars =
  let content = Tpl.process tpl (Tpl.fromList vars)
  in Text.writeFile file content

writeFileIfNotExist :: FilePath -> Template -> [(Text, Text)] -> IO ()
writeFileIfNotExist file tpl vars = do
  ok <- doesFileExist file
  unless ok $ writeFile file tpl vars
