{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Init
where

import           Control.Monad       (when)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Options.Applicative
import           System.Directory    as Dir
import           System.FilePath     (takeFileName, (<.>), (</>))
import qualified Templates.Project   as Prj
import qualified Templates.Utils     as Tpl

data Options = Options
  { projectDir     :: Maybe FilePath
  , syncExtensions :: Bool
  } deriving (Show, Eq, Generic)

initOptionsParser :: Parser Options
initOptionsParser = Options
  <$> optional (strOption
        (  long "project-dir"
        <> metavar "DIR"
        <> help "The directory to initialize the application in"
        ))
  <*> switch
        (  long "sync-extensions"
        <> help "Synchronize the Azure Function binding extensions"
        )

initCommand :: Parser (IO ())
initCommand = runInitCommand <$> initOptionsParser

runInitCommand :: Options -> IO ()
runInitCommand opts = do
  projectRoot <- maybe Dir.getCurrentDirectory pure (projectDir opts)
  Dir.createDirectoryIfMissing True projectRoot

  let name = takeFileName projectRoot

  Tpl.writeFileIfNotExist (projectRoot </> "host.json") Prj.hostJson []
  Tpl.writeFileIfNotExist (projectRoot </> "local.settings.json") Prj.localSettingsJson []

  Tpl.writeFileIfNotExist (projectRoot </> name <.> "cabal") (Prj.cabalFile name) [("name", Text.pack name)]

  Tpl.writeFileIfNotExist (projectRoot </> "src" </> "Main.hs") Prj.mainHs []
  Tpl.writeFileIfNotExist (projectRoot </> "src" </> "Exports.hs") Prj.exportsHs []



