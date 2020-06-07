{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Init
where

import           Control.Monad       (when)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Options.Applicative
import           System.Directory    as Dir
import           System.FilePath     (takeFileName, (</>))
import qualified Templates.IO        as Tpl
import qualified Templates.Project   as Prj

data InitOptions = InitOptions
  { scriptRoot     :: FilePath
  , syncExtensions :: Bool
  } deriving (Show, Eq, Generic)

initOptionsParser :: Parser InitOptions
initOptionsParser = InitOptions
  <$> strOption
        (  long "script-root"
        <> metavar "DIR"
        <> help "The script root directory to initialize the application in"
        )
  <*> switch
        (  long "sync-extensions"
        <> help "Synchronize the Azure Function binding extensions"
        )

initCommand :: Parser (IO ())
initCommand = runInitCommand <$> initOptionsParser

runInitCommand :: InitOptions -> IO ()
runInitCommand opts = do
  projectRoot <- Dir.makeAbsolute (scriptRoot opts)
  let name = takeFileName projectRoot

  Tpl.writeFileIfNotExist (projectRoot </> "host.json") Prj.hostJson []

  Tpl.writeFileIfNotExist (projectRoot </> "package.yaml") Prj.packageYaml [("name", Text.pack name)]

  Tpl.writeFile (projectRoot </> "src" </> "Main.hs") Prj.mainHs []



