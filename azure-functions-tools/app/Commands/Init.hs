{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Init
where

import           Control.Monad       (when)
import           GHC.Generics        (Generic)
import           Options.Applicative
import           System.Directory
import           System.FilePath     ((</>))
import qualified Templates.IO        as Tpl

import qualified Templates.Project as Prj

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
  let execPath = "boo"
  let workerDir = scriptRoot opts </> "workers" </> "haskell"
  createDirectoryIfMissing True workerDir

  Tpl.writeFileIfNotExist (scriptRoot opts </> "host.json") Prj.hostJson []
  Tpl.writeFileIfNotExist (workerDir </> "worker.config.json") Prj.workerConfigJson [("execPath", execPath)]

  undefined


