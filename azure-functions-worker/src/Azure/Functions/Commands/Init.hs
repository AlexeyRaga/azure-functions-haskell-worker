{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Azure.Functions.Commands.Init
where

import qualified Azure.Functions.Internal.Templates as Tpl
import qualified Data.Text                          as Text
import           GHC.Generics                       (Generic)
import           Options.Applicative
import           System.Directory                   as Dir
import           System.Environment                 (getExecutablePath)
import           System.FilePath                    (takeFileName, (</>))

data Options = Options
  { scriptRoot     :: String
  , syncExtensions :: Bool
  } deriving (Generic)

optionsParser :: Parser Options
optionsParser = Options
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
initCommand = runInitCommand <$> optionsParser

runInitCommand :: Options -> IO ()
runInitCommand opts = do
  funcRoot    <- Dir.getCurrentDirectory
  execPath    <- getExecutablePath
  projectRoot <- Dir.makeAbsolute (scriptRoot opts)

  let execFileName = takeFileName execPath

  let workerDir = projectRoot </> "workers" </> "haskell"
  let workerExecPath = workerDir </> execFileName

  createDirectoryIfMissing True workerDir

  Dir.copyFileWithMetadata "host.json" (projectRoot </> "host.json")
  Dir.copyFileWithMetadata "local.settings.json" (projectRoot </> "local.settings.json")

  Dir.copyFileWithMetadata execPath workerExecPath

  Tpl.writeFile (workerDir </> "worker.config.json") Tpl.workerConfigJson [("execPath", Text.pack workerExecPath)]
