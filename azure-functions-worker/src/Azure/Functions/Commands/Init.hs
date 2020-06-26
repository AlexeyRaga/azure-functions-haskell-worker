{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Azure.Functions.Commands.Init
where

import qualified Azure.Functions.Internal.Templates as Tpl
import           Azure.Functions.Registry           (RegisteredFunction (..), Registry (..))
import           Control.Monad                      (forM_, unless)
import           Data.Aeson                         (Value (Null), object, (.=))
import qualified Data.Aeson                         as Aeson
import qualified Data.Map.Strict                    as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.IO                       as Text
import           GHC.Generics                       (Generic)
import           Options.Applicative
import           System.Directory                   as Dir
import           System.Environment                 (getExecutablePath)
import           System.FilePath                    (takeFileName, (<.>), (</>))

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

initCommand :: Registry -> Parser (IO ())
initCommand registry = runInitCommand registry <$> optionsParser

runInitCommand :: Registry -> Options -> IO ()
runInitCommand registry opts = do
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

  forM_ (Map.toList (registeredFunctions registry)) (uncurry $ initRegisteredFunction projectRoot)

initRegisteredFunction :: FilePath -> Text -> RegisteredFunction -> IO ()
initRegisteredFunction path functionName function = do
  let functionPath = path </> (Text.unpack functionName)
  let jsonFile     = functionPath </> "function.json"
  let functionFile = functionPath </> (Text.unpack functionName) <.> "hs"

  createDirectoryIfMissing True functionPath

  Aeson.encodeFile jsonFile $
    object  [ "generatedBy" .= ("azure-functions-haskell-worker" :: Text)
            , "disabled" .= False
            , "bindings" .= filter (/= Null)
                              [ registeredInBinding function
                              , registeredOutBinding function
                              ]
            ]

  Text.writeFile functionFile ""



