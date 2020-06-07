{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Run
where

import           Control.Monad       (filterM, when)
import qualified Data.List           as List
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Options.Applicative
import           System.Directory    as Dir
import           System.Exit         (ExitCode (..))
import           System.FilePath     (takeFileName, (</>))
import           System.Process      (CreateProcess (..), proc, waitForProcess, withCreateProcess)
import qualified Templates.IO        as Tpl
import qualified Templates.Project   as Prj

data RunOptions = RunOptions
  { scriptRoot     :: FilePath
  , syncExtensions :: Bool
  } deriving (Show, Eq, Generic)

runOptionsParser :: Parser RunOptions
runOptionsParser = RunOptions
  <$> strOption
        (  long "script-root"
        <> metavar "DIR"
        <> help "The script root directory to initialize the application in"
        )
  <*> switch
        (  long "sync-extensions"
        <> help "Synchronize the Azure Function binding extensions"
        )

runCommand :: Parser (IO ())
runCommand = runRunCommand <$> runOptionsParser

runRunCommand :: RunOptions -> IO ()
runRunCommand opts = do
  projectRoot <- Dir.makeAbsolute (scriptRoot opts)
  let name = takeFileName projectRoot

  let workerDir = projectRoot </> "workers" </> "haskell"
  createDirectoryIfMissing True workerDir

  print "Running hpack"
  runOSCommand projectRoot "hpack"
    [ projectRoot
    , "--force"
    ]

  print "Copying executable"
  runOSCommand projectRoot "cabal"
    [ "run"
    , "exe:" <> name
    , "--"
    , "init"
    , "--script-root"
    , projectRoot
    ]

  bins <- getExecutables (workerDir </> "bin")
  let execPath = case bins of
                  [bin] -> bin
                  []    -> error $ "No executables have been installed into " <> (workerDir </> "bin")
                  _     -> error $ "More than one executable found in " <> (workerDir </> "bin")

  Tpl.writeFileIfNotExist (workerDir </> "worker.config.json") Prj.workerConfigJson [("execPath", Text.pack execPath)]

  runOSCommand projectRoot "func"
    [ "host"
    , "start"
    ]

--------------------------------------------------------------------------------

runOSCommand :: FilePath -> String -> [String] -> IO ()
runOSCommand cwd cmd args = do
  res <- withCreateProcess
          (proc cmd args) { delegate_ctlc = True, cwd = Just cwd } $ \_ _ _ p ->
          waitForProcess p
  case res of
    ExitSuccess   -> pure ()
    ExitFailure r ->
      let msg = "Error running command: " <> cmd <> " " <> List.intercalate " " args
      in error msg


getExecutables :: FilePath -> IO [FilePath]
getExecutables path = do
  files <- Dir.listDirectory path
  let fullPaths = fmap (path </>) files
  filterM (fmap Dir.executable . Dir.getPermissions) fullPaths
