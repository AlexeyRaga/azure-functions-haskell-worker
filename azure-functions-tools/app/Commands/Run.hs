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
import qualified System.IO.Temp      as Temp
import           System.Process      (CreateProcess (..), proc, waitForProcess, withCreateProcess)
import qualified Templates.Project   as Prj
import qualified Templates.Utils     as Tpl

type ProjectName = String

data Options = Options
  { projectDir     :: Maybe FilePath
  , scriptRoot     :: Maybe FilePath
  , syncExtensions :: Bool
  } deriving (Show, Eq, Generic)

runOptionsParser :: Parser Options
runOptionsParser = Options
  <$> optional (strOption
        (  long "project-dir"
        <> metavar "DIR"
        <> help "Project directory. If not specified, current directory is used."
        ))
  <*> optional (strOption
        (  long "script-root"
        <> metavar "DIR"
        <> help "The script root directory to initialize the application in"
        ))
  <*> switch
        (  long "sync-extensions"
        <> help "Synchronize the Azure Function binding extensions"
        )

runCommand :: Parser (IO ())
runCommand = runRunCommand <$> runOptionsParser

runRunCommand :: Options -> IO ()
runRunCommand opts = do
  funcRoot <- maybe Dir.getCurrentDirectory pure (projectDir opts)
  let name = takeFileName funcRoot

  withScriptRoot opts $ \projectRoot -> do
    let workerDir = projectRoot </> "workers" </> "haskell"
    createDirectoryIfMissing True workerDir

    print "Copying executable"
    runOSCommand funcRoot "cabal"
      [ "run"
      , "exe:" <> name
      , "--"
      , "init"
      , "--script-root"
      , projectRoot
      ]

    runOSCommand projectRoot "func"
      [ "host"
      , "start"
      ]

--------------------------------------------------------------------------------

withScriptRoot :: Options -> (FilePath -> IO a) -> IO a
withScriptRoot opts f =
  case (scriptRoot opts) of
    Just path -> f path
    Nothing ->
      Temp.withSystemTempDirectory "script-root" f


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
