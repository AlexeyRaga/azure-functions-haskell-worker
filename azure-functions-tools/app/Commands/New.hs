{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.New
where

import           Control.Monad       (filterM, when)
import           Data.Char           (isAlpha, isAlphaNum)
import qualified Data.List           as List
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Options.Applicative
import           System.Directory    as Dir
import           System.Exit         (ExitCode (..))
import           System.FilePath     (takeFileName, (<.>), (</>))
import qualified System.IO.Temp      as Temp
import           System.Process      (CreateProcess (..), proc, waitForProcess, withCreateProcess)
import qualified Templates.New       as TplNew
import qualified Templates.Project   as Prj
import qualified Templates.Utils     as Tpl

import Cabal.Patch as Cabal

newtype FunctionName = FunctionName Text deriving (Show, Eq, Generic)
newtype ModuleName = ModuleName Text deriving (Show, Eq, Generic)
data Options = Options
  { projectDir   :: Maybe FilePath
  , name         :: FunctionName
  , functionType :: FunctionType
  } deriving (Show, Eq, Generic)

data FunctionType
  = Http
  | ServiceBus
  deriving (Show, Eq, Generic)

runOptionsParser :: Parser Options
runOptionsParser = Options
  <$> optional (strOption
        (  long "project-dir"
        <> metavar "DIR"
        <> help "Project directory. If not specified, current directory is used."
        ))
  <*> option (eitherReader readFunctionName)
        (  long "name"
        <> metavar "NAME"
        <> help "Function name"
        )
  <*> subparser
        (  command "http" (info httpParser idm)
        <> command "service-bus" (info serviceBusParser idm)
        )

httpParser :: Parser FunctionType
httpParser = pure Http

serviceBusParser :: Parser FunctionType
serviceBusParser = pure ServiceBus

newCommand :: Parser (IO ())
newCommand = runNewCommand <$> runOptionsParser

runNewCommand :: Options -> IO ()
runNewCommand opts = do
  funcRoot <- maybe Dir.getCurrentDirectory pure (projectDir opts)
  let projectName = takeFileName funcRoot
  let ModuleName modName = toModuleName (name opts)

  let srcDir = funcRoot </> "src"
  let functionsDir = srcDir </> "Functions"

  Dir.createDirectoryIfMissing True functionsDir

  let functionFile = functionsDir </> (Text.unpack modName) <.> "hs"

  case functionType opts of
    Http       -> Tpl.writeNewFile functionFile TplNew.httpFunction [("moduleName", modName)]
    ServiceBus -> undefined

  let cabalFilePath = funcRoot </> projectName <.> "cabal"
  cabalLines <- Cabal.readCabal cabalFilePath
  let newCabalLines = Cabal.addFunctionModules cabalLines ["Functions." <> modName]
  Cabal.writeCabal cabalFilePath newCabalLines

--------------------------------------------------------------------------------
readFunctionName :: String -> Either String FunctionName
readFunctionName = \case
  [] -> Left "Function name cannot be empty"
  (x:xs) | isAlpha x && all isValid xs -> Right (FunctionName (Text.pack (x:xs)))
  wrong -> Left $ "Invalid function name: " <> wrong
  where
    isValid c = isAlphaNum c || c == '_' || c == '\''

toModuleName :: FunctionName -> ModuleName
toModuleName (FunctionName t) =
  let (x, xs) = Text.splitAt 1 t
  in ModuleName (Text.toUpper x <> xs)
