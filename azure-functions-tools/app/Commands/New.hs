{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Commands.New
where

import           Control.Monad       (filterM, when)
import           Data.Char           (isAlpha, isAlphaNum)
import qualified Data.List           as List
import           Data.String         (IsString)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           Options.Applicative
import           Patchers.Cabal      as Cabal
import           Patchers.Haskell    as Haskell
import           System.Directory    as Dir
import           System.Exit         (ExitCode (..))
import           System.FilePath     (takeFileName, (<.>), (</>))
import qualified System.IO.Temp      as Temp
import           System.Process      (CreateProcess (..), proc, waitForProcess, withCreateProcess)
import qualified Templates.New       as TplNew
import qualified Templates.Project   as Prj
import qualified Templates.Utils     as Tpl

newtype FunctionName = FunctionName { unFunctionName :: Text } deriving (Show, Eq, IsString, Generic)
newtype ModuleName   = ModuleName Text deriving (Show, Eq, IsString, Generic)

data Options = Options
  { projectDir   :: Maybe FilePath
  , name         :: FunctionName
  , functionType :: FunctionType
  } deriving (Show, Eq, Generic)

data FunctionType
  = Http
  | ServiceBus Text Text
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
serviceBusParser = ServiceBus
  <$> strOption
        (  long "connection-name"
        <> metavar "NAME"
        <> help "Name of the connection string."
        )
  <*> strOption
        (  long "queue-name"
        <> metavar "NAME"
        <> help "Name of the queue to consume."
        )

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

  let functionFile = functionsDir </> (Text.unpack (unFunctionName $ name opts)) <.> "hs"

  case functionType opts of
    Http ->
      Tpl.writeNewFile functionFile TplNew.httpFunction [("moduleName", modName)]
    ServiceBus c n ->
      Tpl.writeNewFile functionFile TplNew.serviceBusFunction
        [ ("moduleName", modName)
        , ("queueName", n)
        , ("connectionName", c)
        ]

  let cabalFilePath = funcRoot </> projectName <.> "cabal"
  Cabal.updateIndentedFile cabalFilePath $ \cabalLines ->
    Cabal.addFunctionModules cabalLines ["Functions." <> modName]

  let exportsFile = funcRoot </> "src" </> "Exports.hs"
  Haskell.updateIndentedFile exportsFile $ \exportsLines ->
    let
      withImports = Haskell.addToImports exportsLines
        ["import qualified Functions." <> modName <> " as Functions." <> modName]

    in Haskell.addToFunction "functions = mempty" withImports
        [  "<> register "
        <> quoted (unFunctionName (name opts))
        <> " Functions."
        <> modName
        <> "."
        <> "function"
        ]

--------------------------------------------------------------------------------

quoted :: Text -> Text
quoted txt = "\"" <> txt <> "\""

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
