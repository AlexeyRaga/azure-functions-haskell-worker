{-# LANGUAGE DeriveGeneric #-}
module Options.Init
where

import GHC.Generics        (Generic)
import Options.Applicative

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

runInitCommands :: InitOptions -> IO ()
runInitCommand opts = undefined
