module Azure.Functions.Worker
( runWorker
)
where

import           Azure.Functions.Commands.Init (initCommand)
import           Azure.Functions.Commands.Run  (runCommand)
import           Azure.Functions.Registry      (Registry)
import           Control.Monad                 (join)
import           Options.Applicative
import           System.Environment            (getArgs)
import qualified System.IO                     as IO

runWorker :: Registry -> IO ()
runWorker registry = do
  IO.hSetBuffering IO.stderr IO.LineBuffering
  join $ customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (commands registry <**> helper) idm)

commands :: Registry -> Parser (IO ())
commands registry = hsubparser
  (  command "init" (info (initCommand registry) idm)
  <> command "run" (info (runCommand registry) idm)
  )
