module Azure.Functions.Worker
where

import           Azure.Functions.Commands.Init (initCommand)
import           Azure.Functions.Commands.Run  (runCommand)
import           Control.Monad                 (join)
import           Options.Applicative
import qualified System.IO                     as IO

import Proto.FunctionRpc

runWorker :: IO ()
runWorker = do
  IO.hSetBuffering IO.stderr IO.LineBuffering
  join $ customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (commands <**> helper) idm)

commands :: Parser (IO ())
commands = hsubparser
  (  command "init" (info initCommand idm)
  <> command "run" (info runCommand idm)
  )
