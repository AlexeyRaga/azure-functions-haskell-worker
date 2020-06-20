module Main where

import           Commands.Init       (initCommand)
import           Commands.New        (newCommand)
import           Commands.Run        (runCommand)
import           Control.Monad       (join)
import           Options.Applicative
import qualified System.IO           as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stderr IO.LineBuffering
  join $ customExecParser
    (prefs $ showHelpOnEmpty <> showHelpOnError)
    (info (commands <**> helper) idm)

commands :: Parser (IO ())
commands = hsubparser
  (  command "init" (info initCommand idm)
  <> command "new" (info newCommand idm)
  <> command "run" (info runCommand idm)
  )
