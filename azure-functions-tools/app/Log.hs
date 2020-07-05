{-# LANGUAGE OverloadedStrings #-}
module Log
( Log.error
, info
, report
, warn
, debug
)
where

import           Data.Text    (Text)
import qualified Data.Text.IO as Text

error :: Text -> IO ()
error msg = Text.putStrLn $ "\x1b[31m" <> msg <> "\x1b[0m"

info :: Text -> IO ()
info = Text.putStrLn

report :: Text -> IO ()
report msg = Text.putStrLn $ "\x1b[32m" <> msg <> "\x1b[0m"

warn :: Text -> IO ()
warn msg = Text.putStrLn $ "\x1b[33m" <> msg <> "\x1b[0m"

debug :: Text -> IO ()
debug msg = Text.putStrLn $ "\x1b[36m" <> msg <> "\x1b[0m"
