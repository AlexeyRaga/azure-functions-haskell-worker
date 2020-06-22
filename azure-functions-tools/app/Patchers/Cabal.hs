{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Patchers.Cabal
( updateIndentedFile
, addFunctionModules
)
where

import Data.Text         (Text)
import Patchers.Indented

addFunctionModules :: IndentedLines -> [Text] -> IndentedLines
addFunctionModules lines values =
  withIndented "common functions" lines $ \funcLines ->
    withIndented "other-modules:" funcLines $ \(IndentedLines modLines) ->
      IndentedLines $ ((0,) <$> values) <> modLines
