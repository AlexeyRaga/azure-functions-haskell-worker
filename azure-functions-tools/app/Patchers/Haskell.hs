{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Patchers.Haskell
( updateIndentedFile
, addToFunction
, addToImports
)
where

import qualified Data.List         as List
import           Data.Maybe        (listToMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Patchers.Indented

addToFunction :: Text -> IndentedLines -> [Text] -> IndentedLines
addToFunction functionLine lines values =
  withIndented functionLine lines $ \(IndentedLines funcLines) ->
    IndentedLines $ ((0,) <$> values) <> funcLines

addToImports :: IndentedLines -> [Text] -> IndentedLines
addToImports (IndentedLines lines) values =
  let
    (pre, post) = List.break (Text.isPrefixOf "import " . snd) lines
    indent = maybe 0 fst (listToMaybe post)
    newLines = (indent,) <$> values
  in IndentedLines $ pre <> newLines <> post
