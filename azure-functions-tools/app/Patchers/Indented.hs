{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Patchers.Indented
where

import           Data.Bifunctor (first)
import           Data.Char      (isSpace)
import           Data.Function  (on, (&))
import           Data.Functor   ((<&>))
import qualified Data.List      as List
import           Data.Maybe     (fromMaybe, listToMaybe)
import           Data.Text      (Text)
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text

newtype IndentedLines = IndentedLines [(Int, Text)] deriving (Show)

readIndentedFile :: FilePath -> IO IndentedLines
readIndentedFile path =
  Text.readFile path <&> Text.lines <&> fmap indented <&> IndentedLines

writeIndentedFile :: FilePath -> IndentedLines -> IO ()
writeIndentedFile path (IndentedLines lines) =
  let indented i line = Text.replicate i " " <> line
  in Text.writeFile path (Text.unlines $ uncurry indented <$> lines)

updateIndentedFile :: FilePath -> (IndentedLines -> IndentedLines) -> IO ()
updateIndentedFile path f =
  readIndentedFile path >>= writeIndentedFile path . f

withIndented :: Text -> IndentedLines -> (IndentedLines -> IndentedLines) -> IndentedLines
withIndented lineText (IndentedLines lines) f =
  case List.break (isRequiredLine . snd) lines of
    (pre, []) -> IndentedLines lines
    (pre, (indent, line):post) ->
      let
        (stanzaLines, rest) = List.break (not . isBlockContent indent) post

        minIndent = stanzaLines
                      & List.filter (not . isEmptyLine)
                      & List.map fst
                      & List.sort
                      & listToMaybe
                      & fromMaybe (indent + 2)

        IndentedLines newStanza = f (stanzaLines <&> first (max 0 . subtract minIndent) & IndentedLines)
      in IndentedLines $ mconcat
                      [ pre
                      , [(indent, line)]
                      , newStanza <&> first (+minIndent)
                      , rest
                      ]
  where
    isEmptyLine (indent, line) = Text.null line
    requiredToken = Text.unwords (Text.words lineText)
    isRequiredLine line = Text.unwords (Text.words line) == requiredToken
    isBlockContent baseIndent (indent, line) = Text.null line || indent > baseIndent

indented :: Text -> (Int, Text)
indented = first Text.length . Text.break (not . isSpace) . Text.stripEnd
