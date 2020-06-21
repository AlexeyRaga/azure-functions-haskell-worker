{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Patchers.Cabal
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

newtype CabalLines = CabalLines [(Int, Text)] deriving (Show)

readCabal :: FilePath -> IO CabalLines
readCabal path =
  Text.readFile path <&> Text.lines <&> fmap indented <&> CabalLines

writeCabal :: FilePath -> CabalLines -> IO ()
writeCabal path (CabalLines lines) =
  let indented i line = Text.replicate i " " <> line
  in Text.writeFile path (Text.unlines $ uncurry indented <$> lines)

withStanza :: [Text] -> CabalLines -> (CabalLines -> CabalLines) -> CabalLines
withStanza name (CabalLines lines) f =
  case List.break (isRequiredStanza . snd) lines of
    (pre, []) -> CabalLines lines
    (pre, (indent, line):post) ->
      let
        (stanzaLines, rest) = List.break (not . isStanzaContent indent) post

        minIndent = stanzaLines
                      & List.filter (not . isEmptyLine)
                      & List.map fst
                      & List.sort
                      & listToMaybe
                      & fromMaybe (indent + 2)

        CabalLines newStanza = f (stanzaLines <&> first (max 0 . subtract minIndent) & CabalLines)
      in CabalLines $ mconcat
                      [ pre
                      , [(indent, line)]
                      , newStanza <&> first (+minIndent)
                      , rest
                      ]
  where
    isEmptyLine (indent, line) = Text.null line
    stanzaName = Text.unwords name
    isRequiredStanza line = Text.unwords (Text.words line) == stanzaName
    isStanzaContent baseIndent (indent, line) = Text.null line || indent > baseIndent

addFunctionModules :: CabalLines -> [Text] -> CabalLines
addFunctionModules lines values =
  withStanza ["common", "functions"] lines $ \funcLines ->
    withStanza ["exposed-modules:"] funcLines $ \(CabalLines modLines) ->
      CabalLines $ ((0,) <$> values) <> modLines

indented :: Text -> (Int, Text)
indented = first Text.length . Text.break (not . isSpace) . Text.stripEnd
