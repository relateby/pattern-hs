{-# LANGUAGE OverloadedStrings #-}
module Gramref.CLI.Commands.Parse
  ( ParseOptions(..)
  , parseOptions
  , runParse
  ) where

import Options.Applicative
import Gramref.CLI.Types (OutputFormat(..), OutputOptions(..), outputOptionsParser, enforceDeterministicCanonical)
import qualified Gramref.CLI.Output as Output
import qualified Gram.Parse as Gram
import System.Exit (ExitCode(..))
import System.IO (stdin, hGetContents)

data ParseOptions = ParseOptions
  { parseInputFile :: Maybe FilePath
  , parseFormat :: OutputFormat
  , parseOutputOptions :: OutputOptions
  } deriving (Show)

parseOptions :: Parser ParseOptions
parseOptions = ParseOptions
  <$> optional (strArgument (metavar "INPUT-FILE" <> help "Input file (or use stdin)"))
  <*> formatOption
  <*> outputOptionsParser

formatOption :: Parser OutputFormat
formatOption = option (maybeReader parseFormatStr)
  ( long "format"
  <> short 'f'
  <> metavar "FORMAT"
  <> value FormatJSON
  <> help "Output format: json, gram, or debug (default: json)"
  )

parseFormatStr :: String -> Maybe OutputFormat
parseFormatStr "json" = Just FormatJSON
parseFormatStr "gram" = Just FormatGram
parseFormatStr "debug" = Just FormatDebug
parseFormatStr _ = Nothing

runParse :: ParseOptions -> IO ExitCode
runParse opts = do
  input <- case parseInputFile opts of
    Nothing -> hGetContents stdin
    Just file -> readFile file
  
  let outputOpts = enforceDeterministicCanonical (parseOutputOptions opts)
  
  case Gram.fromGram input of
    Left err -> do
      Output.formatError (parseFormat opts) outputOpts (show err)
      return (ExitFailure 1)
    Right patterns -> do
      Output.formatOutput (parseFormat opts) outputOpts patterns
      return ExitSuccess

