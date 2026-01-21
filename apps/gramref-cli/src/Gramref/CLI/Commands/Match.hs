{-# LANGUAGE OverloadedStrings #-}
module Gramref.CLI.Commands.Match
  ( MatchOptions(..)
  , matchOptions
  , runMatch
  ) where

import Options.Applicative
import Gramref.CLI.Types (OutputFormat(..), OutputOptions(..), outputOptionsParser, enforceDeterministicCanonical)
import qualified Gramref.CLI.Output as Output
import qualified Gram.Parse as Gram
import System.Exit (ExitCode(..))

data MatchOptions = MatchOptions
  { matchPatternFile :: FilePath
  , matchDataFile :: FilePath
  , matchFormat :: OutputFormat
  , matchOutputOptions :: OutputOptions
  } deriving (Show)

matchOptions :: Parser MatchOptions
matchOptions = MatchOptions
  <$> strArgument (metavar "PATTERN-FILE" <> help "Pattern file in gram notation")
  <*> strArgument (metavar "DATA-FILE" <> help "Data file in gram notation")
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

runMatch :: MatchOptions -> IO ExitCode
runMatch opts = do
  patternStr <- readFile (matchPatternFile opts)
  dataStr <- readFile (matchDataFile opts)
  
  let outputOpts = enforceDeterministicCanonical (matchOutputOptions opts)
  
  case (Gram.fromGram patternStr, Gram.fromGram dataStr) of
    (Left err, _) -> do
      Output.formatError (matchFormat opts) outputOpts ("Pattern parse error: " ++ show err)
      return (ExitFailure 1)
    (_, Left err) -> do
      Output.formatError (matchFormat opts) outputOpts ("Data parse error: " ++ show err)
      return (ExitFailure 1)
    (Right patterns, Right dataPatterns) -> do
      -- For now, use simple structural matching of lists
      if patterns == dataPatterns
        then do
          Output.formatOutput (matchFormat opts) outputOpts dataPatterns
          return ExitSuccess
        else do
          Output.formatError (matchFormat opts) outputOpts "Pattern(s) do not match data"
          return (ExitFailure 2)

