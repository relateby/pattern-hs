{-# LANGUAGE OverloadedStrings #-}
module Gramref.CLI.Commands.Convert
  ( ConvertOptions(..)
  , ConvertFormat(..)
  , convertOptions
  , runConvert
  ) where

import Options.Applicative
import Gramref.CLI.Types (OutputFormat(..), OutputOptions(..), outputOptionsParser, enforceDeterministicCanonical)
import qualified Gramref.CLI.Output as Output
import qualified Gram.Parse as Gram
import qualified Gram.Serialize as Gram
import qualified Gram.JSON ()  -- ToJSON/FromJSON for Pattern
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import System.Exit (ExitCode(..))

data ConvertFormat
  = ConvertGram
  | ConvertJSON
  | ConvertCypher
  | ConvertDot
  | ConvertMermaid
  deriving (Show, Eq)

data ConvertOptions = ConvertOptions
  { convertInputFile :: FilePath
  , convertFrom :: ConvertFormat
  , convertTo :: ConvertFormat
  , convertOutputOptions :: OutputOptions
  } deriving (Show)

convertOptions :: Parser ConvertOptions
convertOptions = ConvertOptions
  <$> strArgument (metavar "INPUT-FILE" <> help "Input file")
  <*> fromOption
  <*> toOption
  <*> outputOptionsParser

fromOption :: Parser ConvertFormat
fromOption = option (maybeReader parseFormat)
  ( long "from"
  <> metavar "FORMAT"
  <> value ConvertGram
  <> help "Input format: gram, json, cypher, dot, or mermaid (default: gram)"
  )

toOption :: Parser ConvertFormat
toOption = option (maybeReader parseFormat)
  ( long "to"
  <> metavar "FORMAT"
  <> value ConvertJSON
  <> help "Output format: gram, json, cypher, dot, or mermaid (default: json)"
  )

parseFormat :: String -> Maybe ConvertFormat
parseFormat "gram" = Just ConvertGram
parseFormat "json" = Just ConvertJSON
parseFormat "cypher" = Just ConvertCypher
parseFormat "dot" = Just ConvertDot
parseFormat "mermaid" = Just ConvertMermaid
parseFormat _ = Nothing

runConvert :: ConvertOptions -> IO ExitCode
runConvert opts = do
  let outputOpts = enforceDeterministicCanonical (convertOutputOptions opts)
  
  case convertFrom opts of
    ConvertGram -> do
      input <- readFile (convertInputFile opts)
      case Gram.fromGram input of
        Left err -> do
          Output.formatError FormatJSON outputOpts (show err)
          return (ExitFailure 1)
        Right patterns -> case convertTo opts of
          ConvertGram -> do
            putStrLn (Gram.toGram patterns)
            return ExitSuccess
          ConvertJSON -> do
            Output.formatOutput FormatJSON outputOpts patterns
            return ExitSuccess
          _ -> do
            Output.formatError FormatJSON outputOpts ("Conversion to " ++ show (convertTo opts) ++ " not yet implemented")
            return (ExitFailure 3)
    
    ConvertJSON -> do
      jsonInput <- BSL.readFile (convertInputFile opts)
      -- Support both JSON array of patterns and single pattern object (backward compat)
      let patternsE = case (eitherDecode jsonInput :: Either String [Pattern.Pattern Subject.Subject]) of
            Right ps -> Right ps
            Left _ -> case (eitherDecode jsonInput :: Either String (Pattern.Pattern Subject.Subject)) of
              Right p -> Right [p]
              Left err -> Left err
      case patternsE of
        Left err -> do
          Output.formatError FormatJSON outputOpts ("JSON parse error: " ++ err)
          return (ExitFailure 1)
        Right patterns -> case convertTo opts of
          ConvertGram -> do
            putStrLn (Gram.toGram patterns)
            return ExitSuccess
          ConvertJSON -> do
            Output.formatOutput FormatJSON outputOpts patterns
            return ExitSuccess
          _ -> do
            Output.formatError FormatJSON outputOpts ("Conversion to " ++ show (convertTo opts) ++ " not yet implemented")
            return (ExitFailure 3)
    
    _ -> do
      Output.formatError FormatJSON outputOpts ("Conversion from " ++ show (convertFrom opts) ++ " not yet implemented")
      return (ExitFailure 3)

