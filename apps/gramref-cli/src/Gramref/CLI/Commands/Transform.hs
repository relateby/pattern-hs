{-# LANGUAGE OverloadedStrings #-}
module Gramref.CLI.Commands.Transform
  ( TransformOptions(..)
  , transformOptions
  , runTransform
  ) where

import Options.Applicative
import Gramref.CLI.Types (OutputFormat(..), OutputOptions(..), outputOptionsParser, enforceDeterministicCanonical)
import qualified Gramref.CLI.Output as Output
import qualified Gram.Parse as Gram
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import System.Exit (ExitCode(..))
import Data.Foldable (toList)

data TransformOperation
  = OpFold
  | OpMap
  | OpFilter
  | OpReverse
  | OpFlatten
  | OpNormalize
  deriving (Show, Eq)

data TransformOptions = TransformOptions
  { transformOperation :: TransformOperation
  , transformInputFile :: FilePath
  , transformFormat :: OutputFormat
  , transformOutputOptions :: OutputOptions
  } deriving (Show)

transformOptions :: Parser TransformOptions
transformOptions = TransformOptions
  <$> operationOption
  <*> strArgument (metavar "INPUT-FILE" <> help "Input file in gram notation")
  <*> formatOption
  <*> outputOptionsParser

operationOption :: Parser TransformOperation
operationOption = option (maybeReader parseOperation)
  ( long "operation"
  <> short 'o'
  <> metavar "OPERATION"
  <> help "Operation: fold, map, filter, reverse, flatten, or normalize"
  )

parseOperation :: String -> Maybe TransformOperation
parseOperation "fold" = Just OpFold
parseOperation "map" = Just OpMap
parseOperation "filter" = Just OpFilter
parseOperation "reverse" = Just OpReverse
parseOperation "flatten" = Just OpFlatten
parseOperation "normalize" = Just OpNormalize
parseOperation _ = Nothing

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

runTransform :: TransformOptions -> IO ExitCode
runTransform opts = do
  input <- readFile (transformInputFile opts)
  
  let outputOpts = enforceDeterministicCanonical (transformOutputOptions opts)
  
  case Gram.fromGram input of
    Left err -> do
      Output.formatError (transformFormat opts) outputOpts (show err)
      return (ExitFailure 1)
    Right patterns -> do
      let transformed = map (applyTransform (transformOperation opts)) patterns
      Output.formatOutput (transformFormat opts) outputOpts transformed
      return ExitSuccess

applyTransform :: TransformOperation -> Pattern.Pattern Subject.Subject -> Pattern.Pattern Subject.Subject
applyTransform OpReverse (Pattern.Pattern v es) = Pattern.Pattern v (reverse es)
applyTransform OpFlatten (Pattern.Pattern v es) = 
  let flattened = Pattern.flatten (Pattern.Pattern v es)
      rootValue = v  -- Use original root value
  in Pattern.fromList rootValue (toList flattened)
applyTransform OpNormalize pat = pat  -- TODO: Implement normalization
applyTransform OpFold pat = pat  -- TODO: Implement fold with function
applyTransform OpMap pat = pat  -- TODO: Implement map with function
applyTransform OpFilter pat = pat  -- TODO: Implement filter with predicate

