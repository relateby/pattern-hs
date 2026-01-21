-- | Property-based tests for deterministic output (User Story 1).
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Spec.Properties.DeterministicSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified Gramref.CLI.JSON as JSON
import qualified Gramref.CLI.Types as Types
import qualified Gram.Parse as Gram

spec :: Spec
spec = do
  describe "Deterministic Output Properties (User Story 1)" $ do
    
    it "same input produces same output with --deterministic" $ do
      -- Generate a simple pattern string
      let input = "(node)"
      case Gram.fromGram input of
        Left _ -> expectationFailure "Failed to parse test input"
        Right [pattern] -> do
          let opts = Types.defaultOutputOptions { Types.deterministic = True }
          let output1 = JSON.patternsToJSON opts [pattern]
          let output2 = JSON.patternsToJSON opts [pattern]
          output1 `shouldBe` output2
    
    it "same input produces same output with --value-only" $ do
      let input = "(node)"
      case Gram.fromGram input of
        Left _ -> expectationFailure "Failed to parse test input"
        Right [pattern] -> do
          let opts = Types.defaultOutputOptions { Types.valueOnly = True }
          let output1 = JSON.patternsToJSON opts [pattern]
          let output2 = JSON.patternsToJSON opts [pattern]
          output1 `shouldBe` output2
    
    it "deterministic output contains fixed timestamp" $ do
      let input = "(node)"
      case Gram.fromGram input of
        Left _ -> expectationFailure "Failed to parse test input"
        Right [pattern] -> do
          let opts = Types.defaultOutputOptions { Types.deterministic = True }
          let output = JSON.patternsToJSON opts [pattern]
          T.isInfixOf (T.pack "1970-01-01T00:00:00+0000") output `shouldBe` True
    
    it "deterministic output contains fixed hash" $ do
      let input = "(node)"
      case Gram.fromGram input of
        Left _ -> expectationFailure "Failed to parse test input"
        Right [pattern] -> do
          let opts = Types.defaultOutputOptions { Types.deterministic = True }
          let output = JSON.patternsToJSON opts [pattern]
          T.isInfixOf (T.pack "0000000000000000000000000000000000000000000000000000000000000000") output `shouldBe` True

