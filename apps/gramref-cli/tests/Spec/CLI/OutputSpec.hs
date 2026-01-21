-- | Unit tests for CLI output formatting (User Story 1).
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Spec.CLI.OutputSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified Gramref.CLI.JSON as JSON
import qualified Gramref.CLI.Types as Types
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Gram.Parse as Gram

spec :: Spec
spec = do
  describe "Output Formatting (User Story 1)" $ do
    
    describe "--value-only flag" $ do
      
      it "produces result value only (no metadata) for successful parse" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.valueOnly = True }
            let output = JSON.patternsToJSON opts [pattern]
            -- Should not contain "Meta" or "Result" wrapper
            T.isInfixOf (T.pack "Meta") output `shouldBe` False
            T.isInfixOf (T.pack "Result") output `shouldBe` False
            -- Should contain pattern structure
            T.isInfixOf (T.pack "subject") output `shouldBe` True
      
      it "produces error object only (no metadata) for error output" $ do
        let opts = Types.defaultOutputOptions { Types.valueOnly = True }
        let output = JSON.errorToJSON opts "Test error message"
        -- Should not contain "Error" wrapper
        T.isInfixOf (T.pack "\"Error\"") output `shouldBe` False
        -- Should contain error type and message
        T.isInfixOf (T.pack "ParseError") output `shouldBe` True
        T.isInfixOf (T.pack "Test error message") output `shouldBe` True
      
      it "produces identical output across multiple runs with --value-only" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.valueOnly = True }
            output1 <- return $ JSON.patternsToJSON opts [pattern]
            output2 <- return $ JSON.patternsToJSON opts [pattern]
            output1 `shouldBe` output2
    
    describe "--deterministic flag" $ do
      
      it "uses fixed timestamp when deterministic is enabled" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.deterministic = True }
            let output = JSON.patternsToJSON opts [pattern]
            -- Should contain fixed timestamp
            T.isInfixOf (T.pack "1970-01-01T00:00:00+0000") output `shouldBe` True
      
      it "uses fixed hash when deterministic is enabled" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.deterministic = True }
            let output = JSON.patternsToJSON opts [pattern]
            -- Should contain fixed hash (all zeros)
            T.isInfixOf (T.pack "0000000000000000000000000000000000000000000000000000000000000000") output `shouldBe` True
      
      it "produces identical output across multiple runs with --deterministic" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.deterministic = True }
            output1 <- return $ JSON.patternsToJSON opts [pattern]
            output2 <- return $ JSON.patternsToJSON opts [pattern]
            output1 `shouldBe` output2
    
    describe "--value-only --deterministic combination" $ do
      
      it "produces value-only output with deterministic behavior" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions 
                  { Types.valueOnly = True
                  , Types.deterministic = True
                  }
            let output = JSON.patternsToJSON opts [pattern]
            -- Should not contain metadata
            T.isInfixOf (T.pack "Meta") output `shouldBe` False
            -- Should contain pattern structure
            T.isInfixOf (T.pack "subject") output `shouldBe` True
            -- Should be deterministic
            output2 <- return $ JSON.patternsToJSON opts [pattern]
            output `shouldBe` output2
    
    describe "Integration test for parse command with --value-only" $ do
      
      it "parse command produces value-only output when flag is set" $ do
        let input = "(test)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.valueOnly = True }
            let output = JSON.patternsToJSON opts [pattern]
            -- Verify it's valid JSON and contains pattern data
            T.isInfixOf (T.pack "subject") output `shouldBe` True
            T.isInfixOf (T.pack "elements") output `shouldBe` True
    
    describe "Edge cases" $ do
      
      it "--value-only with empty pattern produces valid output" $ do
        let input = "()"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.valueOnly = True }
            let output = JSON.patternsToJSON opts [pattern]
            -- Should produce valid JSON even for empty pattern
            T.isInfixOf (T.pack "subject") output `shouldBe` True
            T.isInfixOf (T.pack "elements") output `shouldBe` True
      
      it "all flags combined (--value-only --deterministic --canonical) work together" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions 
                  { Types.valueOnly = True
                  , Types.deterministic = True
                  , Types.canonical = True
                  }
            let opts' = Types.enforceDeterministicCanonical opts
            let output = JSON.patternsToJSON opts' [pattern]
            -- Should not contain metadata
            T.isInfixOf (T.pack "Meta") output `shouldBe` False
            -- Should contain pattern structure
            T.isInfixOf (T.pack "subject") output `shouldBe` True
            -- Should be deterministic
            output2 <- return $ JSON.patternsToJSON opts' [pattern]
            output `shouldBe` output2

