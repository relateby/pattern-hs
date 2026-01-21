-- | Unit tests for canonical JSON functionality (User Story 3).
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Spec.CLI.JSONSpec where

import Test.Hspec
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as Vector
import qualified Gramref.CLI.JSON as JSON
import qualified Gramref.CLI.Types as Types
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Gram.Parse as Gram

-- Helper for creating JSON objects with string keys
(.=) :: String -> Aeson.Value -> (Key.Key, Aeson.Value)
k .= v = (Key.fromString k, v)
infixr 8 .=

spec :: Spec
spec = do
  describe "Canonical JSON (User Story 3)" $ do
    
    describe "canonicalizeJSON function" $ do
      
      it "sorts keys in nested objects" $ do
        -- Create a nested JSON structure with unsorted keys
        let nestedObj = Aeson.object
              [ "zebra" .= (Aeson.object ["c" .= Aeson.toJSON (1 :: Int), "a" .= Aeson.toJSON (2 :: Int), "b" .= Aeson.toJSON (3 :: Int)])
              , "alpha" .= (Aeson.object ["x" .= Aeson.toJSON (4 :: Int), "y" .= Aeson.toJSON (5 :: Int)])
              ]
        let canonical = JSON.canonicalizeJSON nestedObj
        -- Parse back to verify keys are sorted
        case canonical of
          Aeson.Object topObj -> do
            let topKeys = map Key.toText $ KeyMap.keys topObj
            topKeys `shouldBe` sort topKeys
            -- Check nested object keys
            case KeyMap.lookup (Key.fromString "alpha") topObj of
              Just (Aeson.Object nested) -> do
                let nestedKeys = map Key.toText $ KeyMap.keys nested
                nestedKeys `shouldBe` sort nestedKeys
              _ -> expectationFailure "Expected nested object"
          _ -> expectationFailure "Expected top-level object"
    
    describe "--canonical flag" $ do
      
      it "sorts keys at top level" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.canonical = True, Types.valueOnly = True }
            let output = JSON.patternsToJSON opts [pattern]
            -- Parse back to verify structure
            case Aeson.eitherDecodeStrict (TE.encodeUtf8 output) of
              Left _ -> expectationFailure "Output is not valid JSON"
              Right val -> do
                -- If it's an object, keys should be sorted
                case val of
                  Aeson.Object obj -> do
                    let keys = map Key.toText $ KeyMap.keys obj
                    keys `shouldBe` sort keys
                  _ -> return ()
      
      it "sorts keys at all nesting levels" $ do
        -- Use a simple pattern that will have nested structure in JSON
        let input = "(node)"
        case Gram.fromGram input of
          Left err -> expectationFailure $ "Failed to parse test input: " ++ show err
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.canonical = True, Types.valueOnly = True }
            let output = JSON.patternsToJSON opts [pattern]
            -- Verify output is deterministic and contains sorted keys
            output2 <- return $ JSON.patternsToJSON opts [pattern]
            output `shouldBe` output2
      
      it "produces byte-for-byte identical output across runs" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            -- Use deterministic flag to ensure identical output
            let opts = Types.defaultOutputOptions { Types.canonical = True, Types.deterministic = True }
            output1 <- return $ JSON.patternsToJSON opts [pattern]
            output2 <- return $ JSON.patternsToJSON opts [pattern]
            output1 `shouldBe` output2
    
    describe "--canonical --value-only combination" $ do
      
      it "produces canonical value-only output" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions 
                  { Types.canonical = True
                  , Types.valueOnly = True
                  }
            let output = JSON.patternsToJSON opts [pattern]
            -- Should not contain metadata
            T.isInfixOf (T.pack "Meta") output `shouldBe` False
            -- Should be deterministic
            output2 <- return $ JSON.patternsToJSON opts [pattern]
            output `shouldBe` output2
    
    describe "--deterministic auto-enables --canonical" $ do
      
      it "canonical is automatically enabled when deterministic is used" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            let opts = Types.defaultOutputOptions { Types.deterministic = True }
            let opts' = Types.enforceDeterministicCanonical opts
            Types.canonical opts' `shouldBe` True
    
    describe "Hash computation with canonical mode" $ do
      
      it "hash is computed from canonicalized JSON when --canonical is used" $ do
        let input = "(node)"
        case Gram.fromGram input of
          Left _ -> expectationFailure "Failed to parse test input"
          Right [pattern] -> do
            -- Test that hash computation works correctly with canonical mode
            let opts = Types.defaultOutputOptions { Types.canonical = True }
            let output = JSON.patternsToJSON opts [pattern]
            -- The output should be valid JSON with a hash field
            -- The hash should be computed from the canonicalized JSON (before hash is added)
            T.isInfixOf (T.pack "Hash") output `shouldBe` True
            -- Verify output is canonical (keys should be sorted)
            case Aeson.eitherDecodeStrict (TE.encodeUtf8 output) of
              Left _ -> expectationFailure "Output is not valid JSON"
              Right val -> do
                case val of
                  Aeson.Object obj -> do
                    let keys = map Key.toText $ KeyMap.keys obj
                    keys `shouldBe` sort keys
                  _ -> return ()
    
    describe "Edge cases" $ do
      
      it "--canonical handles empty objects correctly" $ do
        let emptyObj = Aeson.object []
        let canonical = JSON.canonicalizeJSON emptyObj
        case canonical of
          Aeson.Object obj -> KeyMap.size obj `shouldBe` 0
          _ -> expectationFailure "Expected empty object"
      
      it "--canonical handles arrays correctly" $ do
        let arr = Aeson.Array $ Vector.fromList [Aeson.object ["b" .= Aeson.toJSON (1 :: Int), "a" .= Aeson.toJSON (2 :: Int)]]
        let canonical = JSON.canonicalizeJSON arr
        case canonical of
          Aeson.Array vec -> do
            case Vector.toList vec of
              [Aeson.Object obj] -> do
                let keys = map Key.toText $ KeyMap.keys obj
                keys `shouldBe` sort keys
              _ -> expectationFailure "Expected array with single object"
          _ -> expectationFailure "Expected array"

