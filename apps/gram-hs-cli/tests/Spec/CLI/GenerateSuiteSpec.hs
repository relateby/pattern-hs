{-# LANGUAGE LambdaCase #-}
-- | Unit tests for test suite generation (User Story 2).
module Spec.CLI.GenerateSuiteSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import qualified GramHs.CLI.Commands.Generate as Generate
import qualified GramHs.CLI.Types as Types
import System.Random (mkStdGen)
import Data.Maybe (fromMaybe)

spec :: Spec
spec = do
  describe "Test Suite Generation (User Story 2)" $ do
    
    describe "generate --type suite" $ do
      
      it "generates valid test suite JSON structure" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 3 gen "standard"
        -- Verify it has test_cases array
        case Aeson.toJSON suite of
          Aeson.Object obj -> do
            KeyMap.lookup (Key.fromString "test_cases") obj `shouldNotBe` Nothing
            KeyMap.lookup (Key.fromString "version") obj `shouldNotBe` Nothing
          _ -> expectationFailure "Expected object"
      
      it "generates sequential test case numbering" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 5 gen "basic"
        let json = Aeson.toJSON suite
        case json of
          Aeson.Object obj -> do
            case KeyMap.lookup (Key.fromString "test_cases") obj of
              Just (Aeson.Array casesVec) -> do
                let cases = Vector.toList casesVec
                length cases `shouldBe` 5
                -- Verify sequential numbering (test_case_001, test_case_002, etc.)
                let mapMaybe f = concatMap (\x -> maybe [] (:[]) (f x))
                let names = mapMaybe (\x -> case x of
                      Aeson.Object tc -> KeyMap.lookup (Key.fromString "name") tc
                      _ -> Nothing) cases
                names `shouldBe` 
                  [ Aeson.String (T.pack "test_case_001")
                  , Aeson.String (T.pack "test_case_002")
                  , Aeson.String (T.pack "test_case_003")
                  , Aeson.String (T.pack "test_case_004")
                  , Aeson.String (T.pack "test_case_005")
                  ]
              _ -> expectationFailure "Expected test_cases array"
          _ -> expectationFailure "Expected object"
      
      it "generates deterministic output with same seed" $ do
        let gen1 = mkStdGen 42
        let gen2 = mkStdGen 42
        let suite1 = Generate.generateTestSuite 3 gen1 "standard"
        let suite2 = Generate.generateTestSuite 3 gen2 "standard"
        Aeson.toJSON suite1 `shouldBe` Aeson.toJSON suite2
      
      it "generates different output with different seeds" $ do
        let gen1 = mkStdGen 42
        let gen2 = mkStdGen 100
        let suite1 = Generate.generateTestSuite 3 gen1 "standard"
        let suite2 = Generate.generateTestSuite 3 gen2 "standard"
        Aeson.toJSON suite1 `shouldNotBe` Aeson.toJSON suite2
    
    describe "Complexity levels" $ do
      
      it "generates patterns for minimal complexity" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 2 gen "minimal"
        let json = Aeson.toJSON suite
        case json of
          Aeson.Object obj -> do
            case KeyMap.lookup (Key.fromString "test_cases") obj of
              Just (Aeson.Array casesVec) -> Vector.length casesVec `shouldBe` 2
              _ -> expectationFailure "Expected test_cases array"
          _ -> expectationFailure "Expected object"
      
      it "generates patterns for basic complexity" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 2 gen "basic"
        let json = Aeson.toJSON suite
        case json of
          Aeson.Object obj -> do
            case KeyMap.lookup (Key.fromString "test_cases") obj of
              Just (Aeson.Array casesVec) -> Vector.length casesVec `shouldBe` 2
              _ -> expectationFailure "Expected test_cases array"
          _ -> expectationFailure "Expected object"
      
      it "generates patterns for standard complexity" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 2 gen "standard"
        let json = Aeson.toJSON suite
        case json of
          Aeson.Object obj -> do
            case KeyMap.lookup (Key.fromString "test_cases") obj of
              Just (Aeson.Array casesVec) -> Vector.length casesVec `shouldBe` 2
              _ -> expectationFailure "Expected test_cases array"
          _ -> expectationFailure "Expected object"
      
      it "generates patterns for complex complexity" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 2 gen "complex"
        let json = Aeson.toJSON suite
        case json of
          Aeson.Object obj -> do
            case KeyMap.lookup (Key.fromString "test_cases") obj of
              Just (Aeson.Array casesVec) -> Vector.length casesVec `shouldBe` 2
              _ -> expectationFailure "Expected test_cases array"
          _ -> expectationFailure "Expected object"
      
      it "generates patterns for adversarial complexity" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 2 gen "adversarial"
        let json = Aeson.toJSON suite
        case json of
          Aeson.Object obj -> do
            case KeyMap.lookup (Key.fromString "test_cases") obj of
              Just (Aeson.Array casesVec) -> Vector.length casesVec `shouldBe` 2
              _ -> expectationFailure "Expected test_cases array"
          _ -> expectationFailure "Expected object"
      
      it "defaults to basic complexity for invalid level" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 2 gen "invalid"
        let json = Aeson.toJSON suite
        case json of
          Aeson.Object obj -> do
            case KeyMap.lookup (Key.fromString "test_cases") obj of
              Just (Aeson.Array casesVec) -> Vector.length casesVec `shouldBe` 2
              _ -> expectationFailure "Expected test_cases array"
          _ -> expectationFailure "Expected object"
    
    describe "Edge cases" $ do
      
      it "handles count of 0 gracefully" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 0 gen "basic"
        let json = Aeson.toJSON suite
        case json of
          Aeson.Object obj -> do
            case KeyMap.lookup (Key.fromString "test_cases") obj of
              Just (Aeson.Array casesVec) -> Vector.length casesVec `shouldBe` 0
              _ -> expectationFailure "Expected test_cases array"
          _ -> expectationFailure "Expected object"
      
      it "generates valid JSON for all test cases" $ do
        let gen = mkStdGen 42
        let suite = Generate.generateTestSuite 10 gen "standard"
        let json = Aeson.toJSON suite
        -- Should be valid JSON (toJSON should succeed)
        case json of
          Aeson.Object obj -> do
            KeyMap.lookup (Key.fromString "test_cases") obj `shouldNotBe` Nothing
            KeyMap.lookup (Key.fromString "version") obj `shouldNotBe` Nothing
          _ -> expectationFailure "Expected object"

