{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Spec.Gram.SchemaSpec (spec) where

import Test.Hspec
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Gram.Schema.JSONSchema as Schema
import qualified Gram.Schema.TypeScript as TypeScript
import qualified Gram.Schema.Rust as Rust
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "JSON Schema Generation" $ do
    
    it "generates valid JSON" $ do
      let schema = Schema.generatePatternSchema
      schema `shouldSatisfy` (\v -> case v of { Object _ -> True; _ -> False })
    
    it "includes $schema field with Draft 2020-12" $ do
      let schema = Schema.generatePatternSchema
      case schema of
        Object obj -> do
          let schemaField = KeyMap.lookup "$schema" obj
          schemaField `shouldBe` Just (String "https://json-schema.org/draft/2020-12/schema")
        _ -> expectationFailure "Schema is not an object"
    
    it "includes $id field" $ do
      let schema = Schema.generatePatternSchema
      case schema of
        Object obj -> do
          let idField = KeyMap.lookup "$id" obj
          idField `shouldSatisfy` (\case { Just (String _) -> True; _ -> False })
        _ -> expectationFailure "Schema is not an object"
    
    it "includes version field" $ do
      let schema = Schema.generatePatternSchema
      case schema of
        Object obj -> do
          let versionField = KeyMap.lookup "version" obj
          versionField `shouldBe` Just (String "0.1.0")
        _ -> expectationFailure "Schema is not an object"
    
    it "includes $defs with Pattern definition" $ do
      let schema = Schema.generatePatternSchema
      case schema of
        Object obj -> do
          let defs = KeyMap.lookup "$defs" obj
          case defs of
            Just (Object defsObj) -> do
              let patternDef = KeyMap.lookup "Pattern" defsObj
              patternDef `shouldSatisfy` (\case { Just (Object _) -> True; _ -> False })
            _ -> expectationFailure "$defs is not an object"
        _ -> expectationFailure "Schema is not an object"
    
    it "includes $defs with Subject definition" $ do
      let schema = Schema.generatePatternSchema
      case schema of
        Object obj -> do
          let defs = KeyMap.lookup "$defs" obj
          case defs of
            Just (Object defsObj) -> do
              let subjectDef = KeyMap.lookup "Subject" defsObj
              subjectDef `shouldSatisfy` (\case { Just (Object _) -> True; _ -> False })
            _ -> expectationFailure "$defs is not an object"
        _ -> expectationFailure "Schema is not an object"
    
    it "includes $defs with Value definition" $ do
      let schema = Schema.generatePatternSchema
      case schema of
        Object obj -> do
          let defs = KeyMap.lookup "$defs" obj
          case defs of
            Just (Object defsObj) -> do
              let valueDef = KeyMap.lookup "Value" defsObj
              valueDef `shouldSatisfy` (\case { Just (Object _) -> True; _ -> False })
            _ -> expectationFailure "$defs is not an object"
        _ -> expectationFailure "Schema is not an object"
    
    it "includes definitions for all complex value types" $ do
      let schema = Schema.generatePatternSchema
      case schema of
        Object obj -> do
          let defs = KeyMap.lookup "$defs" obj
          case defs of
            Just (Object defsObj) -> do
              -- Check for Symbol, TaggedString, Range, Measurement
              KeyMap.member "Symbol" defsObj `shouldBe` True
              KeyMap.member "TaggedString" defsObj `shouldBe` True
              KeyMap.member "Range" defsObj `shouldBe` True
              KeyMap.member "Measurement" defsObj `shouldBe` True
            _ -> expectationFailure "$defs is not an object"
        _ -> expectationFailure "Schema is not an object"

  describe "TypeScript Type Generation" $ do
    
    it "generates valid TypeScript syntax (basic check)" $ do
      let tsCode = TypeScript.generateTypeScriptTypes
      tsCode `shouldSatisfy` T.isInfixOf "export interface Pattern"
      tsCode `shouldSatisfy` T.isInfixOf "export interface Subject"
      tsCode `shouldSatisfy` T.isInfixOf "export type Value"
    
    it "includes all expected interfaces" $ do
      let tsCode = TypeScript.generateTypeScriptTypes
      tsCode `shouldSatisfy` T.isInfixOf "interface Pattern"
      tsCode `shouldSatisfy` T.isInfixOf "interface Subject"
      tsCode `shouldSatisfy` T.isInfixOf "interface ValueSymbol"
      tsCode `shouldSatisfy` T.isInfixOf "interface ValueTaggedString"
      tsCode `shouldSatisfy` T.isInfixOf "interface ValueRange"
      tsCode `shouldSatisfy` T.isInfixOf "interface ValueMeasurement"
    
    it "includes type guards" $ do
      let tsCode = TypeScript.generateTypeScriptTypes
      tsCode `shouldSatisfy` T.isInfixOf "isValueSymbol"
      tsCode `shouldSatisfy` T.isInfixOf "isValueRange"
    
    it "includes JSDoc comments" $ do
      let tsCode = TypeScript.generateTypeScriptTypes
      tsCode `shouldSatisfy` T.isInfixOf "/**"
  
  describe "Rust Type Generation" $ do
    
    it "generates valid Rust syntax (basic check)" $ do
      let rustCode = Rust.generateRustTypes
      rustCode `shouldSatisfy` T.isInfixOf "pub struct Pattern"
      rustCode `shouldSatisfy` T.isInfixOf "pub struct Subject"
      rustCode `shouldSatisfy` T.isInfixOf "pub enum Value"
    
    it "includes all expected structs and enums" $ do
      let rustCode = Rust.generateRustTypes
      rustCode `shouldSatisfy` T.isInfixOf "struct Pattern"
      rustCode `shouldSatisfy` T.isInfixOf "struct Subject"
      rustCode `shouldSatisfy` T.isInfixOf "struct ValueSymbol"
      rustCode `shouldSatisfy` T.isInfixOf "enum Value"
    
    it "includes serde derives" $ do
      let rustCode = Rust.generateRustTypes
      rustCode `shouldSatisfy` T.isInfixOf "Serialize, Deserialize"
      rustCode `shouldSatisfy` T.isInfixOf "#[serde(untagged)]"
    
    it "includes convenience constructors" $ do
      let rustCode = Rust.generateRustTypes
      rustCode `shouldSatisfy` T.isInfixOf "impl Pattern"
      rustCode `shouldSatisfy` T.isInfixOf "pub fn new"
    
    it "includes doc comments" $ do
      let rustCode = Rust.generateRustTypes
      rustCode `shouldSatisfy` T.isInfixOf "///"
