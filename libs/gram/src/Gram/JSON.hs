{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- | JSON serialization and deserialization for Pattern Subject.
--
-- This module provides canonical JSON representation of Pattern<Subject>
-- with bidirectional conversion support. The format is designed for
-- interoperability and can serve as an exchange format between different
-- gram implementations.
--
-- == JSON Format
--
-- Patterns are represented as:
--
-- > { "subject": Subject, "elements": [Pattern] }
--
-- Subjects are represented as:
--
-- > { "identity": String, "labels": [String], "properties": {...} }
--
-- Value types use native JSON for simple types and discriminated objects
-- for complex types (Symbol, TaggedString, Range, Measurement).
module Gram.JSON
  ( -- * Serialization
    patternToValue
  , subjectToValue
  , valueToJSON
    -- * Deserialization  
  , patternFromValue
  , subjectFromValue
  , valueFromJSON
    -- * Utilities
  , canonicalizeJSON
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Key (fromString, toText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Subject.Value as SubjectValue
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Foldable (toList)
import Control.Applicative ((<|>))

-- | Convert a Pattern to aeson Value (for serialization)
patternToValue :: Pattern.Pattern Subject.Subject -> Value
patternToValue (Pattern.Pattern v es) = object
  [ "subject" .= subjectToValue v
  , "elements" .= map patternToValue es
  ]

-- | Convert a Subject to aeson Value (for serialization)
subjectToValue :: Subject.Subject -> Value
subjectToValue (Subject.Subject ident labels props) = object
  [ "identity" .= symbolToValue ident
  , "labels" .= toJSON (Set.toList labels)
  , "properties" .= propsToValue props
  ]

-- | Convert a Symbol to aeson Value
symbolToValue :: Subject.Symbol -> Value
symbolToValue (Subject.Symbol s) = toJSON s

-- | Convert properties map to aeson Value
propsToValue :: Subject.PropertyRecord -> Value
propsToValue props = object $ map (\(k, v) -> fromString k .= valueToJSON v) (Map.toList props)

-- | Convert a Subject.Value to aeson Value (for serialization)
valueToJSON :: SubjectValue.Value -> Value
valueToJSON (SubjectValue.VInteger i) = toJSON i
valueToJSON (SubjectValue.VDecimal d) = toJSON d
valueToJSON (SubjectValue.VBoolean b) = toJSON b
valueToJSON (SubjectValue.VString s) = toJSON s
valueToJSON (SubjectValue.VSymbol s) = object ["type" .= ("symbol" :: T.Text), "value" .= s]
valueToJSON (SubjectValue.VTaggedString tag content) = object ["type" .= ("tagged" :: T.Text), "tag" .= tag, "content" .= content]
valueToJSON (SubjectValue.VArray vs) = toJSON (map valueToJSON vs)
valueToJSON (SubjectValue.VMap m) = toJSON (Map.map valueToJSON m)
valueToJSON (SubjectValue.VRange rv) = rangeValueToJSON rv
valueToJSON (SubjectValue.VMeasurement unit val) = object ["type" .= ("measurement" :: T.Text), "unit" .= unit, "value" .= val]

-- | Convert a RangeValue to aeson Value
rangeValueToJSON :: SubjectValue.RangeValue -> Value
rangeValueToJSON (SubjectValue.RangeValue lower upper) = object
  [ "type" .= ("range" :: T.Text)
  , "lower" .= toJSON lower
  , "upper" .= toJSON upper
  ]

-- | Parse a Pattern from aeson Value (for deserialization)
patternFromValue :: Value -> Parser (Pattern.Pattern Subject.Subject)
patternFromValue = withObject "Pattern" $ \obj -> do
  subj <- obj .: "subject"
  elems <- obj .: "elements"
  v <- subjectFromValue subj
  es <- mapM patternFromValue elems
  return $ Pattern.Pattern v es

-- | Parse a Subject from aeson Value (for deserialization)
subjectFromValue :: Value -> Parser Subject.Subject
subjectFromValue = withObject "Subject" $ \obj -> do
  identStr <- obj .: "identity"
  labels <- obj .: "labels"
  props <- obj .: "properties"
  let ident = Subject.Symbol identStr
  let labelSet = Set.fromList labels
  propsMap <- parseProperties props
  return $ Subject.Subject ident labelSet propsMap

-- | Parse properties map from aeson Value
parseProperties :: Value -> Parser Subject.PropertyRecord
parseProperties = withObject "Properties" $ \obj -> do
  let pairs = KeyMap.toList obj
  Map.fromList <$> mapM parseProp pairs
  where
    parseProp (k, v) = do
      val <- valueFromJSON v
      return (T.unpack $ toText k, val)

-- | Parse a Subject.Value from aeson Value (for deserialization)
valueFromJSON :: Value -> Parser SubjectValue.Value
valueFromJSON v = 
  parseInteger v <|>
  parseDecimal v <|>
  parseBoolean v <|>
  parseString v <|>
  parseSymbol v <|>
  parseTaggedString v <|>
  parseRange v <|>
  parseMeasurement v <|>
  parseArray v <|>
  parseMap v
  where
    parseInteger = withScientific "Integer" $ \n ->
      case floatingOrInteger n of
        Left _ -> fail "Expected integer"
        Right i -> return $ SubjectValue.VInteger i
    
    parseDecimal = withScientific "Decimal" $ \n ->
      return $ SubjectValue.VDecimal (realToFrac n)
    
    parseBoolean = withBool "Boolean" $ \b ->
      return $ SubjectValue.VBoolean b
    
    parseString = withText "String" $ \t ->
      return $ SubjectValue.VString (T.unpack t)
    
    parseSymbol = withObject "Symbol" $ \obj -> do
      ty <- obj .: "type"
      if ty == ("symbol" :: T.Text)
        then do
          val <- obj .: "value"
          return $ SubjectValue.VSymbol val
        else fail "Not a symbol"
    
    parseTaggedString = withObject "TaggedString" $ \obj -> do
      ty <- obj .: "type"
      if ty == ("tagged" :: T.Text)
        then do
          tag <- obj .: "tag"
          content <- obj .: "content"
          return $ SubjectValue.VTaggedString tag content
        else fail "Not a tagged string"
    
    parseRange = withObject "Range" $ \obj -> do
      ty <- obj .: "type"
      if ty == ("range" :: T.Text)
        then do
          lower <- obj .: "lower"
          upper <- obj .: "upper"
          return $ SubjectValue.VRange (SubjectValue.RangeValue lower upper)
        else fail "Not a range"
    
    parseMeasurement = withObject "Measurement" $ \obj -> do
      ty <- obj .: "type"
      if ty == ("measurement" :: T.Text)
        then do
          unit <- obj .: "unit"
          val <- obj .: "value"
          return $ SubjectValue.VMeasurement unit val
        else fail "Not a measurement"
    
    parseArray = withArray "Array" $ \arr -> do
      vals <- mapM valueFromJSON (toList arr)
      return $ SubjectValue.VArray vals
    
    parseMap = withObject "Map" $ \obj -> do
      -- Only treat as map if it doesn't have a "type" field (which indicates a complex value)
      case KeyMap.lookup "type" obj of
        Just _ -> fail "Object with type field is not a map"
        Nothing -> do
          let pairs = KeyMap.toList obj
          valMap <- Map.fromList <$> mapM parseMapEntry pairs
          return $ SubjectValue.VMap valMap
      where
        parseMapEntry (k, v) = do
          val <- valueFromJSON v
          return (T.unpack $ toText k, val)

-- ToJSON instances
instance ToJSON (Pattern.Pattern Subject.Subject) where
  toJSON = patternToValue

instance FromJSON (Pattern.Pattern Subject.Subject) where
  parseJSON = patternFromValue

-- | Recursively sort all object keys alphabetically in a JSON Value
--
-- This function ensures that equivalent data structures produce byte-for-byte
-- identical JSON strings, enabling reliable automated comparison.
--
-- Special handling: Pattern objects always have "subject" before "elements"
-- to maintain semantic ordering.
--
-- @since 0.1.0
canonicalizeJSON :: Value -> Value
canonicalizeJSON (Object obj) = 
  let subjectKey = fromString "subject"
      elementsKey = fromString "elements"
      -- Special case: Pattern objects have exactly two keys: "subject" and "elements"
      -- Explicitly grab them and construct in the correct order
      subjectVal = KeyMap.lookup subjectKey obj
      elementsVal = KeyMap.lookup elementsKey obj
  in case (subjectVal, elementsVal) of
       (Just s, Just e) -> 
         -- Pattern object: subject first, then elements
         -- Use object to construct in correct order
         object [ subjectKey .= canonicalizeJSON s
                , elementsKey .= canonicalizeJSON e
                ]
       _ -> 
         -- Other objects: sort alphabetically
         let pairs = map canonicalizePair $ KeyMap.toList obj
         in Object $ KeyMap.fromList $ List.sortBy (\(k1, _) (k2, _) -> compare (toText k1) (toText k2)) pairs
  where
    canonicalizePair (k, v) = (k, canonicalizeJSON v)
canonicalizeJSON (Array arr) = Array $ fmap canonicalizeJSON arr
canonicalizeJSON v = v
