{-# LANGUAGE OverloadedStrings #-}
-- | Module: Gram.Schema.JSONSchema
--
-- Generates JSON Schema (Draft 2020-12) for Pattern<Subject>.
--
-- This module provides functions to generate a formal JSON Schema document
-- that specifies the structure of Pattern<Subject> for validation and
-- documentation purposes.
--
-- @since 0.1.0
module Gram.Schema.JSONSchema
  ( generatePatternSchema
  , generateSubjectDefinition
  , generateValueDefinition
  ) where

import Data.Aeson
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T

-- | Generate complete JSON Schema (Draft 2020-12) for Pattern<Subject>
--
-- Returns a JSON Value representing the complete schema document with:
-- - Schema metadata ($schema, $id, version)
-- - Pattern definition (recursive structure)
-- - Subject definition (identity, labels, properties)
-- - Value definitions (all 10 value types with discriminators)
--
-- Example usage:
-- @
-- let schema = generatePatternSchema
-- putStrLn $ encodePretty schema
-- @
--
-- @since 0.1.0
generatePatternSchema :: Value
generatePatternSchema = object
  [ "$schema" .= ("https://json-schema.org/draft/2020-12/schema" :: T.Text)
  , "$id" .= ("https://gram-data.github.io/pattern-hs/schemas/pattern-subject.json" :: T.Text)
  , "title" .= ("Pattern<Subject>" :: T.Text)
  , "description" .= ("Canonical JSON Schema for Pattern<Subject> - a recursive graph pattern structure" :: T.Text)
  , "version" .= ("0.1.0" :: T.Text)
  , "$defs" .= object
      [ "Pattern" .= generatePatternDefinition
      , "Subject" .= generateSubjectDefinition
      , "Value" .= generateValueDefinition
      , "Symbol" .= object
          [ "type" .= ("object" :: T.Text)
          , "properties" .= object
              [ "type" .= object ["const" .= ("symbol" :: T.Text)]
              , "value" .= object ["type" .= ("string" :: T.Text)]
              ]
          , "required" .= (["type", "value"] :: [T.Text])
          ]
      , "TaggedString" .= object
          [ "type" .= ("object" :: T.Text)
          , "properties" .= object
              [ "type" .= object ["const" .= ("tagged" :: T.Text)]
              , "tag" .= object ["type" .= ("string" :: T.Text)]
              , "content" .= object ["type" .= ("string" :: T.Text)]
              ]
          , "required" .= (["type", "tag", "content"] :: [T.Text])
          ]
      , "Range" .= object
          [ "type" .= ("object" :: T.Text)
          , "properties" .= object
              [ "type" .= object ["const" .= ("range" :: T.Text)]
              , "lower" .= object
                  [ "oneOf" .=
                      [ object ["type" .= ("number" :: T.Text)]
                      , object ["type" .= ("null" :: T.Text)]
                      ]
                  ]
              , "upper" .= object
                  [ "oneOf" .=
                      [ object ["type" .= ("number" :: T.Text)]
                      , object ["type" .= ("null" :: T.Text)]
                      ]
                  ]
              ]
          , "required" .= (["type", "lower", "upper"] :: [T.Text])
          ]
      , "Measurement" .= object
          [ "type" .= ("object" :: T.Text)
          , "properties" .= object
              [ "type" .= object ["const" .= ("measurement" :: T.Text)]
              , "unit" .= object ["type" .= ("string" :: T.Text)]
              , "value" .= object ["type" .= ("number" :: T.Text)]
              ]
          , "required" .= (["type", "unit", "value"] :: [T.Text])
          ]
      ]
  , "$ref" .= ("#/$defs/Pattern" :: T.Text)
  ]

-- | Generate JSON Schema definition for Pattern
--
-- Pattern is recursive: it contains a Subject and an array of nested Patterns.
--
-- @since 0.1.0
generatePatternDefinition :: Value
generatePatternDefinition = object
  [ "type" .= ("object" :: T.Text)
  , "description" .= ("A pattern with a subject and optional nested pattern elements" :: T.Text)
  , "properties" .= object
      [ "subject" .= object ["$ref" .= ("#/$defs/Subject" :: T.Text)]
      , "elements" .= object
          [ "type" .= ("array" :: T.Text)
          , "items" .= object ["$ref" .= ("#/$defs/Pattern" :: T.Text)]
          , "default" .= ([] :: [Value])
          ]
      ]
  , "required" .= (["subject", "elements"] :: [T.Text])
  , "additionalProperties" .= False
  ]

-- | Generate JSON Schema definition for Subject
--
-- Subject contains:
-- - identity: string identifier
-- - labels: array of strings
-- - properties: map of string keys to Value types
--
-- @since 0.1.0
generateSubjectDefinition :: Value
generateSubjectDefinition = object
  [ "type" .= ("object" :: T.Text)
  , "description" .= ("A subject with identity, labels, and properties" :: T.Text)
  , "properties" .= object
      [ "identity" .= object
          [ "type" .= ("string" :: T.Text)
          , "description" .= ("Identity symbol for the subject" :: T.Text)
          ]
      , "labels" .= object
          [ "type" .= ("array" :: T.Text)
          , "items" .= object ["type" .= ("string" :: T.Text)]
          , "uniqueItems" .= True
          , "default" .= ([] :: [Value])
          , "description" .= ("Set of labels classifying the subject" :: T.Text)
          ]
      , "properties" .= object
          [ "type" .= ("object" :: T.Text)
          , "additionalProperties" .= object ["$ref" .= ("#/$defs/Value" :: T.Text)]
          , "default" .= object []
          , "description" .= ("Map of property names to values" :: T.Text)
          ]
      ]
  , "required" .= (["identity", "labels", "properties"] :: [T.Text])
  , "additionalProperties" .= False
  ]

-- | Generate JSON Schema definition for Value with oneOf discriminator
--
-- Value uses oneOf to represent all 10 possible value types:
-- - Simple: integer, number (decimal), boolean, string
-- - Complex: symbol, tagged string, array, map, range, measurement
--
-- @since 0.1.0
generateValueDefinition :: Value
generateValueDefinition = object
  [ "oneOf" .=
      [ -- Simple value types (primitives)
        object
          [ "type" .= ("integer" :: T.Text)
          , "description" .= ("Integer value" :: T.Text)
          ]
      , object
          [ "type" .= ("number" :: T.Text)
          , "description" .= ("Decimal/floating-point value" :: T.Text)
          ]
      , object
          [ "type" .= ("boolean" :: T.Text)
          , "description" .= ("Boolean value" :: T.Text)
          ]
      , object
          [ "type" .= ("string" :: T.Text)
          , "description" .= ("String value" :: T.Text)
          ]
      -- Complex value types (structured)
      , object
          [ "$ref" .= ("#/$defs/Symbol" :: T.Text)
          , "description" .= ("Symbol value with type discriminator" :: T.Text)
          ]
      , object
          [ "$ref" .= ("#/$defs/TaggedString" :: T.Text)
          , "description" .= ("Tagged string value (e.g., URL, JSON, code)" :: T.Text)
          ]
      , object
          [ "type" .= ("array" :: T.Text)
          , "items" .= object ["$ref" .= ("#/$defs/Value" :: T.Text)]
          , "description" .= ("Array of values" :: T.Text)
          ]
      , object
          [ "type" .= ("object" :: T.Text)
          , "additionalProperties" .= object ["$ref" .= ("#/$defs/Value" :: T.Text)]
          , "not" .= object ["required" .= (["type"] :: [T.Text])]
          , "description" .= ("Map of string keys to values (no 'type' discriminator)" :: T.Text)
          ]
      , object
          [ "$ref" .= ("#/$defs/Range" :: T.Text)
          , "description" .= ("Numeric range value with optional bounds" :: T.Text)
          ]
      , object
          [ "$ref" .= ("#/$defs/Measurement" :: T.Text)
          , "description" .= ("Measurement value with unit and numeric value" :: T.Text)
          ]
      ]
  , "description" .= ("A value can be integer, number, boolean, string, symbol, tagged string, array, map, range, or measurement" :: T.Text)
  ]
