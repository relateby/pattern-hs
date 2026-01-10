{-# LANGUAGE OverloadedStrings #-}
-- | Module: Gram.Schema.TypeScript
--
-- Generates TypeScript type definitions for Pattern<Subject>.
--
-- This module provides functions to generate TypeScript interfaces and types
-- that model Pattern<Subject> for downstream TypeScript/JavaScript ports.
--
-- @since 0.1.0
module Gram.Schema.TypeScript
  ( generateTypeScriptTypes
  ) where

import qualified Data.Text as T
import Data.Text (Text)

-- | Generate TypeScript type definitions for Pattern<Subject>
--
-- Returns TypeScript code with:
-- - Interface definitions for Pattern, Subject
-- - Discriminated union types for Value
-- - Type guards for runtime type checking
-- - JSDoc comments for documentation
--
-- Example usage:
-- @
-- let tsCode = generateTypeScriptTypes
-- writeFile "pattern.ts" (T.unpack tsCode)
-- @
--
-- @since 0.1.0
generateTypeScriptTypes :: Text
generateTypeScriptTypes = T.unlines
  [ "/**"
  , " * Pattern<Subject> TypeScript Type Definitions"
  , " * Generated from gram-hs canonical JSON Schema"
  , " * Version: 0.1.0"
  , " */"
  , ""
  , "/**"
  , " * A pattern with a subject value and optional nested pattern elements."
  , " * Patterns are recursive: they can contain other patterns as elements."
  , " */"
  , "export interface Pattern {"
  , "  /** The subject value of this pattern */"
  , "  value: Subject;"
  , "  /** Nested pattern elements */"
  , "  elements: Pattern[];"
  , "}"
  , ""
  , "/**"
  , " * A subject with identity (symbol), labels, and properties."
  , " */"
  , "export interface Subject {"
  , "  /** Identity symbol for the subject */"
  , "  symbol: string;"
  , "  /** Set of labels classifying the subject */"
  , "  labels: string[];"
  , "  /** Map of property names to values */"
  , "  properties: Record<string, Value>;"
  , "}"
  , ""
  , "/**"
  , " * Symbol value with type discriminator"
  , " */"
  , "export interface ValueSymbol {"
  , "  type: 'symbol';"
  , "  value: string;"
  , "}"
  , ""
  , "/**"
  , " * Tagged string value (e.g., URL, JSON, code)"
  , " */"
  , "export interface ValueTaggedString {"
  , "  type: 'tagged';"
  , "  tag: string;"
  , "  content: string;"
  , "}"
  , ""
  , "/**"
  , " * Numeric range value with optional bounds"
  , " */"
  , "export interface ValueRange {"
  , "  type: 'range';"
  , "  lower: number | null;"
  , "  upper: number | null;"
  , "}"
  , ""
  , "/**"
  , " * Measurement value with unit and numeric value"
  , " */"
  , "export interface ValueMeasurement {"
  , "  type: 'measurement';"
  , "  unit: string;"
  , "  value: number;"
  , "}"
  , ""
  , "/**"
  , " * Value can be integer, number, boolean, string, symbol, tagged string,"
  , " * array, map, range, or measurement."
  , " *"
  , " * This is a discriminated union type. Complex types use a 'type' field"
  , " * for discrimination at runtime."
  , " */"
  , "export type Value ="
  , "  | number              // Integer or decimal"
  , "  | boolean"
  , "  | string"
  , "  | ValueSymbol"
  , "  | ValueTaggedString"
  , "  | Value[]             // Array of values"
  , "  | Record<string, Value>  // Map (no 'type' field)"
  , "  | ValueRange"
  , "  | ValueMeasurement;"
  , ""
  , "// Type guards for runtime type checking"
  , ""
  , "/**"
  , " * Type guard to check if a value is a ValueSymbol"
  , " */"
  , "export function isValueSymbol(value: Value): value is ValueSymbol {"
  , "  return typeof value === 'object' && value !== null && 'type' in value && value.type === 'symbol';"
  , "}"
  , ""
  , "/**"
  , " * Type guard to check if a value is a ValueTaggedString"
  , " */"
  , "export function isValueTaggedString(value: Value): value is ValueTaggedString {"
  , "  return typeof value === 'object' && value !== null && 'type' in value && value.type === 'tagged';"
  , "}"
  , ""
  , "/**"
  , " * Type guard to check if a value is a ValueRange"
  , " */"
  , "export function isValueRange(value: Value): value is ValueRange {"
  , "  return typeof value === 'object' && value !== null && 'type' in value && value.type === 'range';"
  , "}"
  , ""
  , "/**"
  , " * Type guard to check if a value is a ValueMeasurement"
  , " */"
  , "export function isValueMeasurement(value: Value): value is ValueMeasurement {"
  , "  return typeof value === 'object' && value !== null && 'type' in value && value.type === 'measurement';"
  , "}"
  , ""
  , "/**"
  , " * Type guard to check if a value is an array"
  , " */"
  , "export function isValueArray(value: Value): value is Value[] {"
  , "  return Array.isArray(value);"
  , "}"
  , ""
  , "/**"
  , " * Type guard to check if a value is a map (plain object without 'type' field)"
  , " */"
  , "export function isValueMap(value: Value): value is Record<string, Value> {"
  , "  return typeof value === 'object' && value !== null && !Array.isArray(value) && !('type' in value);"
  , "}"
  ]
