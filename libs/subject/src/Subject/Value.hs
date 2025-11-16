-- | Value type system for Subject properties.
--
-- This module defines the `Value` type that supports all gram notation value types,
-- including standard types (integers, decimals, booleans, strings, symbols) and
-- extended types (tagged strings, arrays, maps, ranges, measurements).
--
-- The Value type enables property records to store rich, structured data that
-- matches the gram notation specification.
--
-- == Value Types
--
-- The Value type supports the following value types from gram notation:
--
-- * Standard types:
--   - Integers: @VInteger Integer@
--   - Decimals: @VDecimal Double@
--   - Booleans: @VBoolean Bool@
--   - Strings: @VString String@
--   - Symbols: @VSymbol String@
--
-- * Extended types:
--   - Tagged strings: @VTaggedString String String@ (tag and content)
--   - Arrays: @VArray [Value]@ (lists of values)
--   - Maps: @VMap (Map String Value)@ (key-value maps)
--   - Ranges: @VRange RangeValue@ (numeric ranges)
--   - Measurements: @VMeasurement String Double@ (e.g., "5kg" -> ("kg", 5.0))
--
-- == Examples
--
-- Standard value types:
--
-- >>> VInteger 42
-- >>> VDecimal 3.14
-- >>> VBoolean True
-- >>> VString "hello"
-- >>> VSymbol "mySymbol"
--
-- Extended value types:
--
-- >>> VTaggedString "url" "https://example.com"
-- >>> VArray [VInteger 1, VInteger 2, VInteger 3]
-- >>> VMap (fromList [("key1", VString "value1"), ("key2", VInteger 42)])
-- >>> VRange (RangeValue (Just 1) (Just 10))
-- >>> VMeasurement "kg" 5.0
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Subject.Value
  ( Value (..)
  , RangeValue (..)
  ) where

import Data.Hashable (Hashable (..))
import Data.Map (Map)
import GHC.Generics (Generic)

-- | A range value representing numeric ranges.
--
-- Ranges can be:
-- * Closed: both lower and upper bounds specified
-- * Half-open: only lower or upper bound specified
-- * Open: neither bound specified (represents all numbers)
--
-- Examples:
--
-- >>> RangeValue (Just 1) (Just 10)  -- 1..10 (closed range)
-- >>> RangeValue (Just 1) Nothing    -- 1... (lower bound only)
-- >>> RangeValue Nothing (Just 10)   -- ...10 (upper bound only)
-- >>> RangeValue Nothing Nothing      -- ... (all numbers)
data RangeValue = RangeValue
  { lower :: Maybe Double  -- ^ Lower bound of the range (inclusive)
  , upper :: Maybe Double  -- ^ Upper bound of the range (inclusive)
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A value that can be stored in a Subject property record.
--
-- The Value type supports all gram notation value types, enabling property
-- records to store rich, structured data. Values can be nested (arrays and
-- maps can contain other values), enabling complex data structures.
--
-- === Standard Types
--
-- Standard value types represent basic data:
--
-- * @VInteger Integer@ - Integer numbers (e.g., @42@, @-10@)
-- * @VDecimal Double@ - Decimal numbers (e.g., @3.14@, @-0.5@)
-- * @VBoolean Bool@ - Boolean values (@True@ or @False@)
-- * @VString String@ - String literals (e.g., @\"hello\"@)
-- * @VSymbol String@ - Symbol identifiers (e.g., @mySymbol@)
--
-- === Extended Types
--
-- Extended value types represent structured or qualified data:
--
-- * @VTaggedString String String@ - Tagged strings with a type tag and content
--   (e.g., @url`https://example.com`@ becomes @VTaggedString "url" "https://example.com"@)
-- * @VArray [Value]@ - Arrays of values (e.g., @[1, 2, 3]@)
-- * @VMap (Map String Value)@ - Key-value maps (e.g., @{key: "value"}@)
-- * @VRange RangeValue@ - Numeric ranges (e.g., @1..10@, @1...@, @...10@)
-- * @VMeasurement String Double@ - Measurements with units (e.g., @5kg@ becomes @VMeasurement "kg" 5.0@)
--
-- === Nested Values
--
-- Values can be nested to create complex structures:
--
-- >>> VArray [VString "a", VString "b", VString "c"]
-- >>> VMap (fromList [("nested", VArray [VInteger 1, VInteger 2])])
--
-- === Examples
--
-- Creating values:
--
-- >>> VInteger 42
-- >>> VDecimal 3.14
-- >>> VBoolean True
-- >>> VString "hello"
-- >>> VSymbol "mySymbol"
-- >>> VTaggedString "url" "https://example.com"
-- >>> VArray [VInteger 1, VInteger 2, VInteger 3]
-- >>> VMap (fromList [("name", VString "Alice"), ("age", VInteger 30)])
-- >>> VRange (RangeValue (Just 1) (Just 10))
-- >>> VMeasurement "kg" 5.0
data Value
  = VInteger Integer
  | VDecimal Double
  | VBoolean Bool
  | VString String
  | VSymbol String
  | VTaggedString String String  -- ^ Tag and content
  | VArray [Value]
  | VMap (Map String Value)
  | VRange RangeValue
  | VMeasurement String Double   -- ^ Unit and numeric value (e.g., "kg" and 5.0 for "5kg")
  deriving (Eq, Ord, Show, Generic, Hashable)

