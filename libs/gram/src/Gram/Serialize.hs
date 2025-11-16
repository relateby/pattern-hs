-- | Serialization of Pattern Subject to gram notation.
--
-- This module provides functions to convert Pattern Subject data structures
-- into gram notation text format. The serialization handles all aspects of
-- gram notation including:
--
-- * Subject identity (symbols, quoted strings, numbers)
-- * Labels (single and multiple)
-- * Property records with all value types
-- * Pattern structure (nodes, relationships, nested patterns)
--
-- == Serialization Strategy
--
-- The serialization process converts Haskell data structures to gram notation:
--
-- * @Pattern Subject@ → gram notation string
-- * @Subject@ → gram attributes notation (identity, labels, properties)
-- * @Value@ types → gram value notation
--
-- == Examples
--
-- Serializing a simple subject:
--
-- >>> import Pattern.Core (Pattern(..))
-- >>> import Subject.Core (Subject(..), Symbol(..))
-- >>> import Data.Set (Set)
-- >>> import qualified Data.Set as Set
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
-- >>> let p = Pattern { value = s, elements = [] }
-- >>> toGram p
-- "(n:Person)"
--
-- Serializing with properties:
--
-- >>> import Data.Map (fromList)
-- >>> import Subject.Value (VString)
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
-- >>> let p = Pattern { value = s, elements = [] }
-- >>> toGram p
-- "(n:Person {name:\"Alice\"})"
module Gram.Serialize
  ( toGram
  ) where

import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..), RangeValue(..))
import Data.Map (Map, empty)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Escape special characters in strings for gram notation.
--
-- Escapes quotes and backslashes in string values to ensure proper
-- serialization in gram notation.
--
-- === Examples
--
-- >>> escapeString "Hello"
-- "Hello"
--
-- >>> escapeString "He said \"Hello\""
-- "He said \\\"Hello\\\""
escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

-- | Format a Symbol for gram notation.
--
-- Returns the symbol string as-is. Symbols in gram notation are
-- unquoted identifiers.
--
-- === Examples
--
-- >>> quoteSymbol (Symbol "n")
-- "n"
--
-- >>> quoteSymbol (Symbol "myId")
-- "myId"
quoteSymbol :: Symbol -> String
quoteSymbol (Symbol s) = s

-- | Serialize a Value to gram notation.
--
-- Converts a Value type to its gram notation string representation.
-- Handles all standard and extended value types.
--
-- === Examples
--
-- >>> serializeValue (VInteger 42)
-- "42"
--
-- >>> serializeValue (VString "Alice")
-- "\"Alice\""
--
-- >>> serializeValue (VArray [VInteger 1, VInteger 2])
-- "[1,2]"
serializeValue :: Value -> String
serializeValue (VInteger i) = show i
serializeValue (VDecimal d) = show d
serializeValue (VBoolean True) = "true"
serializeValue (VBoolean False) = "false"
serializeValue (VString s) = "\"" ++ escapeString s ++ "\""
serializeValue (VSymbol sym) = sym
serializeValue (VTaggedString tag content) = tag ++ "`" ++ content ++ "`"
serializeValue (VArray vs) = "[" ++ intercalate "," (map serializeValue vs) ++ "]"
serializeValue (VMap m) = "{" ++ intercalate "," (map serializeProperty (Map.toList m)) ++ "}"
  where
    serializeProperty (k, v) = k ++ ":" ++ serializeValue v
serializeValue (VRange (RangeValue (Just lower) (Just upper))) = formatRangeDouble lower ++ ".." ++ formatRangeDouble upper
serializeValue (VRange (RangeValue (Just lower) Nothing)) = formatRangeDouble lower ++ "..."
serializeValue (VRange (RangeValue Nothing (Just upper))) = "..." ++ formatRangeDouble upper
serializeValue (VRange (RangeValue Nothing Nothing)) = "..."
serializeValue (VMeasurement unit val) = show val ++ unit

-- | Format a Double for range serialization, showing as integer if whole number.
formatRangeDouble :: Double -> String
formatRangeDouble d
  | d == fromInteger (round d) = show (round d :: Integer)
  | otherwise = show d

-- Helper function for intercalate
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | Serialize a property record to gram notation.
--
-- Converts a Map of property key-value pairs to gram notation property
-- record syntax: @{key1:value1,key2:value2}@
--
-- Note: Property order is not guaranteed as Map doesn't preserve insertion order.
--
-- === Examples
--
-- >>> serializePropertyRecord empty
-- ""
--
-- >>> serializePropertyRecord (fromList [("name", VString "Alice")])
-- "{name:\"Alice\"}"
serializePropertyRecord :: Map String Value -> String
serializePropertyRecord props
  | Map.null props = ""
  | otherwise = " {" ++ intercalate "," (map serializeProperty (Map.toList props)) ++ "}"
  where
    serializeProperty (k, v) = k ++ ":" ++ serializeValue v

-- | Serialize a Subject to gram notation.
--
-- Converts a Subject (identity, labels, properties) to gram notation
-- subject syntax: @(identity:label1:label2 {properties})@
--
-- === Examples
--
-- >>> serializeSubject (Subject (Symbol "n") (Set.fromList ["Person"]) empty)
-- "(n:Person)"
--
-- >>> serializeSubject (Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")]))
-- "(n:Person {name:\"Alice\"})"
serializeSubject :: Subject -> String
serializeSubject (Subject ident lbls props) =
  "(" ++
  serializeIdentity ident ++
  serializeLabels lbls ++
  serializePropertyRecord props
  where
    serializeIdentity (Symbol "") = ""  -- Anonymous subject
    serializeIdentity ident = quoteSymbol ident
    
    serializeLabels lbls
      | Set.null lbls = ""
      | otherwise = ":" ++ intercalate ":" (Set.toList lbls)

-- | Serialize pattern elements to gram notation.
--
-- Converts a list of Pattern Subject elements to gram notation,
-- recursively serializing each element. Elements are serialized
-- inside the parent subject's parentheses.
--
-- === Examples
--
-- >>> serializePatternElements []
-- ""
--
-- >>> serializePatternElements [Pattern (Subject (Symbol "a") (Set.fromList ["Person"]) empty) []]
-- " (a:Person)"
serializePatternElements :: [Pattern Subject] -> String
serializePatternElements = concatMap (\p -> " " ++ toGram p)

-- | Serialize a Pattern Subject to gram notation.
--
-- Converts a Pattern Subject data structure into its gram notation
-- string representation. The output follows the gram notation specification
-- for patterns, subjects, and values.
--
-- === Examples
--
-- Simple subject:
--
-- >>> import Pattern.Core (Pattern(..))
-- >>> import Subject.Core (Subject(..), Symbol(..))
-- >>> import Data.Set (Set)
-- >>> import qualified Data.Set as Set
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
-- >>> let p = Pattern { value = s, elements = [] }
-- >>> toGram p
-- "(n:Person)"
--
-- Subject with properties:
--
-- >>> import Data.Map (fromList)
-- >>> import Subject.Value (VString)
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
-- >>> let p = Pattern { value = s, elements = [] }
-- >>> toGram p
-- "(n:Person {name:\"Alice\"})"
--
-- Nested patterns:
--
-- >>> let inner = Pattern (Subject (Symbol "a") (Set.fromList ["Person"]) empty) []
-- >>> let outer = Pattern (Subject (Symbol "g") Set.empty empty) [inner]
-- >>> toGram outer
-- "(g (a:Person))"
toGram :: Pattern Subject -> String
toGram (Pattern subj elems) =
  serializeSubject subj ++ serializePatternElements elems ++ ")"
