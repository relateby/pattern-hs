-- | Serialization of Pattern Subject to gram notation.
--
-- This module provides functions to convert Pattern Subject data structures
-- into gram notation text format. The serialization handles all aspects of
-- gram notation including:
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
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
-- == String Value Serialization
--
-- String values use different formats based on length:
--
-- * __Short strings__ (≤120 characters): Double-quoted with escapes
-- * __Long strings__ (>120 characters): Codefence format (triple-backtick)
--
-- Tagged strings follow the same threshold for inline vs codefence:
--
-- * __Short tagged__: @tag\`content\`@
-- * __Long tagged__: @\`\`\`tag\\ncontent\\n\`\`\`@
--
-- The threshold is defined by 'codefenceThreshold' (120 characters).
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
  , toGramWithHeader
  , serializePattern
  , codefenceThreshold
  ) where

import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..), RangeValue(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (isAlpha, isAlphaNum)

-- | Character threshold for codefence serialization.
--
-- Strings with length greater than this value will be serialized
-- using codefence format (triple-backticks). Length is measured as 
-- total character count including newline characters.
--
-- Strings of this length or fewer use standard quote-delimited format.
--
-- === Examples
--
-- >>> codefenceThreshold
-- 120
--
-- >>> length "short string" <= codefenceThreshold
-- True
codefenceThreshold :: Int
codefenceThreshold = 120

-- | Escape special characters in strings for gram notation.
--
-- Escapes quotes, backslashes, and control characters in string values 
-- to ensure proper serialization in gram notation.
--
-- === Examples
--
-- >>> escapeString "Hello"
-- "Hello"
--
-- >>> escapeString "He said \"Hello\""
-- "He said \\\"Hello\\\""
--
-- >>> escapeString "Line 1\nLine 2"
-- "Line 1\\nLine 2"
escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

-- | Check if a string can safely use codefence format.
--
-- A string can use codefence format if:
--
-- 1. It exceeds the length threshold (120 characters)
-- 2. It does NOT contain the closing fence pattern (@\\n\`\`\`@)
--
-- If the string contains the closing fence pattern, using codefence
-- format would cause the parser to truncate content at that point,
-- violating round-trip preservation.
--
-- === Examples
--
-- >>> canUseCodefence (replicate 121 'x')
-- True
--
-- >>> canUseCodefence (replicate 100 'x' ++ "\n```" ++ replicate 50 'y')
-- False
canUseCodefence :: String -> Bool
canUseCodefence s = 
  length s > codefenceThreshold && not (containsClosingFence s)
  where
    containsClosingFence str = "\n```" `isInfixOf` str
    isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- | Escape content for backtick-delimited strings.
--
-- Escapes backticks and newlines in content that will be placed
-- inside a single-backtick delimited string (tag\`content\`).
--
-- === Examples
--
-- >>> escapeBacktickedContent "hello"
-- "hello"
--
-- >>> escapeBacktickedContent "a`b"
-- "a\\`b"
escapeBacktickedContent :: String -> String
escapeBacktickedContent = concatMap escapeChar
  where
    escapeChar '`' = "\\`"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

-- | Serialize a string using codefence format for long strings.
--
-- Uses triple-backtick codefence format for strings exceeding the
-- threshold length. Content is preserved without escaping.
--
-- === Examples
--
-- >>> serializeCodefenceString "Long content..."
-- "```\nLong content...\n```"
serializeCodefenceString :: String -> String
serializeCodefenceString s = "```\n" ++ s ++ "\n```"

-- | Serialize a tagged string using codefence format for long content.
--
-- Uses triple-backtick codefence format with tag for tagged strings
-- whose content exceeds the threshold length.
--
-- === Examples
--
-- >>> serializeTaggedCodefenceString "md" "Long markdown..."
-- "```md\nLong markdown...\n```"
serializeTaggedCodefenceString :: String -> String -> String
serializeTaggedCodefenceString tag content = "```" ++ tag ++ "\n" ++ content ++ "\n```"

-- | Format a Symbol for gram notation.
--
-- Returns the symbol string, quoted with backticks if necessary.
-- Symbols in gram notation are unquoted identifiers if they start with
-- a letter/underscore and contain only alphanumeric/specified chars.
-- Otherwise they must be quoted.
--
-- === Examples
--
-- >>> quoteSymbol (Symbol "n")
-- "n"
--
-- >>> quoteSymbol (Symbol "my-id.1")
-- "my-id.1"
--
-- >>> quoteSymbol (Symbol "dear world")
-- "`dear world`"
quoteSymbol :: Symbol -> String
quoteSymbol (Symbol s)
  | needsQuoting s = "`" ++ escapeBackticks s ++ "`"
  | otherwise = s
  where
    needsQuoting "" = True
    needsQuoting (c:cs) = not (isIdStart c) || any (not . isIdChar) cs
    
    isIdStart c = isAlpha c || c == '_'
    isIdChar c = isAlphaNum c || c == '_' || c == '-' || c == '.' || c == '@'
    
    escapeBackticks = concatMap (\c -> if c == '`' then "\\`" else [c])

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
serializeValue (VString s)
  | canUseCodefence s = serializeCodefenceString s
  | otherwise = "\"" ++ escapeString s ++ "\""
serializeValue (VSymbol sym) = sym
serializeValue (VTaggedString tag content)
  | canUseCodefence content = serializeTaggedCodefenceString tag content
  | otherwise = tag ++ "`" ++ escapeBacktickedContent content ++ "`"
serializeValue (VArray vs) = "[" ++ intercalate "," (map serializeValue vs) ++ "]"
serializeValue (VMap m) = "{" ++ intercalate "," (map serializeProperty (Map.toList m)) ++ "}"
  where
    serializeProperty (k, v) = k ++ ":" ++ serializeValue v
serializeValue (VRange (RangeValue (Just lowerVal) (Just upperVal))) = formatRangeDouble lowerVal ++ ".." ++ formatRangeDouble upperVal
serializeValue (VRange (RangeValue (Just lowerVal) Nothing)) = formatRangeDouble lowerVal ++ "..."
serializeValue (VRange (RangeValue Nothing (Just upperVal))) = "..." ++ formatRangeDouble upperVal
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

-- | Serialize identity, labels, and properties (attributes part).
-- Used internally by toGram for both node and subject syntax.
serializeIdentity :: Symbol -> String
serializeIdentity (Symbol "") = ""  -- Anonymous subject
serializeIdentity ident = quoteSymbol ident

serializeLabels :: Set String -> String
serializeLabels lbls
  | Set.null lbls = ""
  | otherwise = ":" ++ intercalate ":" (Set.toList lbls)

    -- | Serialize a Subject to gram notation (legacy function, kept for compatibility).
-- Note: This always uses node syntax. Use toGram for proper syntax selection.
-- NOTE: This function is currently unused but kept for potential future use.
{-# WARNING serializeSubject "This function is unused but kept for compatibility" #-}
serializeSubject :: Subject -> String
serializeSubject (Subject ident lbls props) =
  "(" ++
  serializeIdentity ident ++
  serializeLabels lbls ++
  serializePropertyRecord props

-- | Serialize pattern elements for implicit root (no brackets/pipe).
-- Used when serializing the top-level Gram container.
-- NOTE: This function is now inlined into toGram to handle properties.
-- Keeping signature for potential reuse or removing if unused.
-- serializeImplicitElements :: [Pattern Subject] -> String
-- serializeImplicitElements elems = 
--   intercalate "\n" (map toGram elems)

-- | Serialize pattern elements to gram notation.
--
-- Converts a list of Pattern Subject elements to gram notation.
-- For subjects with nested elements, elements are serialized after a pipe separator.
-- References (just identity, no labels/properties/elements) are serialized as just the symbol.
--
-- === Examples
--
-- >>> serializePatternElements []
-- ""
--
-- >>> serializePatternElements [Pattern (Subject (Symbol "a") Set.empty empty) []]
-- " a"
--
-- >>> serializePatternElements [Pattern (Subject (Symbol "a") (Set.fromList ["Person"]) empty) []]
-- " (a:Person)"
serializePatternElements :: [Pattern Subject] -> String
serializePatternElements elems
  | null elems = ""
  | otherwise = 
      let serialized = filter (not . null) $ map serializeNestedElement elems
      in intercalate ", " serialized
  where
    serializeNestedElement :: Pattern Subject -> String
    serializeNestedElement (Pattern (Subject ident lbls props) nested)
      -- If it's a named reference (just identity, no labels/properties/elements), serialize as just the symbol
      | Set.null lbls && Map.null props && null nested && ident /= Symbol "" =
          quoteSymbol ident
      -- Otherwise serialize as a full pattern (recursive call)
      | otherwise = serializePattern (Pattern (Subject ident lbls props) nested)

-- | True when the pattern represents a root/header record: anonymous identity
-- (@Symbol \"\"@), no labels, and no elements. Only the /first/ pattern in a
-- list is considered for bare-record serialization in 'toGram'.
isHeaderLikePattern :: Pattern Subject -> Bool
isHeaderLikePattern (Pattern (Subject ident lbls _) elems) =
  ident == Symbol "" && Set.null lbls && null elems

-- | Serialize a header-like pattern as a bare record @{k:v}@ or @{}@.
bareRecordForHeaderLike :: Pattern Subject -> String
bareRecordForHeaderLike (Pattern (Subject _ _ props) _) =
  if Map.null props then "{}" else trimLeadingSpace (serializePropertyRecord props)

-- | Serialize a list of Patterns to gram notation (newline-separated).
--
-- __First-pattern rule__: If the first pattern is anonymous (@identity == Symbol \"\"@),
-- has no labels, and has no elements, it is serialized as a bare root record
-- @{k:v}@ or @{}@. Only the first pattern is considered; the same shape in
-- any other position is serialized with 'serializePattern'.
--
-- Round-trip: @fromGram s >>= toGram@ preserves a leading bare record as the
-- first element. With 'fromGramWithIds', the header pattern gets an assigned
-- identity and is no longer header-like, so it will serialize as a normal
-- pattern (e.g. @(#1 {...})@).
toGram :: [Pattern Subject] -> String
toGram [] = ""
toGram (p : ps)
  | isHeaderLikePattern p =
      let firstLine = bareRecordForHeaderLike p
          rest = if null ps then "" else "\n" ++ intercalate "\n" (map serializePattern ps)
      in firstLine ++ rest
  | otherwise = intercalate "\n" (map serializePattern (p : ps))

-- | Serialize a header record and a list of Patterns to gram notation.
--
-- The header is always emitted as a bare record @{k:v}@ or @{}@, then a newline,
-- then 'toGram' of the patterns. The header is explicit; the first-pattern rule
-- of 'toGram' still applies to the pattern list (so a header-like first pattern
-- would also serialize as a bare record on the next line).
toGramWithHeader :: Map String Value -> [Pattern Subject] -> String
toGramWithHeader header patterns =
  let headerStr = serializePropertyRecord header
      patternsStr = toGram patterns
  in case (null headerStr, null patternsStr) of
       (True, True) -> "{}"
       (False, True) -> trimLeadingSpace headerStr
       (True, False) -> patternsStr
       (False, False) -> trimLeadingSpace headerStr ++ "\n" ++ patternsStr

-- | Serialize a Pattern Subject to gram notation.
--
-- Converts a Pattern Subject data structure into its gram notation
-- string representation. The output follows the gram notation specification:
-- - Patterns with elements use subject syntax: `[attributes | elements]`
-- - Patterns without elements use node syntax: `(attributes)`
--
-- === Examples
--
-- Simple node (no elements):
--
-- >>> import Pattern.Core (Pattern(..))
-- >>> import Subject.Core (Subject(..), Symbol(..))
-- >>> import Data.Set (Set)
-- >>> import qualified Data.Set as Set
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
-- >>> let p = Pattern { value = s, elements = [] }
-- >>> serializePattern p
-- "(n:Person)"
--
-- Node with properties (no elements):
--
-- >>> import Data.Map (fromList)
-- >>> import Subject.Value (VString)
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
-- >>> let p = Pattern { value = s, elements = [] }
-- >>> serializePattern p
-- "(n:Person {name:\"Alice\"})"
--
-- Subject with nested elements:
--
-- >>> let inner1 = Pattern (Subject (Symbol "a") Set.empty empty) []
-- >>> let inner2 = Pattern (Subject (Symbol "b") Set.empty empty) []
-- >>> let outer = Pattern (Subject (Symbol "g") Set.empty empty) [inner1, inner2]
-- >>> serializePattern outer
-- "[g | a, b]"
serializePattern :: Pattern Subject -> String
serializePattern p@(Pattern subj elems)
  | isImplicitRoot subj = serializeImplicitElements (properties subj) elems -- Implicit root -> record + elements
  | null elems = serializeSubjectAsNode subj  -- No elements -> node syntax
  | Just edges <- isWalkPattern p = serializeWalkPattern edges
  | Just (rel, left, right) <- isEdgePattern p = serializeEdgePattern rel left right
  | otherwise = serializeSubjectAsSubject subj elems  -- Has elements -> subject syntax

-- | Check if subject is the Implicit Root
-- Identified by "Gram.Root" label.
isImplicitRoot :: Subject -> Bool
isImplicitRoot (Subject (Symbol "") lbls _) = "Gram.Root" `Set.member` lbls
isImplicitRoot _ = False

-- | Serialize pattern elements for implicit root (record + elements).
-- Note: We do NOT serialize the "Gram.Root" label itself, as it is implicit in the file structure.
serializeImplicitElements :: Map String Value -> [Pattern Subject] -> String
serializeImplicitElements props' elems' = 
  let propsStr = if Map.null props' then "" else serializePropertyRecord props'
      elemsStr = intercalate "\n" (map serializePattern elems')
  in case (null propsStr, null elemsStr) of
       (True, True) -> "{}" -- Empty graph/root
       (False, True) -> trimLeadingSpace propsStr -- Remove leading space from serializePropertyRecord
       (True, False) -> elemsStr
       (False, False) -> trimLeadingSpace propsStr ++ "\n" ++ elemsStr

-- | Helper to trim leading space from property records
trimLeadingSpace :: String -> String
trimLeadingSpace (' ':xs) = xs
trimLeadingSpace xs = xs

-- | Check if pattern is a Walk Pattern: [Gram.Walk | edge1, edge2, ...]
isWalkPattern :: Pattern Subject -> Maybe [Pattern Subject]
isWalkPattern (Pattern (Subject _ lbls _) edges)
  | "Gram.Walk" `Set.member` lbls = Just edges
  | otherwise = Nothing

-- | Serialize a Walk Pattern as chained path: (a)->(b)->(c)
-- Assumes edges are connected: (a)->(b), (b)->(c), etc.
-- We serialize the first edge fully: (a)->(b)
-- Then for subsequent edges, we only serialize the relationship and right node: ->(c)
serializeWalkPattern :: [Pattern Subject] -> String
serializeWalkPattern [] = ""
serializeWalkPattern [e] = serializePattern e -- Fallback to normal serialization for single edge
serializeWalkPattern (first:rest) = 
  serializePattern first ++ concatMap serializeConnectedEdge rest

-- | Helper to serialize subsequent edges in a walk: -[rel]->(right)
-- We assume the left node of this edge matches the right node of the previous one,
-- so we skip serializing the left node.
serializeConnectedEdge :: Pattern Subject -> String
serializeConnectedEdge p' = 
  case isEdgePattern p' of
    Just (rel, _, right) -> "-" ++ serializeRelationship rel ++ "->" ++ serializePattern right
    Nothing -> " | " ++ serializePattern p' -- Fallback if walk contains non-edge (shouldn't happen in valid walks)

-- | Check if pattern is an Edge Pattern: [rel | left, right]
-- Note: This assumes the parser's convention where an edge is a pattern
-- whose value is the relationship subject and has exactly two elements (nodes).
-- We MUST verify that left and right are atomic (Nodes), otherwise we might
-- falsely identify a list of 2 paths as an edge!
isEdgePattern :: Pattern Subject -> Maybe (Subject, Pattern Subject, Pattern Subject)
isEdgePattern (Pattern r [l@(Pattern _ []), r_node@(Pattern _ [])]) = Just (r, l, r_node)
isEdgePattern _ = Nothing

-- | Serialize an Edge Pattern as path syntax: (left)-[rel]->(right)
serializeEdgePattern :: Subject -> Pattern Subject -> Pattern Subject -> String
serializeEdgePattern rel left right =
  serializePattern left ++ "-" ++ serializeRelationship rel ++ "->" ++ serializePattern right

-- | Serialize relationship part: [rel] or just - if anonymous/empty
serializeRelationship :: Subject -> String
serializeRelationship (Subject (Symbol "") lbls props)
  | Set.null lbls && Map.null props = ""  -- Anonymous relationship: returns empty string so result is -- + "" + -> = -->
  | otherwise = "[" ++ serializeLabels lbls ++ serializePropertyRecord props ++ "]"
serializeRelationship (Subject ident lbls props) =
  "[" ++ serializeIdentity ident ++ serializeLabels lbls ++ serializePropertyRecord props ++ "]"

serializeSubjectAsNode :: Subject -> String
serializeSubjectAsNode (Subject ident lbls props) =
  "(" ++
  serializeIdentity ident ++
  serializeLabels lbls ++
  serializePropertyRecord props ++
  ")"

serializeSubjectAsSubject :: Subject -> [Pattern Subject] -> String
serializeSubjectAsSubject (Subject ident lbls props) nested =
  "[" ++
  serializeIdentity ident ++
  serializeLabels lbls ++
  serializePropertyRecord props ++
  (if not (null nested) then " | " else "") ++
  serializePatternElements nested ++
  "]"
