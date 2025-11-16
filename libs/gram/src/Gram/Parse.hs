-- | Parsing gram notation to Pattern Subject.
--
-- This module provides functions to parse gram notation text format into
-- Pattern Subject data structures. The parsing handles all aspects of
-- gram notation including:
--
-- * Subject identity (symbols, quoted strings, numbers)
-- * Labels (single and multiple)
-- * Property records with all value types
-- * Pattern structure (nodes, relationships, nested patterns)
--
-- == Parsing Strategy
--
-- The parsing process converts gram notation to Haskell data structures:
--
-- * gram notation string → @Pattern Subject@
-- * gram attributes notation → @Subject@
-- * gram value notation → @Value@ types
--
-- == Examples
--
-- Parsing a simple subject:
--
-- >>> fromGram "(n:Person)"
-- Right (Pattern { value = Subject (Symbol "n") (Set.fromList ["Person"]) empty, elements = [] })
--
-- Parsing with properties:
--
-- >>> fromGram "(n:Person {name:\"Alice\"})"
-- Right (Pattern { value = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")]), elements = [] })
--
-- Parsing errors:
--
-- >>> fromGram "(invalid"
-- Left (ParseError "Unexpected end of input")
module Gram.Parse
  ( fromGram
  , ParseError(..)
  ) where

import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..))

-- | Parse error type for gram notation parsing.
--
-- Represents errors that can occur during parsing of gram notation,
-- such as syntax errors, unexpected tokens, or incomplete input.
data ParseError = ParseError String
  deriving (Eq, Show)

-- | Parse gram notation to Pattern Subject.
--
-- Converts a gram notation string into a Pattern Subject data structure.
-- Returns either a parsed Pattern Subject or a ParseError if parsing fails.
--
-- === Examples
--
-- Simple subject:
--
-- >>> fromGram "(n:Person)"
-- Right (Pattern { value = Subject (Symbol "n") (Set.fromList ["Person"]) empty, elements = [] })
--
-- Subject with properties:
--
-- >>> fromGram "(n:Person {name:\"Alice\"})"
-- Right (Pattern { value = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")]), elements = [] })
--
-- Parse error:
--
-- >>> fromGram "(invalid"
-- Left (ParseError "Unexpected end of input")
--
-- **Note**: This is a placeholder implementation. Full parsing will be
-- implemented to handle all gram notation features including:
-- * Anonymous subjects (generating identities during parsing)
-- * All value types (integers, decimals, booleans, strings, symbols, tagged strings, arrays, maps, ranges, measurements)
-- * Pattern structure (nested patterns, relationships)
-- * Proper error messages with position information
fromGram :: String -> Either ParseError (Pattern Subject)
fromGram _ = Left (ParseError "fromGram: Not yet implemented")

