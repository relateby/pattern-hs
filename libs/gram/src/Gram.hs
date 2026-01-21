-- | Main module for the Gram library.
--
-- This module provides convenient access to all Gram library functionality
-- by re-exporting public APIs from core modules. Import this module to access
-- all Gram types, functions, and utilities without needing to import
-- individual modules.
--
-- == Library Organization
--
-- The Gram library is organized into several modules:
--
-- * @Gram.Serialize@ - Serialization of Pattern Subject to gram notation
-- * @Gram.Parse@ - Parsing gram notation to Pattern Subject
-- * @Gram.Validate@ - Validation of parsed Gram AST for semantic correctness
--
-- == Usage
--
-- Import the main Gram module to access all functionality:
--
-- >>> import Gram
-- >>> import Pattern.Core (Pattern(..))
-- >>> import Subject.Core (Subject(..), Symbol(..))
-- >>> import Data.Set (Set)
-- >>> import qualified Data.Set as Set
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
-- >>> let p = Pattern { value = s, elements = [] }
-- >>> toGram p
-- "(n:Person)"
--
-- All public functions and types from Gram.Serialize, Gram.Parse, and
-- Gram.Validate are available through this module. See individual module
-- documentation for detailed information about specific functionality.
--
-- == Root record and first-pattern
--
-- @fromGram@ and @toGram@ preserve a leading bare record as the first pattern
-- (anonymous identity, no labels, no elements). @fromGramWithHeader@ and
-- @toGramWithHeader@ keep the header as @Maybe PropertyRecord@ / @PropertyRecord@
-- separate from the pattern list.
--
-- == Re-export Structure
--
-- This module re-exports:
--
-- * All public exports from @Gram.Serialize@ (toGram, etc.)
-- * All public exports from @Gram.Parse@ (fromGram, ParseError, etc.)
-- * All public exports from @Gram.Validate@ (validate, ValidationError, etc.)
--
-- Internal implementation details and helper functions are not exported through
-- this module, ensuring a clean public API.
module Gram
  ( -- * Parsing
    fromGram
  , fromGramWithIds
  , fromGramWithHeader
  , ParseError(..)
    -- * Serialization
  , toGram
  , toGramWithHeader
  , serializePattern
    -- * Validation
  , validate
  , ValidationError(..)
  ) where

import Gram.Serialize (toGram, toGramWithHeader, serializePattern)
import Gram.Parse (fromGram, fromGramWithIds, fromGramWithHeader, ParseError(..))
import Gram.Validate (validate, ValidationError(..))

