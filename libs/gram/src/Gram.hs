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
  ( module Gram.Serialize
  , module Gram.Parse
  , module Gram.Validate
  ) where

import Gram.Serialize
import Gram.Parse
import Gram.Validate

