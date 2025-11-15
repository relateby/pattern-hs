-- | Main module for the Pattern library.
--
-- This module provides convenient access to all Pattern library functionality
-- by re-exporting public APIs from core modules. Import this module to access
-- all Pattern types, functions, and typeclass instances without needing to
-- import individual modules.
--
-- == Library Organization
--
-- The Pattern library is organized into several modules:
--
-- * @Pattern.Core@ - Core Pattern data type, construction functions, query functions,
--   predicate functions, and typeclass instances (Functor, Applicative, Comonad, etc.)
-- * @Pattern.Views@ - Graph view interpretations for different graph semantics
-- * @Pattern.Graph@ - Graph operations and transformations
-- * @Pattern.Morphisms@ - Pattern morphisms and transformations
--
-- == Usage
--
-- Import the main Pattern module to access all functionality:
--
-- >>> import Pattern
-- >>> let p = pattern "test"
-- >>> value p
-- "test"
--
-- All public functions, types, and typeclass instances from Pattern.Core are
-- available through this module. See individual module documentation for
-- detailed information about specific functionality.
--
-- == Re-export Structure
--
-- This module re-exports:
--
-- * All public exports from @Pattern.Core@ (Pattern type, construction functions,
--   query functions, predicate functions, helper functions, and all typeclass instances)
-- * All public exports from @Pattern.Views@ (graph view interpretations)
-- * All public exports from @Pattern.Graph@ (graph operations)
-- * All public exports from @Pattern.Morphisms@ (pattern morphisms)
--
-- Internal implementation details and helper functions are not exported through
-- this module, ensuring a clean public API.
module Pattern
  ( module Pattern.Core
  , module Pattern.Views
  , module Pattern.Graph
  , module Pattern.Morphisms
  ) where

import Pattern.Core
import Pattern.Views
import Pattern.Graph
import Pattern.Morphisms

