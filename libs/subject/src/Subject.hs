-- | Main module for the Subject library.
--
-- This module provides convenient access to all Subject library functionality
-- by re-exporting public APIs from core modules. Import this module to access
-- all Subject types, functions, and typeclass instances without needing to
-- import individual modules.
--
-- == Library Organization
--
-- The Subject library is organized into several modules:
--
-- * @Subject.Core@ - Core Subject data type, Symbol type, PropertyRecord type,
--   and typeclass instances (Show, Eq, Ord, Hashable, Semigroup, Monoid)
-- * @Subject.Value@ - Value type system supporting standard and extended types
-- * @Subject.Construction@ - Constructor functions and property manipulation
--
-- == Usage
--
-- Import the main Subject module to access all functionality:
--
-- >>> import Subject
-- >>> import Data.Map (fromList, empty)
-- >>> import Data.Set (Set)
-- >>> import qualified Data.Set as Set
-- >>> import Subject.Value (VString)
-- >>> let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
--
-- All public functions, types, and typeclass instances from Subject.Core and
-- Subject.Value are available through this module. See individual module
-- documentation for detailed information about specific functionality.
--
-- == Re-export Structure
--
-- This module re-exports:
--
-- * All public exports from @Subject.Core@ (Subject type, Symbol type,
--   PropertyRecord type, and all typeclass instances)
-- * All public exports from @Subject.Value@ (Value type, RangeValue type)
--
-- Internal implementation details and helper functions are not exported through
-- this module, ensuring a clean public API.
module Subject
  ( module Subject.Core
  , module Subject.Value
  , module Subject.Construction
  ) where

import Subject.Core
import Subject.Construction
import Subject.Value

