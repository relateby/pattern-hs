# Type Signatures: Basic Pattern Type

**Feature**: 002-basic-pattern-type  
**Date**: 2025-01-27

## Overview

This document defines the public API type signatures for the basic Pattern type. For a library project, these serve as the "contracts" that define the interface users will interact with.

---

## Core Module: Pattern.Core

### Pattern Data Type

```haskell
-- | A decorated sequence that stores a value and contains zero or more
-- Pattern elements.
--
-- Patterns form the foundation for representing graph elements. Each pattern
-- can have an associated value of any type, and elements form the pattern
-- sequence itself.
--
-- Examples:
--
-- >>> leaf = Pattern { value = "node1", elements = [] }
-- >>> parent = Pattern { value = "parent", elements = [leaf] }
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
```

### Field Accessors

```haskell
-- | Extract the value stored in a pattern.
--
-- >>> value (Pattern { value = "test", elements = [] })
-- "test"
value :: Pattern v -> v

-- | Extract the list of pattern elements.
--
-- >>> elements (Pattern { value = "parent", elements = [] })
-- []
-- >>> elements (Pattern { value = "parent", elements = [leaf] })
-- [Pattern { value = "node1", elements = [] }]
elements :: Pattern v -> [Pattern v]
```

### Pattern Construction

Patterns can be constructed directly using the `Pattern` constructor:

```haskell
-- | Construct an empty pattern (pattern with no elements).
--
-- Example:
-- >>> leaf = Pattern { value = "node1", elements = [] }
Pattern :: v -> [Pattern v] -> Pattern v
```

Note: Constructor functions (`pattern` and `patternWith`) will be added in Phase 3 of the TODO.md plan. For this phase, patterns are constructed directly using the data constructor.

---

## Module Exports

The `Pattern.Core` module exports:

- `Pattern` (data type and constructor)
- `value` (field accessor)
- `elements` (field accessor)

---

## Type Safety Guarantees

1. **Type consistency**: All patterns in a structure must share the same value type `v` (enforced by type system)
2. **Non-nullable**: Field accessors always return valid values (no null/undefined)
3. **Finite structures**: Patterns must be finite (no infinite recursion in the data structure itself)

---

## Usage Examples

### Creating Empty Patterns

```haskell
-- String pattern
leaf1 :: Pattern String
leaf1 = Pattern { value = "node1", elements = [] }

-- Integer pattern
leaf2 :: Pattern Int
leaf2 = Pattern { value = 42, elements = [] }

-- Custom type pattern
data Person = Person { name :: String, age :: Int }
personPattern :: Pattern Person
personPattern = Pattern { value = Person "Alice" 30, elements = [] }
```

### Creating Patterns with Elements

```haskell
-- Pattern with two elements
parent :: Pattern String
parent = Pattern 
  { value = "parent"
  , elements = [ Pattern { value = "elem1", elements = [] }
               , Pattern { value = "elem2", elements = [] }
               ]
  }

-- Nested patterns
nested :: Pattern String
nested = Pattern 
  { value = "root"
  , elements = [ Pattern { value = "level1"
                         , elements = [ Pattern { value = "level2", elements = [] } ]
                         }
               ]
  }
```

### Inspecting Pattern Structure

```haskell
-- Extract value
getValue :: Pattern String -> String
getValue p = value p

-- Extract elements
getElements :: Pattern String -> [Pattern String]
getElements p = elements p

-- Check if leaf
isLeaf :: Pattern v -> Bool
isLeaf p = null (elements p)
```

---

## Future Extensions

This phase defines only the basic type and field accessors. Future phases will add:

- Typeclass instances (Functor, Foldable, Traversable) - Phase 4-6
- Constructor functions (`pattern`, `patternWith`) - Phase 3
- Classification functions (`isNode`, `isRelationship`, etc.) - Phase 8-11
- Navigation functions (`source`, `target`, `nodes`, etc.) - Phase 12

