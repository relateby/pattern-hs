# Type Signatures: Basic Pattern Type

**Feature**: 002-basic-pattern-type  
**Date**: 2025-01-27

## Overview

This document defines the public API type signatures for the basic Pattern type. For a library project, these serve as the "contracts" that define the interface users will interact with.

---

## Core Module: Pattern.Core

### Pattern Data Type

```haskell
-- | A recursive tree structure that stores a value and contains zero or more
-- child Pattern instances.
--
-- Patterns form the foundation for representing graph elements. Each pattern
-- node can have an associated value of any type, and child patterns form a
-- hierarchical tree structure.
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
-- | Extract the value stored in a pattern node.
--
-- >>> value (Pattern { value = "test", elements = [] })
-- "test"
value :: Pattern v -> v

-- | Extract the list of child patterns.
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
-- | Construct a leaf pattern (pattern with no children).
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

### Creating Leaf Patterns

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

### Creating Patterns with Children

```haskell
-- Pattern with two child patterns
parent :: Pattern String
parent = Pattern 
  { value = "parent"
  , elements = [ Pattern { value = "child1", elements = [] }
               , Pattern { value = "child2", elements = [] }
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

-- Extract children
getChildren :: Pattern String -> [Pattern String]
getChildren p = elements p

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

