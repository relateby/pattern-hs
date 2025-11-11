# Type Signatures: Construction Functions

**Feature**: 004-construction-functions  
**Date**: 2025-01-28

## Overview

This document defines the public API type signatures for the pattern constructor functions. These functions provide a convenient way to create patterns without using verbose record syntax.

---

## Core Module: Pattern.Core

### Constructor Functions

#### pattern

```haskell
-- | Create an atomic pattern (pattern with no elements) from a value.
--
-- This function provides a convenient way to create atomic patterns without
-- using verbose record syntax. The resulting pattern is functionally identical
-- to one created with @Pattern { value = x, elements = [] }@.
--
-- === Examples
--
-- Create an atomic pattern with a string value:
--
-- >>> atom = pattern "atom1"
-- >>> value atom
-- "atom1"
-- >>> elements atom
-- []
--
-- Create an atomic pattern with an integer value:
--
-- >>> num = pattern 42
-- >>> value num
-- 42
--
-- Create an atomic pattern with a custom type:
--
-- >>> data Person = Person { name :: String, age :: Maybe Int }
-- >>> person = pattern (Person "Alice" (Just 30))
-- >>> value person
-- Person {name = "Alice", age = Just 30}
--
-- === Functional Equivalence
--
-- The following are equivalent:
--
-- >>> pattern "test" == Pattern { value = "test", elements = [] }
-- True
pattern :: v -> Pattern v
```

#### patternWith

```haskell
-- | Create a pattern with elements from a value and a list of pattern elements.
--
-- This function provides a convenient way to create patterns with elements
-- without using verbose record syntax. The resulting pattern is functionally
-- identical to one created with @Pattern { value = x, elements = ps }@.
--
-- The function preserves the order of elements in the input list and handles
-- all element counts: 0 (atomic pattern), 1 (singular pattern), 2 (pair),
-- or many (extended pattern).
--
-- === Examples
--
-- Create a singular pattern (one element):
--
-- >>> singular = patternWith "soccer" [pattern "a team sport involving kicking a ball"]
-- >>> length (elements singular)
-- 1
--
-- Create a pair pattern (two elements):
--
-- >>> pair = patternWith "knows" [pattern "Alice", pattern "Bob"]
-- >>> length (elements pair)
-- 2
--
-- Create an extended pattern (many elements):
--
-- >>> extended = patternWith "graph" [pattern "elem1", pattern "elem2", pattern "elem3"]
-- >>> length (elements extended)
-- 3
--
-- Empty list produces atomic pattern:
--
-- >>> atomic = patternWith "empty" []
-- >>> elements atomic
-- []
-- >>> atomic == pattern "empty"
-- True
--
-- === Functional Equivalence
--
-- The following are equivalent:
--
-- >>> patternWith "test" [pattern "elem"] == Pattern { value = "test", elements = [Pattern { value = "elem", elements = [] }] }
-- True
--
-- === Element Order
--
-- Element order is preserved:
--
-- >>> p1 = patternWith "seq" [pattern "a", pattern "b"]
-- >>> p2 = patternWith "seq" [pattern "b", pattern "a"]
-- >>> p1 == p2
-- False
patternWith :: v -> [Pattern v] -> Pattern v
```

#### fromList

```haskell
-- | Create a pattern from a list of values by converting each value to an atomic pattern.
--
-- This function provides a convenient way to create patterns from lists of raw values.
-- Each value in the list is automatically converted to an atomic pattern, then all
-- atomic patterns are combined into a single pattern with the given decoration.
--
-- The function preserves the order of values in the input list and handles all
-- element counts: 0 (atomic pattern), 1 (singular pattern), 2 (pair), or many
-- (extended pattern).
--
-- === Examples
--
-- Create a pattern from a list of strings:
--
-- >>> p = fromList "graph" ["Alice", "Bob", "Charlie"]
-- >>> value p
-- "graph"
-- >>> length (elements p)
-- 3
-- >>> map value (elements p)
-- ["Alice", "Bob", "Charlie"]
--
-- Create a pattern from a list of integers:
--
-- >>> nums = fromList "numbers" [1, 2, 3, 4, 5]
-- >>> length (elements nums)
-- 5
--
-- Empty list produces atomic pattern:
--
-- >>> atomic = fromList "empty" []
-- >>> elements atomic
-- []
-- >>> atomic == pattern "empty"
-- True
--
-- === Functional Equivalence
--
-- The following are equivalent:
--
-- >>> fromList "test" ["a", "b"] == patternWith "test" [pattern "a", pattern "b"]
-- True
--
-- === Implementation
--
-- This function is implemented as:
--
-- @
-- fromList decoration values = patternWith decoration (map pattern values)
-- @
fromList :: v -> [v] -> Pattern v
```

---

## Module Exports

The `Pattern.Core` module exports:

- `pattern` (constructor function for atomic patterns)
- `patternWith` (constructor function for patterns with elements)
- `fromList` (constructor function for patterns from lists of values)

These are in addition to existing exports:
- `Pattern` (data type and constructor)
- `value` (field accessor)
- `elements` (field accessor)

The main `Pattern` module re-exports these functions for convenience.

---

## Type Safety Guarantees

1. **Type consistency**: Both functions preserve the type parameter `v`, ensuring all patterns in a structure share the same value type
2. **Type inference**: Functions work with any type `v` that can be used in Pattern
3. **Element type matching**: `patternWith` requires all elements to be `Pattern v` where `v` matches the value type

---

## Usage Examples

### Creating Atomic Patterns

```haskell
import Pattern.Core (pattern)

-- String pattern
atom1 :: Pattern String
atom1 = pattern "atom1"

-- Integer pattern
atom2 :: Pattern Int
atom2 = pattern 42

-- Custom type pattern
data Person = Person { name :: String, age :: Maybe Int }
personPattern :: Pattern Person
personPattern = pattern (Person "Alice" (Just 30))
```

### Creating Patterns with Elements

```haskell
import Pattern.Core (pattern, patternWith)

-- Singular pattern (one element)
singular :: Pattern String
singular = patternWith "soccer" [pattern "a team sport involving kicking a ball"]

-- Role-based singular pattern with custom type
data Person = Person { name :: String, age :: Maybe Int }
goalie :: Pattern Person
goalie = patternWith (Person "Goalie" Nothing) [pattern (Person "Hans" (Just 25))]

-- Pair pattern (two elements)
pair :: Pattern String
pair = patternWith "knows" [pattern "Alice", pattern "Bob"]

-- Extended pattern (many elements)
extended :: Pattern String
extended = patternWith "graph" 
  [ pattern "elem1"
  , pattern "elem2"
  , pattern "elem3"
  ]

-- Nested patterns
nested :: Pattern String
nested = patternWith "outer" 
  [ patternWith "middle" 
      [ patternWith "inner" 
          [ pattern "innermost" ]
      ]
  ]
```

### Functional Equivalence Verification

```haskell
-- Verify equivalence with record syntax
testEquivalence :: Bool
testEquivalence = 
  pattern "test" == Pattern { value = "test", elements = [] }

testEquivalenceWith :: Bool
testEquivalenceWith = 
  patternWith "soccer" [pattern "a team sport involving kicking a ball"] == 
    Pattern { value = "soccer", elements = [Pattern { value = "a team sport involving kicking a ball", elements = [] }] }
```

---

## Relationship to Pattern Data Type

These constructor functions are convenience wrappers:

- `pattern v` = `Pattern { value = v, elements = [] }`
- `patternWith v ps` = `Pattern { value = v, elements = ps }`

They provide no additional functionality beyond convenience; all behavior is identical to record syntax.

---

## Implementation Status

- ✅ Type signatures defined
- ⏳ Implementation planned
- ⏳ Tests planned
- ⏳ Documentation planned

