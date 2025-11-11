# Data Model: Construction Functions

**Feature**: 004-construction-functions  
**Date**: 2025-01-28

## Overview

This feature adds three constructor functions to simplify pattern creation:
- `pattern` - creates atomic patterns (patterns with no elements)
- `patternWith` - creates patterns with elements (1, 2, or more elements)
- `fromList` - creates patterns from a list of values (convenience function)

These functions provide a more convenient API than the verbose record syntax while maintaining full functional equivalence.

## Core Functions

### pattern

**Purpose**: Create an atomic pattern (pattern with no elements) from a single value.

**Type Signature**: `pattern :: v -> Pattern v`

**Behavior**: 
- Takes a value of any type `v`
- Returns a `Pattern v` with that value and an empty elements list
- Functionally equivalent to `Pattern { value = x, elements = [] }`

**Example**:
```haskell
-- Create atomic pattern with string value
atom1 :: Pattern String
atom1 = pattern "atom1"

-- Equivalent to:
atom1' :: Pattern String
atom1' = Pattern { value = "atom1", elements = [] }
```

### patternWith

**Purpose**: Create a pattern with elements from a value and a list of pattern elements.

**Type Signature**: `patternWith :: v -> [Pattern v] -> Pattern v`

**Behavior**:
- Takes a value of any type `v` and a list of `Pattern v` elements
- Returns a `Pattern v` with that value and those elements
- Preserves the order of elements in the input list
- Handles empty lists (produces atomic pattern, equivalent to `pattern`)
- Functionally equivalent to `Pattern { value = x, elements = ps }`

**Example**:
```haskell
-- Create singular pattern (one element)
singular :: Pattern String
singular = patternWith "soccer" [pattern "a team sport involving kicking a ball"]

-- Create pair pattern (two elements)
pair :: Pattern String
pair = patternWith "knows" [pattern "Alice", pattern "Bob"]

-- Create extended pattern (many elements)
extended :: Pattern String
extended = patternWith "graph" 
  [ pattern "elem1"
  , pattern "elem2"
  , pattern "elem3"
  ]

-- Empty list produces atomic pattern
atomic :: Pattern String
atomic = patternWith "empty" []  -- Equivalent to pattern "empty"
```

## Functional Equivalence

Both constructor functions produce patterns that are functionally identical to patterns created with record syntax:

```haskell
-- These are equivalent:
p1 = pattern "test"
p2 = Pattern { value = "test", elements = [] }
-- p1 == p2 is True

-- These are equivalent:
p3 = patternWith "soccer" [pattern "a team sport involving kicking a ball"]
p4 = Pattern { value = "soccer", elements = [Pattern { value = "a team sport involving kicking a ball", elements = [] }] }
-- p3 == p4 is True
```

## Type Safety

Both functions preserve type safety:
- `pattern :: v -> Pattern v` - preserves the type parameter `v`
- `patternWith :: v -> [Pattern v] -> Pattern v` - ensures all patterns share the same type `v`
- Type system enforces that all patterns in a structure share the same value type

## Element Order Preservation

The `patternWith` function preserves the order of elements in the input list:

```haskell
-- Order matters
p1 = patternWith "seq" [pattern "a", pattern "b", pattern "c"]
p2 = patternWith "seq" [pattern "c", pattern "b", pattern "a"]
-- p1 /= p2 (order is different)
```

## Edge Cases

### Empty List in patternWith

When `patternWith` receives an empty list, it produces an atomic pattern:

```haskell
-- These are equivalent:
p1 = patternWith "test" []
p2 = pattern "test"
-- p1 == p2 is True
```

### Nested Patterns

Both functions work correctly with nested patterns:

```haskell
-- Nested construction
nested :: Pattern String
nested = patternWith "outer" 
  [ patternWith "middle" 
      [ patternWith "inner" 
          [ pattern "innermost" ]
      ]
  ]
```

### All Value Types

Both functions work with any value type:

```haskell
-- String values
strPattern :: Pattern String
strPattern = pattern "text"

-- Integer values
intPattern :: Pattern Int
intPattern = pattern 42

-- Custom types
data Person = Person { name :: String, age :: Maybe Int }
personPattern :: Pattern Person
personPattern = pattern (Person "Alice" (Just 30))
```

## Relationship to Pattern Data Type

These constructor functions are convenience wrappers around the Pattern data type:

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
```

- `pattern v` = `Pattern { value = v, elements = [] }`
- `patternWith v ps` = `Pattern { value = v, elements = ps }`

The functions don't change the underlying data structure; they simply provide a more convenient API.

### fromList

**Purpose**: Create a pattern from a list of values by converting each value to an atomic pattern.

**Type Signature**: `fromList :: v -> [v] -> Pattern v`

**Behavior**:
- Takes a decoration value of type `v` and a list of values `[v]`
- Converts each value in the list to an atomic pattern using `pattern`
- Creates a pattern with those atomic patterns as elements using `patternWith`
- Preserves the order of values in the input list
- Handles empty lists (produces atomic pattern, equivalent to `pattern`)
- Functionally equivalent to: `fromList decoration values = patternWith decoration (map pattern values)`

**Example**:
```haskell
-- Convert list of strings to pattern
p :: Pattern String
p = fromList "graph" ["Alice", "Bob", "Charlie"]

-- Equivalent to:
p' :: Pattern String
p' = patternWith "graph" [pattern "Alice", pattern "Bob", pattern "Charlie"]

-- Empty list produces atomic pattern
atomic :: Pattern String
atomic = fromList "empty" []  -- Equivalent to pattern "empty"
```

**Relationship to Other Functions**:
- `fromList decoration values` = `patternWith decoration (map pattern values)`
- More convenient than manually mapping `pattern` over a list
- Useful when you have raw data (list of values) rather than existing patterns

## Implementation Status

- ✅ Design complete
- ⏳ Implementation planned
- ⏳ Tests planned
- ⏳ Documentation planned

