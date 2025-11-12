# Quickstart: Functor Instance for Pattern

**Feature**: 005-functor-instance  
**Date**: 2025-01-28

## Overview

This quickstart guide demonstrates how to use the Functor instance for `Pattern` to transform values while preserving pattern structure. The Functor instance enables you to apply functions to all values in a pattern without manually traversing the structure.

---

## Installation

The Functor instance is part of the `pattern` package. Once the project is built:

```bash
# Using Cabal
cabal install pattern

# Or add to your project's dependencies
# In your .cabal file:
build-depends: pattern >= 0.1.0.0
```

---

## Basic Usage

### Importing

```haskell
import Pattern
-- Functor instance is automatically available via the Pattern module
```

### Transforming Atomic Patterns

```haskell
-- Transform a string value to uppercase
let p = pattern "hello"
    p' = fmap (map toUpper) p
-- p' = pattern "HELLO"

-- Transform an integer value
let p = pattern 5
    p' = fmap (* 2) p
-- p' = pattern 10

-- Transform using a custom function
let p = pattern "test"
    p' = fmap reverse p
-- p' = pattern "tset"
```

### Transforming Patterns with Elements

```haskell
-- Transform all values in a pattern with multiple elements
let p = patternWith "group" [pattern "a", pattern "b", pattern "c"]
    p' = fmap (map toUpper) p
-- p' = patternWith "GROUP" [pattern "A", pattern "B", pattern "C"]

-- Transform integer values
let p = patternWith 0 [pattern 1, pattern 2, pattern 3]
    p' = fmap (* 2) p
-- p' = patternWith 0 [pattern 2, pattern 4, pattern 6]
```

### Transforming Nested Patterns

```haskell
-- Transform values at all nesting levels
let p = patternWith "outer"
      [ patternWith "inner1" [pattern "a", pattern "b"]
      , patternWith "inner2" [pattern "c"]
      ]
    p' = fmap (map toUpper) p
-- p' = patternWith "OUTER"
--        [ patternWith "INNER1" [pattern "A", pattern "B"]
--        , patternWith "INNER2" [pattern "C"]
--        ]
```

---

## Common Patterns

### Type Conversion

```haskell
-- String to Int (using length)
let p = pattern "hello"
    p' = fmap length p
-- p' :: Pattern Int
-- p' = pattern 5

-- Int to String (using show)
let p = pattern 42
    p' = fmap show p
-- p' :: Pattern String
-- p' = pattern "42"

-- Custom type to String (extracting a field)
data Person = Person { name :: String, age :: Maybe Int }
let p = pattern (Person "Alice" (Just 30))
    p' = fmap name p
-- p' :: Pattern String
-- p' = pattern "Alice"
```

### Field Extraction

```haskell
-- Extract a field from custom types
data Person = Person { name :: String, age :: Maybe Int }
let p = patternWith (Person "Team" Nothing)
      [ pattern (Person "Alice" (Just 30))
      , pattern (Person "Bob" (Just 25))
      ]
    names = fmap name p
-- names = patternWith "Team" [pattern "Alice", pattern "Bob"]
```

### Value Transformation

```haskell
-- Apply a transformation to all values
let p = patternWith "numbers" [pattern 1, pattern 2, pattern 3]
    doubled = fmap (* 2) p
-- doubled = patternWith "numbers" [pattern 2, pattern 4, pattern 6]

-- Apply string operations
let p = patternWith "words" [pattern "hello", pattern "world"]
    uppercased = fmap (map toUpper) p
-- uppercased = patternWith "words" [pattern "HELLO", pattern "WORLD"]
```

### Constant Replacement

```haskell
-- Replace all values with a constant using (<$)
let p = patternWith "original" [pattern "a", pattern "b"]
    constant = "replaced" <$ p
-- constant = patternWith "replaced" [pattern "replaced", pattern "replaced"]
```

---

## Functor Laws

The Functor instance satisfies two mathematical laws:

### Identity Law

```haskell
-- fmap id = id
let p = patternWith "test" [pattern "a", pattern "b"]
    p' = fmap id p
-- p' == p  -- True
```

### Composition Law

```haskell
-- fmap (f . g) = fmap f . fmap g
let p = pattern "hello"
    f = map toUpper
    g = reverse
    result1 = fmap (f . g) p
    result2 = (fmap f . fmap g) p
-- result1 == result2  -- True
```

---

## Real-World Examples

### Processing Pattern Data

```haskell
-- Transform a pattern containing person data
data Person = Person { name :: String, age :: Maybe Int }

let team = patternWith (Person "Team" Nothing)
      [ pattern (Person "Alice" (Just 30))
      , pattern (Person "Bob" (Just 25))
      , pattern (Person "Charlie" (Just 35))
      ]

-- Extract all names
let names = fmap name team
-- names = patternWith "Team" [pattern "Alice", pattern "Bob", pattern "Charlie"]

-- Transform ages (multiply by 2, if present)
let doubledAges = fmap (\p -> p { age = fmap (* 2) (age p) }) team
-- doubledAges = patternWith (Person "Team" Nothing)
--                 [ pattern (Person "Alice" (Just 60))
--                 , pattern (Person "Bob" (Just 50))
--                 , pattern (Person "Charlie" (Just 70))
--                 ]
```

### Nested Pattern Transformation

```haskell
-- Transform a deeply nested pattern structure
let hierarchy = patternWith "company"
      [ patternWith "department1"
          [ patternWith "team1" [pattern "Alice", pattern "Bob"]
          , patternWith "team2" [pattern "Charlie"]
          ]
      , patternWith "department2"
          [ patternWith "team3" [pattern "David", pattern "Eve"]
          ]
      ]

-- Transform all string values to uppercase
let uppercased = fmap (map toUpper) hierarchy
-- All values at all levels are now uppercase
```

### Type-Safe Transformations

```haskell
-- Transform from one type to another safely
let stringPattern = patternWith "numbers"
      [ pattern "1"
      , pattern "2"
      , pattern "3"
      ]

-- Convert strings to integers
let intPattern = fmap read stringPattern :: Pattern Int
-- intPattern = patternWith 0 [pattern 1, pattern 2, pattern 3]
-- (Note: read requires proper error handling in production code)
```

---

## Structure Preservation

The Functor instance preserves pattern structure:

```haskell
-- Element count is preserved
let p = patternWith "group" [pattern "a", pattern "b", pattern "c"]
    p' = fmap (map toUpper) p
-- length (elements p) == length (elements p')  -- True (both are 3)

-- Nesting depth is preserved
let p = patternWith "outer" [patternWith "inner" [pattern "value"]]
    p' = fmap (map toUpper) p
-- Both p and p' have the same nesting depth (2 levels)

-- Element order is preserved
let p = patternWith "list" [pattern "first", pattern "second", pattern "third"]
    p' = fmap (map toUpper) p
-- The order of elements in p' matches the order in p
```

---

## Edge Cases

### Atomic Patterns

```haskell
-- Atomic patterns (no elements) are transformed correctly
let p = pattern "value"
    p' = fmap (map toUpper) p
-- p' = pattern "VALUE"
-- Structure preserved: still atomic (elements = [])
```

### Empty Elements

```haskell
-- Patterns with empty elements list
let p = patternWith "decoration" []
    p' = fmap (map toUpper) p
-- p' = patternWith "DECORATION" []
-- Structure preserved: still has empty elements
```

### Singular Patterns

```haskell
-- Singular patterns (one element)
let p = patternWith "role" [pattern "person"]
    p' = fmap (map toUpper) p
-- p' = patternWith "ROLE" [pattern "PERSON"]
-- Structure preserved: still singular (one element)
```

### Deep Nesting

```haskell
-- Deeply nested patterns (3+ levels)
let p = patternWith "level1"
      [ patternWith "level2"
          [ patternWith "level3" [pattern "value"]
          ]
      ]
    p' = fmap (map toUpper) p
-- All values at all levels are transformed
-- Structure preserved: same nesting depth
```

---

## Best Practices

### Use Type Annotations

```haskell
-- When transforming types, use type annotations for clarity
let p = pattern "42"
    p' = fmap read p :: Pattern Int
-- Type annotation makes the transformation clear
```

### Compose Transformations

```haskell
-- Compose multiple transformations using functor laws
let p = pattern "hello"
    -- Instead of: fmap (map toUpper . reverse) p
    -- Use composition: (fmap (map toUpper) . fmap reverse) p
    p' = (fmap (map toUpper) . fmap reverse) p
-- p' = pattern "OLLEH"
```

### Preserve Structure

```haskell
-- Remember that fmap preserves structure
-- If you need to change structure, use other operations
-- (not part of Functor instance)
```

---

## Next Steps

- Learn about the Foldable instance (Feature 5) for folding over pattern values
- Learn about the Traversable instance (Feature 6) for effectful traversals
- Explore pattern views and morphisms for interpreting patterns as graphs

---

## See Also

- [Data Model](./data-model.md) - Detailed data model documentation
- [Type Signatures](./contracts/type-signatures.md) - Complete API reference
- [Feature Specification](./spec.md) - Full feature specification

