# Type Signatures: Functor Instance for Pattern

**Feature**: 005-functor-instance  
**Date**: 2025-01-28

## Overview

This document defines the public API type signatures for the Functor instance for `Pattern`. The Functor instance enables value transformation while preserving pattern structure.

---

## Core Module: Pattern.Core

### Functor Instance

```haskell
-- | Functor instance for Pattern that enables value transformation
-- while preserving pattern structure.
--
-- The Functor instance satisfies the functor laws:
--
-- * Identity law: @fmap id = id@
-- * Composition law: @fmap (f . g) = fmap f . fmap g@
--
-- Examples:
--
-- >>> let p = pattern "hello"
-- >>> fmap (map toUpper) p
-- Pattern { value = "HELLO", elements = [] }
--
-- >>> let p = patternWith "group" [pattern "a", pattern "b"]
-- >>> fmap (map toUpper) p
-- Pattern { value = "GROUP", elements = [Pattern { value = "A", elements = [] }, Pattern { value = "B", elements = [] }] }
--
-- >>> let p = patternWith "outer" [patternWith "inner" [pattern "value"]]
-- >>> fmap (map toUpper) p
-- Pattern { value = "OUTER", elements = [Pattern { value = "INNER", elements = [Pattern { value = "VALUE", elements = [] }] }] }
instance Functor Pattern where
  fmap :: (a -> b) -> Pattern a -> Pattern b
  fmap f (Pattern v es) = Pattern (f v) (map (fmap f) es)
```

### Functor Methods

#### fmap

```haskell
-- | Transform values in a pattern while preserving structure.
--
-- The function is applied to all values in the pattern structure:
-- the decoration value and all values in element patterns (recursively).
-- The pattern structure (element count, nesting depth, element order)
-- remains unchanged.
--
-- Examples:
--
-- >>> fmap (map toUpper) (pattern "test")
-- Pattern { value = "TEST", elements = [] }
--
-- >>> fmap (* 2) (pattern 5)
-- Pattern { value = 10, elements = [] }
--
-- >>> fmap length (pattern "hello")
-- Pattern { value = 5, elements = [] }
--
-- >>> let p = patternWith "group" [pattern "a", pattern "b"]
-- >>> fmap (map toUpper) p
-- Pattern { value = "GROUP", elements = [Pattern { value = "A", elements = [] }, Pattern { value = "B", elements = [] }] }
--
-- >>> let p = patternWith "outer" [patternWith "inner" [pattern "value"]]
-- >>> fmap (map toUpper) p
-- Pattern { value = "OUTER", elements = [Pattern { value = "INNER", elements = [Pattern { value = "VALUE", elements = [] }] }] }
fmap :: (a -> b) -> Pattern a -> Pattern b
```

**Type Signature**: `(a -> b) -> Pattern a -> Pattern b`

**Behavior**:
- Applies the transformation function to the decoration value
- Recursively applies the transformation to all element patterns
- Preserves pattern structure (element count, nesting depth, element order)

**Functor Laws**:
- **Identity**: `fmap id = id`
- **Composition**: `fmap (f . g) = fmap f . fmap g`

#### (<$)

```haskell
-- | Replace all values in a pattern with a constant value.
--
-- This is the default implementation provided by the Functor typeclass.
-- It preserves pattern structure while replacing all values.
--
-- Example:
--
-- >>> "constant" <$ pattern "original"
-- Pattern { value = "constant", elements = [] }
--
-- >>> let p = patternWith "original" [pattern "a", pattern "b"]
-- >>> "constant" <$ p
-- Pattern { value = "constant", elements = [Pattern { value = "constant", elements = [] }, Pattern { value = "constant", elements = [] }] }
(<$) :: a -> Pattern b -> Pattern a
```

**Type Signature**: `a -> Pattern b -> Pattern a`

**Behavior**:
- Replaces all values in the pattern structure with the constant value
- Preserves pattern structure
- Default implementation: `(<$) = fmap . const`

---

## Module Exports

The `Pattern.Core` module exports:

- `Functor` instance for `Pattern` (via `fmap` and `(<$)`)

The main `Pattern` module re-exports the Functor instance.

---

## Type Safety Guarantees

1. **Type transformation**: The Functor instance allows safe transformation from `Pattern a` to `Pattern b` via any function `a -> b`
2. **Structure preservation**: Pattern structure (element count, nesting depth, element order) is guaranteed to remain unchanged
3. **Functor laws**: The implementation satisfies functor laws (verified by property-based tests)
4. **Recursive application**: Transformation is applied recursively to all nested patterns

---

## Usage Examples

### Basic Value Transformation

```haskell
-- Transform string values to uppercase
let p = pattern "hello"
    p' = fmap (map toUpper) p
-- p' = pattern "HELLO"

-- Transform integer values
let p = pattern 5
    p' = fmap (* 2) p
-- p' = pattern 10

-- Transform custom type values
data Person = Person { name :: String, age :: Maybe Int }
let p = pattern (Person "Alice" (Just 30))
    p' = fmap (\person -> person { name = map toUpper (name person) }) p
-- p' = pattern (Person "ALICE" (Just 30))
```

### Transforming Patterns with Elements

```haskell
-- Transform all values in a pattern with elements
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

### Type Transformation

```haskell
-- String to Int
let p = pattern "hello"
    p' = fmap length p
-- p' :: Pattern Int
-- p' = pattern 5

-- Int to String
let p = pattern 42
    p' = fmap show p
-- p' :: Pattern String
-- p' = pattern "42"

-- Custom type to String
data Person = Person { name :: String, age :: Maybe Int }
let p = pattern (Person "Alice" (Just 30))
    p' = fmap name p
-- p' :: Pattern String
-- p' = pattern "Alice"
```

### Functor Laws

```haskell
-- Identity law: fmap id = id
let p = patternWith "test" [pattern "a", pattern "b"]
    p' = fmap id p
-- p' == p  -- True

-- Composition law: fmap (f . g) = fmap f . fmap g
let p = pattern "hello"
    f = map toUpper
    g = reverse
    result1 = fmap (f . g) p
    result2 = (fmap f . fmap g) p
-- result1 == result2  -- True
```

### Using (<$) for Constant Replacement

```haskell
-- Replace all values with a constant
let p = patternWith "original" [pattern "a", pattern "b"]
    p' = "constant" <$ p
-- p' = patternWith "constant" [pattern "constant", pattern "constant"]
```

---

## Functor Laws Verification

The Functor instance must satisfy the following laws (verified by property-based tests):

### Identity Law

```haskell
fmap id = id
```

**Test**: For any pattern `p`, `fmap id p == p`

### Composition Law

```haskell
fmap (f . g) = fmap f . fmap g
```

**Test**: For any pattern `p` and functions `f` and `g`, `fmap (f . g) p == (fmap f . fmap g) p`

---

## Future Extensions

This phase defines only the Functor instance. Future phases will add:

- Foldable instance (Feature 5) - for folding over pattern values
- Traversable instance (Feature 6) - for effectful traversals
- Additional transformation utilities built on Functor

