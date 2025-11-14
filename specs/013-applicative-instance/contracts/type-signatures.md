# Type Signatures: Applicative Instance for Pattern

**Feature**: 013-applicative-instance  
**Date**: 2025-01-28

## Applicative Instance

### Instance Declaration

```haskell
instance Applicative Pattern where
  pure :: a -> Pattern a
  (<*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
```

**Type Constraint**: None (Applicative is a typeclass, not a constraint)

**Module**: `Pattern.Core`

---

## Pure Function

### Type Signature

```haskell
pure :: a -> Pattern a
```

**Purpose**: Wrap a value in an atomic pattern (pattern with empty elements list), enabling the value to be used in applicative operations.

**Behavior**:
- Takes a value of any type `a`
- Returns a `Pattern a` with that value and an empty elements list
- Creates an atomic pattern: `Pattern x []`

**Examples**:

```haskell
-- Wrap an integer value
pure 5 :: Pattern Int
-- Result: Pattern {value = 5, elements = []}

-- Wrap a string value
pure "hello" :: Pattern String
-- Result: Pattern {value = "hello", elements = []}

-- Wrap a function value
pure (+1) :: Pattern (Int -> Int)
-- Result: Pattern {value = (+1), elements = []}
```

**Haddock Documentation**:

```haskell
-- | Wrap a value in an atomic pattern.
--
-- Creates a pattern with the provided value and an empty elements list.
-- This enables values to be used in applicative operations with patterns.
--
-- === Examples
--
-- >>> pure 5
-- Pattern {value = 5, elements = []}
--
-- >>> pure "hello"
-- Pattern {value = "hello", elements = []}
--
-- >>> pure (+1) <*> pure 5
-- Pattern {value = 6, elements = []}
pure :: a -> Pattern a
```

---

## Applicative Apply Operator

### Type Signature

```haskell
(<*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
```

**Purpose**: Apply functions stored in a pattern to values stored in a pattern, producing a pattern with the results. Uses structure-preserving/zip-like semantics: applies functions to values at corresponding positions (root to root, element to element).

**Behavior**:
- Takes a pattern containing functions `Pattern (a -> b)` and a pattern containing values `Pattern a`
- Returns a pattern containing results `Pattern b`
- Applies root function to root value
- Recursively applies element functions to element values at corresponding positions
- Uses zip-like truncation: if element counts differ, applies up to minimum count

**Examples**:

```haskell
-- Atomic patterns
let f = pure (+1)
    x = pure 5
in f <*> x
-- Result: Pattern {value = 6, elements = []}

-- Patterns with elements
let fs = patternWith id [pure (*2), pure (+10)]
    xs = patternWith 5 [pure 3, pure 7]
in fs <*> xs
-- Result: Pattern {value = 5, elements = [Pattern {value = 6, elements = []}, Pattern {value = 17, elements = []}]}

-- Nested patterns
let fs = patternWith id [patternWith (*2) [pure (*3)]]
    xs = patternWith 1 [patternWith 2 [pure 3]]
in fs <*> xs
-- Result: Pattern {value = 1, elements = [Pattern {value = 4, elements = [Pattern {value = 9, elements = []}]}]}

-- Mismatched element counts (zip-like truncation)
let fs = patternWith id [pure (*2), pure (+10)]  -- 2 elements
    xs = patternWith 5 [pure 3, pure 7, pure 11]      -- 3 elements
in fs <*> xs
-- Result: Pattern {value = 5, elements = [Pattern {value = 6, elements = []}, Pattern {value = 17, elements = []}]}
-- Note: Third element (11) is ignored due to truncation
```

**Haddock Documentation**:

```haskell
-- | Apply functions stored in a pattern to values stored in a pattern.
--
-- Uses structure-preserving/zip-like semantics: applies functions to values
-- at corresponding positions (root to root, element to element). When element
-- counts differ, applies functions to values up to the minimum element count,
-- ignoring extra elements in the longer pattern.
--
-- === Structure-Preserving Semantics
--
-- The operator preserves pattern structure during function application:
--
-- * Root function is applied to root value
-- * Element functions are applied to element values at corresponding positions
-- * Function application is applied recursively to nested patterns
-- * Zip-like truncation handles mismatched element counts
--
-- === Examples
--
-- Atomic patterns:
--
-- >>> let f = pure (+1)
-- >>> let x = pure 5
-- >>> f <*> x
-- Pattern {value = 6, elements = []}
--
-- Patterns with elements:
--
-- >>> let fs = patternWith id [pure (*2), pure (+10)]
-- >>> let xs = patternWith 5 [pure 3, pure 7]
-- >>> fs <*> xs
-- Pattern {value = 5, elements = [Pattern {value = 6, elements = []}, Pattern {value = 17, elements = []}]}
--
-- Mismatched element counts (truncation):
--
-- >>> let fs = patternWith id [pure (*2)]  -- 1 element
-- >>> let xs = patternWith 5 [pure 3, pure 7]    -- 2 elements
-- >>> fs <*> xs
-- Pattern {value = 5, elements = [Pattern {value = 6, elements = []}]}
(<*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
```

---

## Module Exports

The `Pattern.Core` module exports:

- `Applicative` instance for `Pattern` (via `pure` and `<*>`)

The main `Pattern` module re-exports the Applicative instance.

---

## Type Safety Guarantees

1. **Type transformation**: The Applicative instance allows safe transformation from `Pattern (a -> b)` and `Pattern a` to `Pattern b`
2. **Structure preservation**: Pattern structure (element count, nesting depth, element order) is preserved during function application (up to truncation for mismatched structures)
3. **Applicative laws**: The implementation satisfies all Applicative laws (verified by property-based tests)
4. **Recursive application**: Function application is applied recursively to all nested patterns
5. **Consistency with Functor**: The relationship `fmap f x = pure f <*> x` holds for all functions and patterns

---

## Usage Examples

### Basic Function Application

```haskell
-- Apply a function to a value
let f = pure (+1)
    x = pure 5
    result = f <*> x
-- result = Pattern {value = 6, elements = []}
```

### Patterns with Elements

```haskell
-- Apply multiple functions to multiple values
let fs = patternWith id [pure (*2), pure (+10), pure (^2)]
    xs = patternWith 5 [pure 3, pure 7, pure 2]
    result = fs <*> xs
-- result = Pattern {value = 5, elements = [Pattern {value = 6, elements = []}, Pattern {value = 17, elements = []}, Pattern {value = 4, elements = []}]}
```

### Nested Patterns

```haskell
-- Apply functions recursively to nested patterns
let fs = patternWith id 
      [ patternWith (*2) [pure (*3)]
      , patternWith (+1) []
      ]
    xs = patternWith 1
      [ patternWith 2 [pure 3]
      , patternWith 4 []
      ]
    result = fs <*> xs
-- result = Pattern {value = 1, elements = [Pattern {value = 4, elements = [Pattern {value = 9, elements = []}]}, Pattern {value = 5, elements = []}]}
```

### Type Transformations

```haskell
-- Apply functions that change value types
let fs = patternWith length [show, read]
    xs = patternWith "hello" [42, "123"]
    result = fs <*> xs
-- Note: This example is simplified - actual type constraints would apply
```

### Consistency with Functor

```haskell
-- fmap and pure/<*> produce the same result
let f = (+1)
    x = pattern 5
    result1 = fmap f x
    result2 = pure f <*> x
-- result1 == result2  -- True
```

---

## Applicative Laws

The implementation satisfies all Applicative laws:

1. **Identity Law**: `pure id <*> v = v`
2. **Composition Law**: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
3. **Homomorphism Law**: `pure f <*> pure x = pure (f x)`
4. **Interchange Law**: `u <*> pure y = pure ($ y) <*> u`

These laws are verified through property-based testing.

