# Data Model: Functor Instance for Pattern

**Feature**: 005-functor-instance  
**Date**: 2025-01-28

## Overview

The Functor instance for `Pattern` enables value transformation while preserving pattern structure. This is a fundamental category theory concept that allows transforming values in patterns without manually traversing and reconstructing the pattern structure.

## Core Entity: Functor Instance

### Definition

A **Functor** is a typeclass that represents a structure-preserving transformation. For `Pattern`, the Functor instance allows transforming values of type `v` to values of type `w` while preserving the pattern structure (element count, nesting depth, element order).

### Typeclass Instance

```haskell
instance Functor Pattern where
  fmap :: (a -> b) -> Pattern a -> Pattern b
  fmap f (Pattern v es) = Pattern (f v) (map (fmap f) es)
```

### Categorical Interpretation

From a category theory perspective, `Pattern` is a functor from the category of types to itself. The functor instance provides:

1. **Structure Preservation**: The pattern structure (element count, nesting depth, element order) remains unchanged during transformation
2. **Value Transformation**: All values in the pattern structure are transformed by the provided function
3. **Recursive Application**: The transformation is applied recursively to all nested patterns

### Functor Laws

The Functor instance must satisfy two mathematical laws:

#### Identity Law

```haskell
fmap id = id
```

**Meaning**: Applying the identity function to a pattern produces the same pattern unchanged.

**Example**:
```haskell
let p = patternWith "test" [pattern "a", pattern "b"]
in fmap id p == p  -- True
```

#### Composition Law

```haskell
fmap (f . g) = fmap f . fmap g
```

**Meaning**: Applying a composition of functions is equivalent to applying each function sequentially.

**Example**:
```haskell
let p = pattern "hello"
    f = map toUpper
    g = reverse
in fmap (f . g) p == (fmap f . fmap g) p  -- True
```

### Structure Preservation

The Functor instance preserves the following pattern structure properties:

1. **Element Count**: The number of elements at each level remains unchanged
2. **Nesting Depth**: The depth of nested patterns remains unchanged
3. **Element Order**: The order of elements in sequences remains unchanged
4. **Pattern Classification**: Atomic patterns remain atomic, patterns with elements remain patterns with elements

### Value Transformation

The Functor instance transforms values at all levels of the pattern structure:

1. **Top-level Value**: The decoration value is transformed
2. **Element Values**: All values in element patterns are transformed
3. **Nested Values**: All values in nested patterns are transformed recursively

### Implementation Details

#### Recursive Transformation

The `fmap` implementation applies the transformation function recursively:

```haskell
fmap f (Pattern v es) = Pattern (f v) (map (fmap f) es)
```

**Breakdown**:
- `f v`: Transform the decoration value at this level
- `map (fmap f) es`: Recursively transform all element patterns

#### Type Transformation

The Functor instance allows changing the value type:

- `Pattern String` → `Pattern Int` (via `fmap length`)
- `Pattern Int` → `Pattern String` (via `fmap show`)
- `Pattern a` → `Pattern b` (via any function `a -> b`)

### Relationships

#### Relationship to Pattern Data Type

- The Functor instance operates on the `Pattern` data type defined in Feature 1
- It transforms values while preserving the pattern structure
- It works with all pattern structures (atomic, with elements, nested)

#### Relationship to Category Theory

- `Pattern` is a functor in the category of Haskell types
- The functor laws ensure categorical correctness
- The implementation follows standard functor patterns for recursive data structures

#### Relationship to Other Typeclass Instances

- **Eq instance** (Feature 2): Required for testing functor laws (equality comparison)
- **Show instance** (Feature 2): Required for test diagnostics
- **Foldable instance** (Feature 5, planned): Will build on Functor for folding operations
- **Traversable instance** (Feature 6, planned): Will build on Functor for effectful traversals

## Pattern Structure Preservation Examples

### Atomic Pattern

```haskell
-- Before: Pattern String
let p = pattern "test"

-- After: Pattern String (uppercase)
let p' = fmap (map toUpper) p
-- p' = pattern "TEST"
-- Structure preserved: still atomic (no elements)
```

### Pattern with Elements

```haskell
-- Before: Pattern String
let p = patternWith "group" [pattern "a", pattern "b", pattern "c"]

-- After: Pattern String (uppercase)
let p' = fmap (map toUpper) p
-- p' = patternWith "GROUP" [pattern "A", pattern "B", pattern "C"]
-- Structure preserved: same number of elements (3), same order
```

### Nested Pattern

```haskell
-- Before: Pattern String
let p = patternWith "outer" 
      [ patternWith "inner1" [pattern "a", pattern "b"]
      , patternWith "inner2" [pattern "c"]
      ]

-- After: Pattern String (uppercase)
let p' = fmap (map toUpper) p
-- p' = patternWith "OUTER"
--        [ patternWith "INNER1" [pattern "A", pattern "B"]
--        , patternWith "INNER2" [pattern "C"]
--        ]
-- Structure preserved: same nesting depth, same element counts, same order
```

## Type Safety

### Type Parameter Consistency

- All values in a pattern structure share the same type before transformation
- All values in a pattern structure share the same type after transformation
- The transformation function must be type-safe (enforced by Haskell's type system)

### Type Transformation

The Functor instance allows safe type transformations:

```haskell
-- String to Int
fmap length :: Pattern String -> Pattern Int

-- Int to String
fmap show :: Pattern Int -> Pattern String

-- Custom type transformations
fmap name :: Pattern Person -> Pattern String
```

## Validation Rules

### Functor Law Validation

The Functor instance must satisfy:

1. **Identity Law**: `fmap id = id` for all patterns
2. **Composition Law**: `fmap (f . g) = fmap f . fmap g` for all patterns and functions

These laws are verified through property-based testing (QuickCheck).

### Structure Preservation Validation

The Functor instance must preserve:

1. Element count at each level
2. Nesting depth
3. Element order
4. Pattern classification (atomic vs. with elements)

These properties are verified through unit tests and property-based tests.

## Edge Cases

### Atomic Pattern (No Elements)

```haskell
let p = pattern "value"
    p' = fmap (map toUpper) p
-- p' = pattern "VALUE"
-- Structure: still atomic (elements = [])
```

### Pattern with Empty Elements

```haskell
let p = patternWith "decoration" []
    p' = fmap (map toUpper) p
-- p' = patternWith "DECORATION" []
-- Structure: still has empty elements (same as atomic)
```

### Singular Pattern (One Element)

```haskell
let p = patternWith "role" [pattern "person"]
    p' = fmap (map toUpper) p
-- p' = patternWith "ROLE" [pattern "PERSON"]
-- Structure: still singular (one element)
```

### Pair Pattern (Two Elements)

```haskell
let p = patternWith "pair" [pattern "a", pattern "b"]
    p' = fmap (map toUpper) p
-- p' = patternWith "PAIR" [pattern "A", pattern "B"]
-- Structure: still a pair (two elements)
```

### Extended Pattern (Many Elements)

```haskell
let p = patternWith "list" [pattern "a", pattern "b", pattern "c", pattern "d"]
    p' = fmap (map toUpper) p
-- p' = patternWith "LIST" [pattern "A", pattern "B", pattern "C", pattern "D"]
-- Structure: same number of elements, same order
```

### Deeply Nested Pattern

```haskell
let p = patternWith "level1"
      [ patternWith "level2"
          [ patternWith "level3" [pattern "value"]
          ]
      ]
    p' = fmap (map toUpper) p
-- p' = patternWith "LEVEL1"
--        [ patternWith "LEVEL2"
--            [ patternWith "LEVEL3" [pattern "VALUE"]
--            ]
--        ]
-- Structure: same nesting depth (3 levels), same element counts
```

## Summary

The Functor instance for `Pattern` provides a standard, mathematically sound way to transform values while preserving pattern structure. It satisfies functor laws and works with all pattern structures, making it a foundational capability for pattern manipulation and composition.

