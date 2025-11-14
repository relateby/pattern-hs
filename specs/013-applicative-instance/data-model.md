# Data Model: Applicative Instance for Pattern

**Feature**: 013-applicative-instance  
**Date**: 2025-01-28

## Overview

The Applicative instance for `Pattern` enables applying functions stored in patterns to values stored in patterns using structure-preserving/zip-like semantics. This is a fundamental category theory concept that extends Functor, allowing structured function application where both functions and values are organized as pattern structures.

## Core Entity: Applicative Instance

### Definition

An **Applicative** is a typeclass that represents a structure-preserving way to apply functions stored in a structure to values stored in a structure. For `Pattern`, the Applicative instance allows applying functions of type `a -> b` stored in patterns to values of type `a` stored in patterns, producing patterns of type `b`, while preserving pattern structure.

### Typeclass Instance

```haskell
instance Applicative Pattern where
  pure :: a -> Pattern a
  pure x = Pattern x []
  
  (<*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
  Pattern f fs <*> Pattern x xs = 
    Pattern (f x) (zipWith (<*>) fs xs)
```

### Categorical Interpretation

From a category theory perspective, `Pattern` is an applicative functor that provides a way to apply functions in a structured context. The Applicative instance provides:

1. **Structure Preservation**: The pattern structure (element count, nesting depth, element order) is preserved during function application
2. **Structured Function Application**: Functions stored in patterns are applied to values stored in patterns at corresponding positions
3. **Recursive Application**: Function application is applied recursively to all nested patterns
4. **Zip-like Semantics**: Functions are applied to values at matching positions, with truncation for mismatched structures

### Applicative Laws

The Applicative instance must satisfy four mathematical laws:

#### Identity Law

```haskell
pure id <*> v = v
```

**Meaning**: Applying the identity function to a pattern produces the same pattern unchanged.

**Example**:
```haskell
let p = patternWith "test" [pattern "a", pattern "b"]
in pure id <*> p == p  -- True
```

#### Composition Law

```haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

**Meaning**: Applying a composition of functions is equivalent to applying each function sequentially.

**Example**:
```haskell
let u = pattern (+1)
    v = pattern (*2)
    w = pattern 5
in pure (.) <*> u <*> v <*> w == u <*> (v <*> w)  -- True
```

#### Homomorphism Law

```haskell
pure f <*> pure x = pure (f x)
```

**Meaning**: Applying a pure function to a pure value equals pure application.

**Example**:
```haskell
let f = (+1)
    x = 5
in pure f <*> pure x == pure (f x)  -- True (both equal pattern 6)
```

#### Interchange Law

```haskell
u <*> pure y = pure ($ y) <*> u
```

**Meaning**: Applying a function pattern to a pure value equals applying a pure function to the value pattern.

**Example**:
```haskell
let u = pattern (+1)
    y = 5
in u <*> pure y == pure ($ y) <*> u  -- True (both equal pattern 6)
```

### Structure-Preserving Semantics

The Applicative instance uses structure-preserving/zip-like semantics:

1. **Root Application**: The root function in the function pattern is applied to the root value in the value pattern
2. **Element Application**: Functions in elements are applied to values in corresponding elements (position by position)
3. **Recursive Application**: Function application is applied recursively to nested patterns
4. **Zip-like Truncation**: When element counts differ, functions are applied to values up to the minimum element count, ignoring extra elements

### Implementation Details

#### Pure Operation

The `pure` function wraps a value in an atomic pattern:

```haskell
pure :: a -> Pattern a
pure x = Pattern x []
```

**Behavior**:
- Creates an atomic pattern (pattern with empty elements list)
- The value becomes the pattern's decoration value
- Enables values to be used in applicative operations

#### Applicative Apply Operation

The `<*>` operator applies functions to values:

```haskell
(<*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
Pattern f fs <*> Pattern x xs = 
  Pattern (f x) (zipWith (<*>) fs xs)
```

**Breakdown**:
- `f x`: Apply root function to root value
- `zipWith (<*>) fs xs`: Recursively apply element functions to element values at corresponding positions
- `zipWith` provides zip-like truncation: if `fs` has 3 elements and `xs` has 5 elements, only the first 3 are applied

#### Processing Order

The Applicative instance processes patterns in a specific order:

1. Root function is applied to root value first
2. Element functions are applied to element values recursively from left to right
3. Nested patterns are processed recursively at each level

This order ensures consistent behavior and aligns with the structure-preserving semantics.

### Relationship to Functor

Applicative extends Functor, and the relationship must hold:

```haskell
fmap f x = pure f <*> x
```

**Meaning**: Applying a function using Functor's `fmap` produces the same result as applying it using Applicative's `pure` and `<*>`.

**Example**:
```haskell
let f = (+1)
    x = pattern 5
in fmap f x == pure f <*> x  -- True (both equal pattern 6)
```

### Use Cases

The Applicative instance enables several practical use cases:

1. **Structured Function Application**: Apply functions stored in patterns to values stored in patterns
2. **Pattern-based Validation**: Validate pattern values using pattern-structured validation functions
3. **Structured Transformations**: Transform patterns using pattern-structured transformation functions
4. **Compositional Operations**: Compose operations on patterns in a structured way

### Edge Cases

The Applicative instance handles various edge cases:

1. **Atomic Patterns**: `pure f <*> pure x` produces `pure (f x)` (homomorphism law)
2. **Mismatched Element Counts**: Zip-like truncation applies functions to values up to minimum count
3. **Empty Elements**: Atomic patterns (empty elements list) work correctly
4. **Deep Nesting**: Function application works recursively at all nesting levels
5. **Type Transformations**: Functions can change value types (e.g., `String -> Int`)

### Type Safety Guarantees

1. **Type transformation**: The Applicative instance allows safe transformation from `Pattern (a -> b)` and `Pattern a` to `Pattern b`
2. **Structure preservation**: Pattern structure (element count, nesting depth, element order) is preserved during function application (up to truncation for mismatched structures)
3. **Applicative laws**: The implementation satisfies all Applicative laws (verified by property-based tests)
4. **Recursive application**: Function application is applied recursively to all nested patterns
5. **Consistency with Functor**: The relationship `fmap f x = pure f <*> x` holds for all functions and patterns

