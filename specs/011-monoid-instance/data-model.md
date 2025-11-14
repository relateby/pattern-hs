# Data Model: Monoid Instance for Pattern

**Feature**: 011-monoid-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Overview

This document describes the `Monoid` instance for the `Pattern` type, which extends the existing `Semigroup` instance by providing an identity element (`mempty`). The identity pattern has `mempty` value (from value type's Monoid) and empty elements list, enabling identity-based operations and standard Monoid combinators.

## Core Entity

### Pattern Type

The `Pattern` type is a recursive data structure representing decorated sequences:

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
  deriving (Eq)
```

**Existing Properties**:
- Has `Eq` instance for equality comparison
- Has `Show` instance for display
- Has `Functor`, `Foldable`, and `Traversable` instances
- Has `Ord` instance for ordering
- Has `Semigroup` instance for pattern combination
- Recursive structure enables arbitrary nesting

### Monoid Instance

**Type Signature**:
```haskell
instance Monoid v => Monoid (Pattern v) where
  mempty :: Pattern v
  (<>) :: Pattern v -> Pattern v -> Pattern v  -- Inherited from Semigroup
```

**Identity Semantics**:
- **Identity pattern**: `mempty = Pattern { value = mempty, elements = [] }`
- **Left identity**: `mempty <> p = p` for all patterns `p`
- **Right identity**: `p <> mempty = p` for all patterns `p`
- **Consistency**: Uses same `<>` implementation as Semigroup instance
- **Structure-preserving**: Preserves the decorated sequence model (elements form pattern, value is decoration)

**Implementation**:
```haskell
instance Monoid v => Monoid (Pattern v) where
  mempty = Pattern { value = mempty, elements = [] }
  -- <> inherited from Semigroup instance
```

## Identity Rules

### Rule 1: Identity Pattern Structure

The identity pattern has `mempty` value (from value type's Monoid) and empty elements list:

```haskell
mempty :: Pattern v
-- Structure: Pattern { value = mempty, elements = [] }
```

**For String values**:
```haskell
mempty :: Pattern String
-- Result: Pattern { value = "", elements = [] }
```

**For Sum Int values**:
```haskell
mempty :: Pattern (Sum Int)
-- Result: Pattern { value = Sum 0, elements = [] }
```

**For Product Int values**:
```haskell
mempty :: Pattern (Product Int)
-- Result: Pattern { value = Product 1, elements = [] }
```

### Rule 2: Left Identity Law

Combining identity with any pattern produces that pattern:

```haskell
mempty <> p = p
```

**Verification**:
- `value (mempty <> p) = value mempty <> value p = mempty <> value p = value p` (by Monoid identity for value type)
- `elements (mempty <> p) = elements mempty ++ elements p = [] ++ elements p = elements p` (by list concatenation)
- Therefore: `mempty <> p = Pattern { value = value p, elements = elements p } = p` ✅

**Example**:
```haskell
mempty <> pattern "test"
-- Result: pattern "test"
```

### Rule 3: Right Identity Law

Combining any pattern with identity produces that pattern:

```haskell
p <> mempty = p
```

**Verification**:
- `value (p <> mempty) = value p <> value mempty = value p <> mempty = value p` (by Monoid identity for value type)
- `elements (p <> mempty) = elements p ++ elements mempty = elements p ++ [] = elements p` (by list concatenation)
- Therefore: `p <> mempty = Pattern { value = value p, elements = elements p } = p` ✅

**Example**:
```haskell
pattern "test" <> mempty
-- Result: pattern "test"
```

### Rule 4: Consistency with Semigroup

The Monoid instance uses the same `<>` implementation as Semigroup:

```haskell
-- Semigroup instance
instance Semigroup v => Semigroup (Pattern v) where
  Pattern v1 els1 <> Pattern v2 els2 = Pattern (v1 <> v2) (els1 ++ els2)

-- Monoid instance (extends Semigroup)
instance Monoid v => Monoid (Pattern v) where
  mempty = Pattern { value = mempty, elements = [] }
  -- <> inherited from Semigroup
```

This ensures `p1 <> p2` produces the same result whether using Semigroup or Monoid instance.

## Edge Cases

### Identity with Atomic Patterns

Combining identity with atomic patterns:

```haskell
mempty <> Pattern { value = "a", elements = [] }
-- Result: Pattern { value = "a", elements = [] }

Pattern { value = "a", elements = [] } <> mempty
-- Result: Pattern { value = "a", elements = [] }
```

### Identity with Patterns Having Elements

Combining identity with patterns that have elements:

```haskell
mempty <> Pattern { value = "root", elements = [p1, p2] }
-- Result: Pattern { value = "root", elements = [p1, p2] }

Pattern { value = "root", elements = [p1, p2] } <> mempty
-- Result: Pattern { value = "root", elements = [p1, p2] }
```

### Identity with Nested Patterns

Combining identity with nested patterns:

```haskell
mempty <> Pattern { value = "outer", elements = [Pattern { value = "inner", elements = [p1] }] }
-- Result: Pattern { value = "outer", elements = [Pattern { value = "inner", elements = [p1] }] }
```

Nested structures are preserved when combining with identity.

### Using mconcat with Empty List

The `mconcat` combinator handles empty lists:

```haskell
mconcat [] :: Pattern v
-- Result: mempty
```

### Using mconcat with List Containing Only Identity

The `mconcat` combinator with list containing only identity:

```haskell
mconcat [mempty, mempty, mempty] :: Pattern v
-- Result: mempty
```

### Type Constraint

The `Monoid` instance requires `Monoid v` constraint:

```haskell
-- This compiles (String has Monoid instance)
mempty :: Pattern String

-- This doesn't compile if CustomType doesn't have Monoid instance
mempty :: Pattern CustomType
-- Error: No instance for (Monoid CustomType)
```

## Validation Rules

### Monoid Left Identity Law

The `Monoid` instance must satisfy:

```haskell
mempty <> p = p
```

This must hold for all patterns `p` of type `Pattern v` where `Monoid v`.

### Monoid Right Identity Law

The `Monoid` instance must satisfy:

```haskell
p <> mempty = p
```

This must hold for all patterns `p` of type `Pattern v` where `Monoid v`.

### Consistency with Semigroup

The Monoid instance must be consistent with Semigroup:

```haskell
p1 <> p2  -- Same result whether using Semigroup or Monoid instance
```

### Element Order Preservation

The combination operation must preserve element order (inherited from Semigroup):

```haskell
elements (p1 <> p2) = elements p1 ++ elements p2
```

This ensures sequence semantics are maintained.

### Decorated Sequence Model Alignment

The identity pattern must align with the decorated sequence model:

- **Elements form the pattern**: Identity has empty elements (no pattern sequence)
- **Value is decoration**: Identity has `mempty` value (no decoration)

## Relationships

### Relationship to Semigroup Instance

The `Monoid` instance extends the `Semigroup` instance:

```haskell
-- Semigroup provides <>
instance Semigroup v => Semigroup (Pattern v) where
  Pattern v1 els1 <> Pattern v2 els2 = Pattern (v1 <> v2) (els1 ++ els2)

-- Monoid adds mempty, inherits <>
instance Monoid v => Monoid (Pattern v) where
  mempty = Pattern { value = mempty, elements = [] }
  -- <> inherited from Semigroup
```

The Monoid instance naturally extends Semigroup by adding the identity element.

### Relationship to Value Type's Monoid

The `Monoid` instance for `Pattern` delegates identity to the value type's Monoid instance:

```haskell
value mempty = mempty  -- From value type's Monoid
```

This respects the value type's own identity semantics.

### Relationship to Standard Monoid Combinators

The `Monoid` instance enables standard Monoid combinators:

- `mconcat :: [Pattern v] -> Pattern v`: Combines a list of patterns (returns `mempty` for empty list)
- `mappend :: Pattern v -> Pattern v -> Pattern v`: Alias for `<>` (inherited from Semigroup)

### Relationship to Existing Operations

**Semigroup (`<>`)**: Combines two patterns. Requires both patterns to exist. Cannot handle empty collections gracefully.

**Monoid (`mempty`, `mconcat`)**: Extends Semigroup with identity element. Enables:
- Handling empty collections: `mconcat [] = mempty`
- Optional pattern construction: `maybe mempty pattern optionalValue`
- Clean initialization: `mempty <> p1 <> p2`
- Standard accumulation patterns: `foldr (<>) mempty patterns`

## State Transitions

N/A - `Monoid` instance is a pure function with no state.

## Notes

- The `Monoid` instance is straightforward: add `mempty`, inherit `<>` from Semigroup
- No additional state or caching is needed
- The implementation follows standard Haskell conventions for extending Semigroup to Monoid
- Identity pattern creation (`mempty`) is O(1) time
- Combination with identity is O(n) time where n is the number of elements in the non-identity pattern
- The instance enables identity-based operations and standard Monoid combinators

