# Data Model: Hashable Instance for Pattern

**Feature**: 012-hashable-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Overview

This document describes the `Hashable` instance for the `Pattern` type, which enables using patterns as keys in `HashMap` and elements in `HashSet` for efficient hash-based lookups and deduplication.

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
- Has `Semigroup` and `Monoid` instances
- Recursive structure enables arbitrary nesting

### Hashable Instance

**Type Signature**:
```haskell
instance Hashable v => Hashable (Pattern v) where
  hashWithSalt :: Int -> Pattern v -> Int
```

**Hash Semantics**:
- **Structure-preserving**: Hash patterns based on their structure (value and elements recursively)
- **Consistent with Eq**: Equal patterns (according to `Eq`) produce the same hash value
- **Good distribution**: Patterns with different structures produce different hash values in the majority of cases
- **Recursive**: Nested patterns are hashed recursively, ensuring deep structures contribute to the hash

**Implementation**:
```haskell
instance Hashable v => Hashable (Pattern v) where
  hashWithSalt s (Pattern v els) = s `hashWithSalt` v `hashWithSalt` els
```

Where `els` is hashed as a list, which recursively hashes each element pattern using the `Hashable` instance for `[Pattern v]` (which requires `Hashable (Pattern v)`).

## Hash Rules

### Rule 1: Structure-Preserving Hashing

Patterns are hashed based on their structure (value and elements), not flattened values:

```haskell
-- Pattern with value "a" and elements [pattern "b", pattern "c"]
hash (patternWith "a" [pattern "b", pattern "c"])
-- Hash includes: value "a" + elements [pattern "b", pattern "c"]

-- Different structure with same flattened values
hash (patternWith "a" [patternWith "b" [pattern "c"]])
-- Hash includes: value "a" + elements [patternWith "b" [pattern "c"]]
-- Different hash (different structure)
```

The hash distinguishes patterns with different structures even if they have the same flattened values.

### Rule 2: Hash Consistency with Eq

Equal patterns (according to `Eq`) produce the same hash value:

```haskell
-- If two patterns are equal
p1 == p2  -- True
-- Then their hashes are equal
hash p1 == hash p2  -- True
```

This is a fundamental requirement for `Hashable` instances and enables correct behavior in hash-based containers.

### Rule 3: Recursive Hashing

Nested patterns are hashed recursively:

```haskell
-- Pattern with nested structure
patternWith "outer" [patternWith "inner" [pattern "value"]]
-- Hash includes: value "outer" + elements [patternWith "inner" [pattern "value"]]
-- Which recursively hashes: value "inner" + elements [pattern "value"]
-- Which recursively hashes: value "value" + elements []
```

Deeply nested structures contribute to the hash value correctly.

### Rule 4: Hash Distribution

Patterns with different structures produce different hash values in the majority of cases:

```haskell
-- Different structures produce different hashes (usually)
hash (pattern "a") /= hash (pattern "b")  -- Usually true
hash (patternWith "a" [pattern "b"]) /= hash (pattern "a")  -- Usually true
```

Hash collisions are possible but should be minimized. When collisions occur, hash-based containers handle them correctly through equality comparison.

## Edge Cases

### Atomic Patterns (No Elements)

Hashing atomic patterns includes the value and empty elements list:

```haskell
hash (pattern "a")
-- Hash includes: value "a" + elements []
```

### Patterns with Many Elements

Hashing patterns with many elements includes all elements:

```haskell
hash (patternWith "root" [pattern "a", pattern "b", ..., pattern "z"])
-- Hash includes: value "root" + elements [pattern "a", pattern "b", ..., pattern "z"]
```

All elements contribute to the hash value.

### Deeply Nested Patterns

Hashing deeply nested patterns recursively hashes all nested structures:

```haskell
hash (patternWith "level1" [patternWith "level2" [patternWith "level3" [pattern "value"]]])
-- Hash includes all levels recursively
```

Deep nesting contributes to the hash value correctly.

### Patterns with Same Flattened Values but Different Structures

Patterns with the same flattened values but different structures produce different hashes:

```haskell
-- Pattern 1: flat structure
p1 = patternWith "a" [pattern "b", pattern "c"]
-- Pattern 2: nested structure with same flattened values
p2 = patternWith "a" [patternWith "b" [pattern "c"]]

hash p1 /= hash p2  -- Different hashes (different structures)
```

Structure-preserving hashing distinguishes these patterns.

### Type Constraint

The `Hashable` instance requires `Hashable v` constraint:

```haskell
-- This compiles (String has Hashable instance)
hash (pattern "a") :: Int

-- This doesn't compile if CustomType doesn't have Hashable instance
hash (pattern customValue) :: Int
-- Error: No instance for (Hashable CustomType)
```

## Validation Rules

### Hash Consistency with Eq

The `Hashable` instance must satisfy:

```haskell
-- For all patterns p1 and p2
if p1 == p2 then hash p1 == hash p2
```

This must hold for all patterns `p1`, `p2` of type `Pattern v` where `Hashable v`.

### Hash Distribution

The hash function should provide good distribution:

- Patterns with different structures should produce different hash values in the majority of cases
- Hash collisions should be minimized
- When collisions occur, they should be handled correctly through equality comparison in hash-based containers

### Structure-Preserving Hashing

The hash function must distinguish patterns with different structures:

- Patterns with the same flattened values but different structures must produce different hashes
- The hash must include both value and elements, not just flattened values

## Relationships

### Relationship to Value Type's Hashable

The `Hashable` instance for `Pattern` delegates value hashing to the value type's `Hashable` instance:

```haskell
hashWithSalt s (Pattern v els) = s `hashWithSalt` v `hashWithSalt` els
```

This respects the value type's own hashing semantics.

### Relationship to List Hashing

Element hashing uses standard list hashing:

```haskell
hashWithSalt s (Pattern v els) = s `hashWithSalt` v `hashWithSalt` els
```

Where `els :: [Pattern v]` is hashed using the `Hashable` instance for `[Pattern v]`, which recursively hashes each element pattern.

### Relationship to Eq Instance

The hash function is designed to be consistent with the `Eq` instance:

- `Eq` compares value first, then elements recursively
- `Hashable` hashes value first, then elements recursively
- This ensures equal patterns produce the same hash

### Relationship to Hash-Based Containers

The `Hashable` instance enables hash-based containers:

- `HashMap (Pattern v) a`: Patterns as keys with O(1) average-case lookups
- `HashSet (Pattern v)`: Patterns as elements with O(1) average-case membership testing
- Hash collisions are handled through equality comparison using the `Eq` instance

### Relationship to Ordered Containers

Hash-based containers provide an alternative to ordered containers:

- `HashMap`/`HashSet`: O(1) average-case operations, no ordering
- `Data.Map`/`Data.Set`: O(log n) operations, sorted order
- Choose based on whether ordering is needed and performance requirements

## State Transitions

N/A - `Hashable` instance is a pure function with no state.

## Notes

- The `Hashable` instance is straightforward: hash value and elements recursively
- No additional state or caching is needed
- The implementation follows standard Haskell conventions for recursive types
- Performance is O(n) where n is the number of nodes in the pattern structure
- The instance enables efficient hash-based lookups and deduplication
- Hash collisions are acceptable if handled correctly through equality comparison

