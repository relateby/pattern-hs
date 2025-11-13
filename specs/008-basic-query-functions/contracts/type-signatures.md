# Type Signatures: Basic Query Functions

**Feature**: 008-basic-query-functions  
**Date**: 2025-01-27

## Overview

This document defines the public API type signatures for basic query functions for `Pattern`. These functions enable pattern introspection by providing structural information and value extraction.

---

## Core Module: Pattern.Core

### Query Functions

#### length

```haskell
-- | Returns the number of direct elements in a pattern's sequence.
--
-- This function counts only the direct children of a pattern, not nested
-- descendants. For atomic patterns (no elements), it returns 0.
--
-- === Examples
--
-- >>> length (pattern "root")
-- 0
--
-- >>> length (patternWith "root" [pattern "a", pattern "b"])
-- 2
--
-- >>> length (patternWith "root" [patternWith "inner" [pattern "a"]])
-- 1  -- Only counts direct children, not nested elements
--
-- === Properties
--
-- * @length (Pattern v []) == 0@ (atomic patterns have length 0)
-- * @length p >= 0@ (length is always non-negative)
-- * @length p == length (elements p)@ (equivalent to counting elements list)
length :: Pattern v -> Int
length (Pattern _ els) = length els
```

#### size

```haskell
-- | Returns the total number of nodes in a pattern structure.
--
-- This function recursively counts all nodes in a pattern, including the
-- root node and all nested pattern nodes. For atomic patterns, it returns 1
-- (just the root node).
--
-- === Examples
--
-- >>> size (pattern "root")
-- 1
--
-- >>> size (patternWith "root" [pattern "a", pattern "b"])
-- 3  -- root + a + b
--
-- >>> size (patternWith "root" [patternWith "inner" [pattern "a"]])
-- 3  -- root + inner + a
--
-- === Properties
--
-- * @size (Pattern v []) == 1@ (atomic patterns have size 1)
-- * @size p >= 1@ (size is always at least 1)
-- * @size p >= length p@ (size includes root, length doesn't)
-- * @size p == 1 + sum (map size (elements p))@ (recursive definition)
size :: Pattern v -> Int
size (Pattern _ els) = 1 + sum (map size els)
```

#### depth

```haskell
-- | Returns the maximum nesting depth of a pattern structure.
--
-- Depth is counted from the root: depth 0 for atomic patterns (root only),
-- depth 1 for one level of nesting, etc. For patterns with multiple branches
-- of different depths, returns the maximum depth across all branches.
--
-- === Examples
--
-- >>> depth (pattern "root")
-- 0  -- Root only, no nesting
--
-- >>> depth (patternWith "root" [pattern "a"])
-- 1  -- One level of nesting
--
-- >>> depth (patternWith "root" [patternWith "inner" [pattern "a"]])
-- 2  -- Two levels of nesting
--
-- >>> depth (patternWith "root" [pattern "a", patternWith "b" [patternWith "c" [pattern "d"]]])
-- 3  -- Maximum depth across branches (a=1, b->c->d=3)
--
-- === Properties
--
-- * @depth (Pattern v []) == 0@ (atomic patterns have depth 0)
-- * @depth p >= 0@ (depth is always non-negative)
-- * @depth p <= size p - 1@ (depth cannot exceed size - 1)
-- * @depth p == if null (elements p) then 0 else 1 + maximum (map depth (elements p))@
depth :: Pattern v -> Int
depth (Pattern _ []) = 0
depth (Pattern _ els) = 1 + maximum (map depth els)
```

#### values

```haskell
-- | Returns all values from a pattern structure as a flat list.
--
-- This function extracts all values from a pattern, including the pattern's
-- own value and all element values at all nesting levels. The order is:
-- pattern's value first, then element values in order, recursively.
--
-- This function is equivalent to @toList@ from the Foldable instance and
-- to @flatten@, but is provided explicitly for clarity and intentional
-- value extraction.
--
-- === Examples
--
-- >>> values (pattern "root")
-- ["root"]
--
-- >>> values (patternWith "root" [pattern "a", pattern "b"])
-- ["root", "a", "b"]
--
-- >>> values (patternWith "root" [patternWith "inner" [pattern "a"]])
-- ["root", "inner", "a"]
--
-- === Properties
--
-- * @values (Pattern v []) == [v]@ (atomic patterns return single value)
-- * @length (values p) == size p@ (one value per node)
-- * @values p == toList p@ (equivalent to Foldable.toList)
-- * @values p == flatten p@ (equivalent to flatten function)
-- * @head (values p) == value p@ (first value is pattern's value)
values :: Pattern v -> [v]
values = toList
```

#### value (Existing)

```haskell
-- | Returns the decoration value of a pattern.
--
-- This is the field accessor from the Pattern data type definition.
-- It provides direct access to a pattern's decoration value without
-- any computation.
--
-- === Examples
--
-- >>> value (pattern "test")
-- "test"
--
-- >>> value (pattern 42)
-- 42
--
-- >>> value (patternWith "root" [pattern "a"])
-- "root"
--
-- === Properties
--
-- * @value (Pattern v _) == v@ (direct field access)
-- * Works for patterns of any value type
-- * No computation required (O(1) operation)
value :: Pattern v -> v
-- Already defined as field accessor in Pattern data type
```

---

## Module Exports

All query functions should be exported from `Pattern.Core` and re-exported from the main `Pattern` module:

```haskell
-- Pattern.Core exports
module Pattern.Core (
  -- ... existing exports ...
  length,
  size,
  depth,
  values,
  value  -- already exported
) where

-- Pattern module re-exports
module Pattern (
  module Pattern.Core,
  -- ... other module exports ...
) where
```

---

## Type Constraints

### No Type Constraints Required

All query functions work with patterns of any value type `v`. No type class constraints are needed:

- `length`, `size`, `depth`: Work with any `Pattern v` (no constraints on `v`)
- `values`: Works with any `Pattern v` (no constraints on `v`)
- `value`: Works with any `Pattern v` (no constraints on `v`)

### Purity Guarantee

All query functions are pure (no side effects):

- No IO operations
- No mutable state
- Deterministic: same input always produces same output
- Referentially transparent

---

## Performance Characteristics

### Time Complexity

- `length`: O(1) - Direct list length operation
- `size`: O(n) - Must traverse all nodes, where n is total node count
- `depth`: O(n) - Must traverse all nodes to find maximum depth
- `values`: O(n) - Must traverse all nodes to extract values, where n is total node count
- `value`: O(1) - Direct field access

### Space Complexity

- `length`, `size`, `depth`: O(d) - Stack space for recursion, where d is maximum depth
- `values`: O(n) - Must allocate list for all values, where n is total node count
- `value`: O(1) - No additional space needed

### Performance Targets

- `length`: <1ms for any pattern structure
- `size`: <10ms for patterns with up to 1000 nodes
- `depth`: <5ms for patterns with up to 100 levels of nesting
- `values`: <10ms for patterns with up to 1000 nodes

---

## Error Handling

### No Error Cases

All query functions are total functions (defined for all inputs):

- No invalid inputs (any `Pattern v` is valid)
- No error conditions
- No exceptions thrown
- Always return valid results

### Edge Case Handling

Functions handle all edge cases correctly:

- Atomic patterns (no elements)
- Patterns with single element
- Patterns with many elements
- Deeply nested patterns
- Patterns with varying branch depths

---

## Testing Requirements

### Unit Tests

Each function must have unit tests covering:

- Atomic patterns
- Patterns with single element
- Patterns with multiple elements
- Nested patterns (2-3 levels)
- Deeply nested patterns (10+ levels)
- Patterns with many direct elements (100+)

### Property-Based Tests

Property-based tests should verify:

- `length p >= 0`
- `size p >= 1`
- `size p >= length p`
- `depth p >= 0`
- `depth p <= size p - 1`
- `length (values p) == size p`
- `head (values p) == value p`
- `values p == toList p`
- `values p == flatten p`

### Integration Tests

Integration tests should verify:

- Query functions work correctly with all existing Pattern operations
- Query functions work with patterns created using all construction functions
- Query functions work with patterns transformed using Functor, Foldable, Traversable

