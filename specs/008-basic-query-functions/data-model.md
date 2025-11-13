# Data Model: Basic Query Functions

**Feature**: 008-basic-query-functions  
**Date**: 2025-01-27

## Overview

This feature adds query functions to the existing Pattern data type. No new data structures are introduced. The query functions operate on the existing `Pattern v` type to extract structural information and values.

## Existing Data Structure

### Pattern v

The Pattern type is already defined in `Pattern.Core`:

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
```

**Fields**:
- `value :: v`: The decoration value associated with this pattern
- `elements :: [Pattern v]`: The sequence of pattern elements (the pattern itself)

**Properties**:
- Recursive structure: elements are themselves Patterns
- Type parameter `v`: Allows patterns with any value type
- Immutable: Pattern values are immutable once constructed

## Query Functions (New)

### length :: Pattern v -> Int

Returns the number of direct elements in a pattern's sequence.

**Semantics**:
- Atomic pattern (empty elements): Returns 0
- Pattern with n direct elements: Returns n
- Does not count nested descendants, only direct children

**Examples**:
- `length (Pattern "root" [])` = 0
- `length (Pattern "root" [Pattern "a" []])` = 1
- `length (Pattern "root" [Pattern "a" [], Pattern "b" []])` = 2

### size :: Pattern v -> Int

Returns the total number of nodes in a pattern structure (including all nested patterns).

**Semantics**:
- Atomic pattern: Returns 1 (the root node)
- Pattern with n direct elements: Returns 1 + sum of sizes of all elements
- Recursively counts all nodes at all nesting levels

**Examples**:
- `size (Pattern "root" [])` = 1
- `size (Pattern "root" [Pattern "a" []])` = 2 (root + one element)
- `size (Pattern "root" [Pattern "a" [Pattern "b" []]])` = 3 (root + a + b)

### depth :: Pattern v -> Int

Returns the maximum nesting depth of a pattern structure.

**Semantics**:
- Atomic pattern: Returns 0 (root only, no nesting)
- Pattern with one level of nesting: Returns 1
- Pattern with multiple branches: Returns maximum depth across all branches
- Depth = number of nesting levels below root

**Examples**:
- `depth (Pattern "root" [])` = 0
- `depth (Pattern "root" [Pattern "a" []])` = 1
- `depth (Pattern "root" [Pattern "a" [Pattern "b" []]])` = 2

### values :: Pattern v -> [v]

Returns all values from a pattern structure as a flat list.

**Semantics**:
- Atomic pattern: Returns list containing only the pattern's value
- Pattern with elements: Returns pattern's value followed by all element values recursively
- Order: Pattern's value first, then element values in order, recursively
- Equivalent to `toList` from Foldable instance

**Examples**:
- `values (Pattern "root" [])` = ["root"]
- `values (Pattern "root" [Pattern "a" []])` = ["root", "a"]
- `values (Pattern "root" [Pattern "a" [Pattern "b" []]])` = ["root", "a", "b"]

### value :: Pattern v -> v (Existing)

Field accessor already exists from Pattern data type definition.

**Semantics**:
- Returns the decoration value of a pattern
- Works for patterns of any value type
- No computation required (direct field access)

**Examples**:
- `value (Pattern "test" [])` = "test"
- `value (Pattern 42 [])` = 42

## Relationships

### Query Function Relationships

- `size p >= length p` (size includes root, length doesn't)
- `size p >= 1` (always at least the root node)
- `length p >= 0` (can be 0 for atomic patterns)
- `depth p >= 0` (depth 0 for atomic patterns)
- `length (values p) == size p` (one value per node)

### Integration with Existing Functions

- `values` is equivalent to `toList` from Foldable instance
- `values` is equivalent to `flatten` function (both extract all values)
- Query functions are pure (no side effects)
- Query functions work with any value type `v`

## Validation Rules

### Input Validation

- All query functions accept any `Pattern v` (no invalid inputs)
- Functions handle all pattern structures correctly:
  - Atomic patterns (no elements)
  - Patterns with elements
  - Deeply nested patterns
  - Patterns with many direct elements

### Output Validation

- `length` returns non-negative integers (>= 0)
- `size` returns positive integers (>= 1)
- `depth` returns non-negative integers (>= 0)
- `values` returns a list (may be empty only if pattern has no value, which is impossible)
- All functions are deterministic (same input always produces same output)

## Edge Cases

### Atomic Patterns

- `length (Pattern v [])` = 0
- `size (Pattern v [])` = 1
- `depth (Pattern v [])` = 0
- `values (Pattern v [])` = [v]

### Single Element Patterns

- `length (Pattern v [Pattern v1 []])` = 1
- `size (Pattern v [Pattern v1 []])` = 2
- `depth (Pattern v [Pattern v1 []])` = 1
- `values (Pattern v [Pattern v1 []])` = [v, v1]

### Deep Nesting

- Functions must handle patterns with 100+ levels of nesting
- Functions must handle patterns with 1000+ total nodes
- Functions must complete without stack overflow for typical use cases

### Many Direct Elements

- Functions must handle patterns with 100+ direct elements
- Performance should remain acceptable (<10ms for 1000 nodes)

## State Transitions

N/A - Query functions are pure and don't modify state. They only read pattern structure and return computed values.

