# Type Signatures: Monoid Instance for Pattern

**Feature**: 011-monoid-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Monoid Instance

### Type Signature

```haskell
instance Monoid v => Monoid (Pattern v) where
  mempty :: Pattern v
  (<>) :: Pattern v -> Pattern v -> Pattern v  -- Inherited from Semigroup
```

### Implementation

```haskell
instance Monoid v => Monoid (Pattern v) where
  mempty = Pattern { value = mempty, elements = [] }
  -- <> inherited from Semigroup instance
```

### Type Constraints

- **Required**: `Monoid v` - The value type must have a `Monoid` instance
- **Provided**: `Monoid (Pattern v)` - Patterns are monoids when values are monoids
- **Inherits**: `Semigroup (Pattern v)` - Monoid extends Semigroup, inherits `<>` operation

### Semantics

- **Identity pattern**: `mempty = Pattern { value = mempty, elements = [] }` (value from value type's Monoid, empty elements)
- **Left identity**: `mempty <> p = p` for all patterns `p` (Monoid law)
- **Right identity**: `p <> mempty = p` for all patterns `p` (Monoid law)
- **Consistency**: Uses same `<>` implementation as Semigroup instance
- **Structure-preserving**: Preserves decorated sequence model (elements form pattern, value is decoration)

## Standard Monoid Combinators

### mconcat

```haskell
-- Type signature
mconcat :: [Pattern v] -> Pattern v
```

**Requirement**: `Monoid (Pattern v)` instance (satisfied when `Monoid v`)

**Example**:
```haskell
mconcat [pattern "a", pattern "b", pattern "c"]
-- Result: pattern "abc" (for String values) with concatenated elements

mconcat []  -- Empty list
-- Result: mempty
```

### mappend

```haskell
-- Type signature
mappend :: Pattern v -> Pattern v -> Pattern v
```

**Requirement**: `Monoid (Pattern v)` instance (satisfied when `Monoid v`)

**Note**: `mappend` is an alias for `<>` (inherited from Semigroup)

**Example**:
```haskell
mappend (pattern "a") (pattern "b")
-- Result: pattern "ab" (for String values) with concatenated elements
```

## Test Contracts

### Unit Test Requirements

1. **Identity Pattern Structure**
   - Test: Verify `mempty` has correct structure
   - Expected: `value mempty = mempty` (from value type) and `elements mempty = []`
   - Test: Verify identity for different value types (String, Sum Int, Product Int, All, Any)
   - Expected: Correct identity value for each type

2. **Left Identity Law**
   - Test: `mempty <> p = p` for atomic patterns
   - Expected: Result equals original pattern
   - Test: `mempty <> p = p` for patterns with elements
   - Expected: Result equals original pattern
   - Test: `mempty <> p = p` for nested patterns
   - Expected: Result equals original pattern

3. **Right Identity Law**
   - Test: `p <> mempty = p` for atomic patterns
   - Expected: Result equals original pattern
   - Test: `p <> mempty = p` for patterns with elements
   - Expected: Result equals original pattern
   - Test: `p <> mempty = p` for nested patterns
   - Expected: Result equals original pattern

4. **Consistency with Semigroup**
   - Test: `p1 <> p2` produces same result using Semigroup or Monoid instance
   - Expected: Identical results
   - Test: Verify `<>` implementation is same for both instances
   - Expected: Same implementation (inherited)

5. **Standard Monoid Combinators**
   - Test: `mconcat []` returns `mempty`
   - Expected: Identity pattern
   - Test: `mconcat [p1, p2, p3]` combines all patterns
   - Expected: All patterns combined correctly
   - Test: `mconcat [mempty, mempty, mempty]` returns `mempty`
   - Expected: Identity pattern

### Property-Based Test Requirements

1. **Monoid Left Identity Law**
   - Property: `mempty <> p = p` for all patterns `p`
   - Test: Generate random patterns and verify left identity
   - Test: Test with different value types (String, Sum Int, Product Int, All, Any)
   - Test: Test with different pattern structures (atomic, with elements, nested)

2. **Monoid Right Identity Law**
   - Property: `p <> mempty = p` for all patterns `p`
   - Test: Generate random patterns and verify right identity
   - Test: Test with different value types (String, Sum Int, Product Int, All, Any)
   - Test: Test with different pattern structures (atomic, with elements, nested)

3. **Identity Pattern Structure**
   - Property: `value mempty = mempty` (from value type) and `elements mempty = []`
   - Test: Verify identity pattern structure for different value types
   - Test: Verify identity pattern structure is consistent across different Monoid instances

4. **Consistency with Semigroup**
   - Property: `p1 <> p2` produces same result using Semigroup or Monoid instance
   - Test: Verify consistency for all pattern combinations
   - Test: Test with different value types and pattern structures

### Integration Test Requirements

1. **Standard Monoid Combinators**
   - Test: Use `mconcat` with list of patterns
   - Expected: All patterns combined correctly
   - Test: Use `mconcat` with empty list
   - Expected: Returns `mempty`
   - Test: Use `mconcat` with list containing only `mempty`
   - Expected: Returns `mempty`

2. **Edge Cases**
   - Test: Combine `mempty` with atomic patterns
   - Expected: Identity laws hold
   - Test: Combine `mempty` with patterns having elements
   - Expected: Identity laws hold
   - Test: Combine `mempty` with deeply nested patterns
   - Expected: Identity laws hold, nested structures preserved
   - Test: Combine `mempty` with patterns of different nesting depths
   - Expected: Identity laws hold, structures preserved

3. **Type Constraint Verification**
   - Test: Attempt to use `mempty` with value type without Monoid instance
   - Expected: Compile-time error
   - Test: Use `mempty` with value type with Monoid instance
   - Expected: Compiles and executes correctly

4. **Value Type Monoid Integration**
   - Test: Identity with String values (empty string)
   - Expected: `value mempty = ""`
   - Test: Identity with Sum Int values (Sum 0)
   - Expected: `value mempty = Sum 0`
   - Test: Identity with Product Int values (Product 1)
   - Expected: `value mempty = Product 1`
   - Test: Identity with All values (All True)
   - Expected: `value mempty = All True`
   - Test: Identity with Any values (Any False)
   - Expected: `value mempty = Any False`

## Test Coverage Requirements

- **Unit tests**: Cover identity pattern structure, left/right identity laws, consistency with Semigroup, standard Monoid combinators, edge cases, different value types
- **Property-based tests**: Verify Monoid identity laws (left and right), identity pattern structure, consistency with Semigroup
- **Integration tests**: Verify standard Monoid combinators (`mconcat`), edge cases, type constraints, value type integration
- **Coverage target**: 100% of `Monoid` instance methods covered by tests

## Performance Requirements

- **Identity pattern creation**: O(1) time (constant time, just creates pattern with identity value and empty list)
- **Combination with identity**: O(n) time where n is the number of elements in the non-identity pattern (inherited from Semigroup)
- **Value identity lookup**: O(1) for most value types (delegated to value type's Monoid)
- **No performance optimizations needed**: Standard pattern creation and list operations are sufficient

