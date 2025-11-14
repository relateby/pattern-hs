# Type Signatures: Hashable Instance for Pattern

**Feature**: 012-hashable-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Hashable Instance

### Type Signature

```haskell
instance Hashable v => Hashable (Pattern v) where
  hashWithSalt :: Int -> Pattern v -> Int
  hash :: Pattern v -> Int
```

### Implementation

```haskell
instance Hashable v => Hashable (Pattern v) where
  hashWithSalt s (Pattern v els) = s `hashWithSalt` v `hashWithSalt` els
```

The `hash` function is provided by the default implementation in the `Hashable` class, which uses `hashWithSalt` with a default salt.

### Type Constraints

- **Required**: `Hashable v` - The value type must have a `Hashable` instance
- **Provided**: `Hashable (Pattern v)` - Patterns are hashable when values are hashable

### Semantics

- **Structure-preserving**: Hash patterns based on their structure (value and elements recursively)
- **Consistent with Eq**: For all patterns `p1` and `p2`, if `p1 == p2`, then `hash p1 == hash p2`
- **Good distribution**: Patterns with different structures produce different hash values in the majority of cases
- **Recursive**: Nested patterns are hashed recursively, ensuring deep structures contribute to the hash

## Standard Hashable Functions

### hash

```haskell
-- Type signature
hash :: Hashable a => a -> Int
```

**Requirement**: `Hashable (Pattern v)` instance (satisfied when `Hashable v`)

**Example**:
```haskell
hash (pattern "a")  -- Int
hash (patternWith "root" [pattern "a", pattern "b"])  -- Int
```

### hashWithSalt

```haskell
-- Type signature
hashWithSalt :: Hashable a => Int -> a -> Int
```

**Requirement**: `Hashable (Pattern v)` instance (satisfied when `Hashable v`)

**Example**:
```haskell
hashWithSalt 42 (pattern "a")  -- Int
hashWithSalt 42 (patternWith "root" [pattern "a", pattern "b"])  -- Int
```

## Hash-Based Container Integration

### HashMap

```haskell
-- Type signature
HashMap (Pattern v) a
```

**Requirement**: `Hashable (Pattern v)` instance (satisfied when `Hashable v`)

**Example**:
```haskell
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

let m = HashMap.fromList [(pattern "a", 1), (pattern "b", 2)]
HashMap.lookup (pattern "a") m  -- Just 1
```

### HashSet

```haskell
-- Type signature
HashSet (Pattern v)
```

**Requirement**: `Hashable (Pattern v)` instance (satisfied when `Hashable v`)

**Example**:
```haskell
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

let s = HashSet.fromList [pattern "a", pattern "b", pattern "c"]
HashSet.member (pattern "a") s  -- True
```

## Test Contracts

### Unit Test Requirements

1. **Atomic Pattern Hashing**
   - Test: Hash atomic pattern
   - Expected: Returns valid hash value
   - Test: Hash multiple atomic patterns with different values
   - Expected: Different values produce different hashes (usually)

2. **Pattern with Elements Hashing**
   - Test: Hash pattern with elements
   - Expected: Returns valid hash value
   - Test: Hash patterns with different element counts
   - Expected: Different structures produce different hashes (usually)

3. **Nested Pattern Hashing**
   - Test: Hash pattern with nested structures
   - Expected: Returns valid hash value
   - Test: Hash patterns with different nesting depths
   - Expected: Different depths produce different hashes (usually)

4. **Hash Consistency with Eq**
   - Test: Hash two equal patterns
   - Expected: Same hash value
   - Test: Hash two different patterns
   - Expected: May have different hashes (collisions possible but should be rare)

5. **Structure-Preserving Hashing**
   - Test: Hash patterns with same flattened values but different structures
   - Expected: Different hashes (different structures)
   - Test: Hash patterns with different values but same structure
   - Expected: Different hashes (different values)

### Property-Based Test Requirements

1. **Hash Consistency with Eq**
   - Property: For all patterns `p1` and `p2`, if `p1 == p2`, then `hash p1 == hash p2`
   - Test: Generate random patterns and verify hash consistency
   - Test: Test with different value types (String, Int, custom Hashable instances)
   - Test: Test all pattern structures (atomic, with elements, nested, different depths)

2. **Hash Distribution**
   - Property: Patterns with different structures produce different hashes in the majority of cases
   - Test: Generate many patterns with different structures, measure collision rate
   - Test: Verify collision rate is below acceptable threshold (e.g., < 1% for random patterns)
   - Test: Test with patterns of different sizes and nesting depths

3. **Recursive Hashing**
   - Property: Nested patterns contribute to hash value correctly
   - Test: Verify that changing nested structure changes hash
   - Test: Verify that deeply nested patterns hash correctly
   - Test: Test with different nesting depths

4. **Structure-Preserving Hashing**
   - Property: Patterns with different structures produce different hashes even if flattened values are same
   - Test: Generate patterns with same flattened values but different structures
   - Test: Verify they produce different hashes
   - Test: Test with various structure differences

### Integration Test Requirements

1. **HashMap Integration**
   - Test: Create `HashMap (Pattern v) a` with patterns as keys
   - Expected: Patterns can be used as keys, lookups work correctly
   - Test: Insert patterns, lookup patterns, verify correct behavior
   - Test: Handle hash collisions correctly (patterns with same hash but different values)

2. **HashSet Integration**
   - Test: Create `HashSet (Pattern v)` with patterns as elements
   - Expected: Patterns can be used as elements, membership testing works correctly
   - Test: Insert patterns, test membership, verify deduplication works
   - Test: Handle hash collisions correctly (patterns with same hash but different values)

3. **Edge Cases**
   - Test: Hash atomic patterns
   - Expected: Valid hash values
   - Test: Hash patterns with many elements
   - Expected: All elements contribute to hash
   - Test: Hash deeply nested patterns
   - Expected: Nested structures contribute to hash
   - Test: Hash patterns with duplicate values
   - Expected: Different structures produce different hashes

4. **Type Constraint Verification**
   - Test: Attempt to use `hash` with value type without Hashable instance
   - Expected: Compile-time error
   - Test: Use `hash` with value type with Hashable instance
   - Expected: Compiles and executes correctly

5. **Performance Testing**
   - Test: Hash computation performance for large patterns
   - Expected: O(n) time complexity where n is number of nodes
   - Test: HashMap lookup performance
   - Expected: O(1) average-case lookups
   - Test: HashSet membership testing performance
   - Expected: O(1) average-case membership testing

## Test Coverage Requirements

- **Unit tests**: Cover all edge cases (atomic patterns, different element counts, nested structures, different value types, hash consistency)
- **Property-based tests**: Verify hash consistency with `Eq`, hash distribution, recursive hashing, structure-preserving hashing
- **Integration tests**: Verify `HashMap` and `HashSet` integration, edge cases, type constraints, performance
- **Coverage target**: 100% of `Hashable` instance methods covered by tests

## Performance Requirements

- **Hash computation complexity**: O(n) where n is the number of nodes in the pattern structure
- **HashMap lookup**: O(1) average-case, O(n) worst-case (with collisions)
- **HashSet membership**: O(1) average-case, O(n) worst-case (with collisions)
- **Hash distribution**: Collision rate should be below acceptable threshold (e.g., < 1% for random patterns)
- **No performance optimizations needed**: Standard hashable library functions are sufficient

