# Research: Hashable Instance for Pattern

**Feature**: 012-hashable-instance  
**Date**: 2025-01-27  
**Status**: Complete

## Research Objectives

1. Identify concrete use cases for hash-based containers with patterns
2. Evaluate whether `HashMap`/`HashSet` provide value beyond `Data.Map`/`Data.Set`
3. Design hash semantics consistent with `Eq` instance
4. Determine hash distribution strategy
5. Review Haskell best practices for `Hashable` instances

## Use Case Analysis

### Use Case 1: High-Performance Pattern Lookups

**Scenario**: Applications requiring frequent pattern lookups where O(1) average-case performance is critical.

**Example**:
- Pattern matching engine that needs to check if a pattern exists in a large collection
- Graph algorithms that index patterns by structure for fast membership testing
- Pattern deduplication in streaming data processing

**Performance Benefit**: `HashMap` provides O(1) average-case lookups vs O(log n) for `Data.Map`, significant improvement for large collections (1000+ patterns).

**Value Beyond Ordered Containers**: ✅ When ordering is not needed, hash-based containers provide better performance for lookup-heavy operations.

### Use Case 2: Pattern Deduplication in Large Collections

**Scenario**: Processing large collections of patterns where deduplication is needed but ordering is not required.

**Example**:
- Collecting unique patterns from a stream of pattern matches
- Building pattern indexes without needing sorted order
- Pattern caching where fast membership testing is more important than ordering

**Performance Benefit**: `HashSet` provides O(1) average-case membership testing vs O(log n) for `Data.Set`, enabling efficient deduplication of large pattern collections.

**Value Beyond Ordered Containers**: ✅ When ordering is not needed, hash-based containers provide better performance for membership testing and deduplication.

### Use Case 3: Integration with Hash-Based Libraries

**Scenario**: Using patterns with libraries that require `Hashable` instances or work better with hash-based containers.

**Example**:
- Using patterns as keys in third-party libraries that require `Hashable`
- Interfacing with hash-based data structures from other packages
- Pattern-based memoization using hash-based caches

**Value Beyond Ordered Containers**: ✅ Some libraries and algorithms specifically require or benefit from hash-based containers, making `Hashable` instance necessary for integration.

## Comparison with Ordered Containers

### When to Use `Data.Map`/`Data.Set` (Ord-based)

- **Ordering needed**: When patterns need to be sorted or accessed in order
- **Range queries**: When range-based lookups are required
- **Small collections**: When collection size is small (<100 patterns), overhead difference is negligible
- **Deterministic iteration**: When iteration order must be deterministic and sorted

### When to Use `HashMap`/`HashSet` (Hashable-based)

- **Lookup performance**: When O(1) average-case lookups are critical for large collections
- **No ordering needed**: When patterns don't need to be sorted
- **Membership testing**: When frequent membership testing is the primary operation
- **Large collections**: When collection size is large (1000+ patterns), performance difference becomes significant
- **Library integration**: When libraries require `Hashable` instances

**Conclusion**: Hash-based containers provide distinct value for performance-critical lookup operations and library integration, even when ordered containers are available.

## Hash Semantics Design

### Decision: Structure-Preserving Hashing

**Chosen Approach**: Hash patterns based on their structure (value and elements recursively), ensuring that equal patterns (according to `Eq`) produce the same hash value.

**Hash Function Design**:
```haskell
hashWithSalt s (Pattern v els) = 
  s `hashWithSalt` v `hashWithSalt` els
```

Where `els` is hashed as a list, which recursively hashes each element pattern.

**Rationale**:
- Ensures hash consistency with `Eq`: equal patterns have same structure, so same hash
- Distinguishes patterns with different structures even if they have same flattened values
- Aligns with structure-preserving semantics of Pattern type
- Standard pattern for recursive types in Haskell

**Alternatives Considered**:
- **Flattened value hashing**: Hash based on `toList` (all values). Rejected - would produce same hash for patterns with different structures but same flattened values, violating structure-preserving semantics.
- **Value-only hashing**: Hash only the root value. Rejected - would produce same hash for patterns with same value but different elements, violating hash consistency with `Eq`.
- **Custom hash combination**: Use custom hash combination function. Rejected - standard `hashWithSalt` pattern is sufficient and follows Haskell conventions.

### Decision: Hash Consistency with Eq

**Requirement**: For all patterns `p1` and `p2`, if `p1 == p2`, then `hash p1 == hash p2`.

**Verification Strategy**:
- Property-based tests: Generate patterns, verify that equal patterns have same hash
- Test all pattern structures: atomic, with elements, nested, different depths
- Test with different value types (String, Int, custom types with Hashable instances)

**Rationale**:
- Fundamental requirement for `Hashable` instances
- Enables correct behavior in hash-based containers (collisions handled through equality)
- Standard property that all `Hashable` instances must satisfy

### Decision: Hash Distribution

**Goal**: Minimize hash collisions to enable efficient hash-based operations.

**Strategy**:
- Use structure-preserving hashing (value + elements) to maximize differentiation
- Rely on `hashable` library's hash combination functions for good distribution
- Test hash distribution statistically to verify collision rate is acceptable

**Collision Handling**:
- Hash collisions are acceptable if handled correctly through equality comparison
- Hash-based containers (`HashMap`, `HashSet`) automatically handle collisions using `Eq`
- As long as hash consistency with `Eq` is maintained, collisions don't cause incorrect behavior

**Verification Strategy**:
- Statistical testing: Generate many patterns, measure collision rate
- Test with patterns of different structures to ensure good distribution
- Verify that patterns with different structures produce different hashes in majority of cases

## Alignment with Pattern Model

### Structure-Preserving Semantics

**Verification**: ✅ Hash function preserves structure by hashing both value and elements recursively. Patterns with different structures produce different hashes, even if they have the same flattened values.

### Consistency with Eq Instance

**Verification**: ✅ Hash function is designed to be consistent with `Eq` instance. Since `Eq` compares value first, then elements recursively, the hash function follows the same structure, ensuring equal patterns produce the same hash.

### Recursive Hashing

**Verification**: ✅ Hash function recursively hashes nested patterns, ensuring that deeply nested structures contribute to the hash value correctly. This aligns with the recursive nature of the Pattern type.

## Haskell Best Practices

### Hashable Instance Pattern

**Standard Pattern**: For recursive types, hash by hashing components recursively:

```haskell
instance Hashable v => Hashable (Pattern v) where
  hashWithSalt s (Pattern v els) = 
    s `hashWithSalt` v `hashWithSalt` els
```

**Rationale**:
- Follows standard Haskell pattern for recursive types
- Uses `hashWithSalt` for proper hash combination
- Clear and straightforward implementation
- Easy to verify hash consistency with `Eq`

### Type Constraint

**Chosen**: `Hashable v => Hashable (Pattern v)`

**Rationale**:
- Value type must have `Hashable` instance to hash values
- Standard pattern for parameterized types
- Type-safe: compiler enforces constraint
- Consistent with other typeclass instances (Eq, Ord, Semigroup, Monoid)

### Hashable Library Integration

**Dependency**: `hashable ^>=1.4`

**Rationale**:
- Standard library for `Hashable` typeclass
- Provides `hashWithSalt` function for proper hash combination
- Well-tested and widely used
- Compatible with GHC 9.8.4 and 9.10.3

### Documentation Requirements

**Required**:
- Explain hash semantics clearly (structure-preserving, recursive)
- Document hash consistency with `Eq` instance
- Provide examples showing hash-based container usage
- Explain when to use hash-based vs ordered containers

## Implementation Strategy

### Phase 1: Evaluation and Design
- Document use cases (this research)
- Design hash semantics (this research)
- Verify alignment with Pattern model (this research)

### Phase 2: Implementation
- Implement `Hashable` instance in `Pattern.Core`
- Add comprehensive Haddock documentation
- Write unit tests for edge cases

### Phase 3: Verification
- Write property-based tests for hash consistency with `Eq`
- Write distribution tests to verify collision rate
- Test with different value types
- Verify `HashMap` and `HashSet` integration

## Conclusion

**Decision**: ✅ Proceed with `Hashable` instance implementation

**Rationale**:
1. Three concrete use cases identified (high-performance lookups, deduplication, library integration)
2. Hash semantics clearly defined and align with Pattern model
3. Provides distinct value beyond ordered containers for performance-critical operations
4. Follows standard Haskell patterns and best practices
5. Satisfies hash consistency with `Eq` requirement
6. Hash distribution strategy defined with collision handling approach

**Next Steps**: Proceed to Phase 1 design (data-model.md, contracts, quickstart.md)

