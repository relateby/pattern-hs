# Feature Specification: Hashable Instance for Pattern

**Feature Branch**: `012-hashable-instance`  
**Created**: 2025-01-27  
**Status**: Draft  
**Input**: User description: "Build 8.5 Hashable Instance as described in @TODO.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Evaluate Use Cases for Hash-Based Containers (Priority: P1)

As a developer using the Pattern library, I need to understand whether hash-based containers (`HashMap`, `HashSet`) provide value beyond ordered containers (`Data.Map`, `Data.Set`), so that I can determine if a Hashable instance should be implemented.

**Why this priority**: Before implementing any feature, we must validate that it solves real problems. Since `Data.Set` and `Data.Map` already work with patterns (via the `Ord` instance), we need to identify clear use cases where hash-based containers provide distinct advantages (e.g., performance, specific algorithms, or library requirements).

**Independent Test**: Can be fully tested by documenting potential use cases, analyzing performance requirements, and determining if hash-based containers provide value beyond ordered containers. Delivers a clear go/no-go decision with documented rationale.

**Acceptance Scenarios**:

1. **Given** a need for pattern lookups and deduplication, **When** evaluating use cases, **Then** at least three concrete use cases are identified that benefit from hash-based containers over ordered containers
2. **Given** identified use cases, **When** analyzing performance requirements, **Then** hash-based lookups provide measurable performance benefits (e.g., O(1) average-case vs O(log n) for ordered containers) for the identified use cases
3. **Given** use case evaluation, **When** reviewing against existing Pattern operations, **Then** Hashable provides distinct value not already available through `Ord`-based containers

---

### User Story 2 - Design Hash Semantics Consistent with Equality (Priority: P2)

As a developer implementing the Hashable instance, I need clear semantics for how patterns are hashed, so that the implementation is consistent with the existing `Eq` instance and provides good hash distribution.

**Why this priority**: Hashable instances must satisfy the fundamental property that equal patterns (according to `Eq`) have the same hash value. Additionally, the hash function should provide good distribution to minimize collisions and enable efficient hash-based operations.

**Independent Test**: Can be fully tested by documenting the hash semantics, verifying they satisfy hash consistency with `Eq` (equal patterns have same hash), and ensuring they provide good distribution. Delivers a clear specification of how patterns are hashed.

**Acceptance Scenarios**:

1. **Given** two patterns that are equal according to `Eq`, **When** computing their hash values, **Then** both patterns produce the same hash value
2. **Given** two patterns that are not equal according to `Eq`, **When** computing their hash values, **Then** the hash values may differ (though collisions are possible, they should be minimized)
3. **Given** the hash semantics, **When** analyzing hash distribution, **Then** patterns with different structures produce different hash values in the majority of cases (minimizing collisions)
4. **Given** the hash semantics, **When** reviewing against structure-preserving model, **Then** the hash function distinguishes patterns with different structures even if they have the same flattened values

---

### User Story 3 - Implement Hashable Instance (Priority: P3)

As a developer using the Pattern library, I need a Hashable instance for Pattern, so that I can use patterns as keys in `HashMap` and elements in `HashSet` for efficient hash-based lookups and deduplication.

**Why this priority**: Once use cases are validated and semantics are designed, the implementation enables the feature. This provides the actual functionality for hash-based pattern operations.

**Independent Test**: Can be fully tested by implementing the Hashable instance, verifying it compiles with the required type constraints, and ensuring it follows the designed semantics. Delivers a working Hashable instance for Pattern.

**Acceptance Scenarios**:

1. **Given** the Hashable instance is implemented, **When** using patterns as keys in `HashMap`, **Then** patterns are correctly hashed and can be used for lookups
2. **Given** patterns with value type `v` that has Hashable instance, **When** using patterns in `HashSet`, **Then** the operation compiles and executes correctly
3. **Given** the Hashable instance, **When** using standard hash-based operations, **Then** they work correctly with Pattern types

---

### User Story 4 - Verify Hash Consistency and Distribution (Priority: P3)

As a developer using the Pattern library, I need confidence that the Hashable instance satisfies hash consistency with `Eq` and provides good hash distribution, so that I can rely on it in production code.

**Why this priority**: Hashable instances must satisfy the fundamental property that equal patterns have the same hash. Additionally, good hash distribution minimizes collisions and enables efficient hash-based operations. Edge cases (nested patterns, large structures) must be handled correctly.

**Independent Test**: Can be fully tested by writing property-based tests for hash consistency with `Eq` and distribution tests for hash collisions. Delivers comprehensive test coverage verifying correctness.

**Acceptance Scenarios**:

1. **Given** any two patterns `p1` and `p2` that are equal according to `Eq`, **When** computing hash values, **Then** `hash p1 == hash p2` holds
2. **Given** patterns with different structures, **When** computing hash values, **Then** hash collisions are minimized (verified through statistical testing)
3. **Given** nested patterns at various depths, **When** computing hash values, **Then** the nested structure contributes to the hash value correctly
4. **Given** patterns with different element counts and structures, **When** computing hash values, **Then** all patterns produce valid hash values without errors

---

### User Story 5 - Integration with Hash-Based Containers (Priority: P3)

As a developer using the Pattern library, I need to use patterns in `HashMap` and `HashSet` for efficient lookups and deduplication, so that I can build performant pattern-based data structures.

**Why this priority**: The primary use case for Hashable instances is enabling hash-based containers. Integration tests verify that patterns work correctly in real-world scenarios with `HashMap` and `HashSet`.

**Independent Test**: Can be fully tested by creating `HashMap (Pattern v) a` and `HashSet (Pattern v)` instances and verifying that patterns are correctly hashed, can be used for lookups, and handle deduplication correctly. Delivers verification of real-world usage.

**Acceptance Scenarios**:

1. **Given** patterns used as keys in a `HashMap`, **When** performing lookups, **Then** patterns are correctly matched using their hash values
2. **Given** a set of patterns, **When** they are inserted into a `HashSet`, **Then** duplicate patterns (equal according to `Eq`) are correctly deduplicated
3. **Given** patterns with the same hash value but different structures, **When** they are inserted into a `HashSet`, **Then** both are retained (hash collisions are handled correctly through equality comparison)
4. **Given** patterns used in hash-based containers, **When** performing operations, **Then** performance is acceptable for the identified use cases

---

### Edge Cases

- What happens when hashing atomic patterns (no elements)? (Should hash value and empty elements list)
- What happens when hashing patterns with deeply nested structures? (Should recursively hash nested structures)
- What happens when hashing patterns with the same flattened values but different structures? (Should distinguish them based on structure, not flattened values)
- What happens when the value type `v` doesn't have a `Hashable` instance? (The `Hashable (Pattern v)` instance should require `Hashable v`, preventing compilation if `v` is not hashable)
- How does hashing handle patterns with many elements? (Should efficiently hash all elements without performance degradation)
- What happens when hash collisions occur? (Should be handled correctly through equality comparison in hash-based containers)
- How does hashing handle patterns with duplicate values? (Should produce different hashes for patterns with different structures even if values are duplicated)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST evaluate and document at least three concrete use cases for hash-based containers before implementation
- **FR-002**: System MUST analyze whether `HashMap`/`HashSet` provide value beyond `Data.Map`/`Data.Set` (which use `Ord`)
- **FR-003**: System MUST design hash semantics that are consistent with the existing `Eq` instance (equal patterns have the same hash)
- **FR-004**: System MUST design hash semantics that provide good distribution to minimize collisions
- **FR-005**: System MUST implement Hashable instance for Pattern with constraint `Hashable v` (value type must have Hashable instance)
- **FR-006**: System MUST ensure hash consistency with `Eq`: for all patterns `p1` and `p2`, if `p1 == p2`, then `hash p1 == hash p2`
- **FR-007**: System MUST use structure-preserving hashing that distinguishes patterns with different structures even if they have the same flattened values
- **FR-008**: System MUST handle edge cases: atomic patterns (empty elements), nested patterns, large structures, duplicate values
- **FR-009**: System MUST provide comprehensive tests verifying hash consistency with `Eq` (100% of equal patterns have same hash)
- **FR-010**: System MUST provide tests verifying hash distribution (minimize collisions through statistical testing)
- **FR-011**: System MUST provide integration tests with `HashMap` and `HashSet` demonstrating real-world usage
- **FR-012**: System MUST document the hash semantics in Haddock documentation with examples

### Key Entities

- **Pattern**: The recursive data structure with `value :: v` and `elements :: [Pattern v]`. The `Hashable` instance must hash patterns based on their structure, hashing value and elements recursively.
- **Hash Function**: A function that maps patterns to hash values, satisfying the property that equal patterns (according to `Eq`) produce the same hash value, while providing good distribution to minimize collisions.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: At least three concrete use cases for hash-based containers are documented and evaluated for providing value beyond ordered containers
- **SC-002**: Hash semantics are fully specified and verified to satisfy hash consistency with `Eq` for all pattern structures (100% of equal patterns have same hash)
- **SC-003**: Hashable instance implementation compiles successfully with `Hashable v` constraint and follows designed semantics
- **SC-004**: All property-based tests for hash consistency with `Eq` pass (100% of generated test cases where `p1 == p2` implies `hash p1 == hash p2`)
- **SC-005**: Hash distribution tests demonstrate minimal collisions (statistical testing shows collision rate below acceptable threshold for identified use cases)
- **SC-006**: All integration tests with `HashMap` and `HashSet` pass, demonstrating correct pattern hashing and lookup behavior
- **SC-007**: All edge case tests pass (atomic patterns, nested patterns, large structures, duplicate values)
- **SC-008**: Haddock documentation includes clear examples demonstrating pattern hashing with at least three different scenarios
- **SC-009**: Performance benchmarks (if applicable) demonstrate that hash-based operations meet performance requirements for identified use cases

## Assumptions

- The value type `v` will have a `Hashable` instance available when Pattern's Hashable instance is used (enforced by type constraint)
- Hash-based containers (`HashMap`, `HashSet`) provide value beyond ordered containers (`Data.Map`, `Data.Set`) for at least some use cases
- Hash consistency with `Eq` is more important than perfect hash distribution (collisions are acceptable if handled correctly)
- Structure-preserving hashing (based on value and elements) aligns with the decorated sequence model
- The hash function should distinguish patterns with different structures even if they have the same flattened values
- Performance requirements for hash-based lookups are acceptable for the identified use cases (O(1) average-case vs O(log n) for ordered containers)

## Dependencies

- **Pattern Core type** (Feature 001) - must exist and be stable
- **Pattern Eq instance** (Feature 002) - required for hash consistency verification
- **Pattern construction functions** (Feature 003) - needed for creating test patterns
- **Pattern Show instance** (Feature 002) - helpful for debugging and documentation examples
- **Pattern Ord instance** (Feature 009) - provides alternative for ordered containers, used for comparison in use case evaluation

## Out of Scope

- Performance optimizations beyond basic correctness - can be addressed in future iterations if needed
- Custom hash functions beyond the standard `Hashable` instance - only one hash semantics will be implemented
- Hash-based algorithms beyond basic container operations - focus is on enabling `HashMap` and `HashSet`
- Perfect hash functions (guaranteed no collisions) - collisions are acceptable if handled correctly through equality comparison
- Hash-based operations that don't require Hashable instance - focus is on standard library integration
