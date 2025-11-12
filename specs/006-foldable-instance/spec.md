# Feature Specification: Foldable Instance for Pattern

**Feature Branch**: `006-foldable-instance`  
**Created**: 2025-01-28  
**Status**: Draft  
**Input**: User description: "Pattern should support a Foldable Implementation as described in @TODO.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Aggregate Values from Patterns (Priority: P1) 🎯 MVP

As a developer working with patterns, I need to aggregate values from patterns (sum, product, concatenation, etc.) so that I can compute statistics, combine values, and perform calculations over pattern structures without manually traversing the pattern tree.

**Why this priority**: Aggregation is a fundamental operation for working with collections of data. Without folding capabilities, developers must manually traverse pattern structures to extract and combine values, making common operations like summing integers, concatenating strings, or counting elements unnecessarily complex. This capability enables efficient data processing and analysis over pattern structures.

**Independent Test**: Can be fully tested by applying fold operations to patterns and verifying that: (1) all values are collected correctly, (2) aggregation functions produce correct results, and (3) folding works for atomic patterns, patterns with elements, and nested patterns. This delivers the ability to aggregate pattern values without manual traversal.

**Acceptance Scenarios**:

1. **Given** an atomic pattern with an integer value 5, **When** I fold it with addition starting from 0, **Then** I get 5 as the result
2. **Given** a pattern with multiple integer values [10, 20, 30], **When** I fold all values with addition, **Then** I get the sum of all values (including the pattern's own value)
3. **Given** a pattern with string values, **When** I fold all values with concatenation, **Then** I get a single string containing all values
4. **Given** a deeply nested pattern structure, **When** I fold all values, **Then** I get an aggregation of values from all nesting levels
5. **Given** a pattern with custom type values, **When** I fold values using a custom aggregation function, **Then** I get the correct aggregated result

---

### User Story 2 - Extract Values as a Flat List (Priority: P1)

As a developer working with patterns, I need to extract all values from a pattern as a flat list, so that I can work with pattern values in a familiar list-based interface and use standard list-processing functions.

**Why this priority**: Converting patterns to lists is a common operation that enables interoperability with existing list-based code and libraries. The standard Foldable `toList` provides this functionality, extracting all values as a flat list. Pattern structure is already preserved by the Pattern type itself, so structure-preserving conversion is not needed. Without this capability, developers must manually traverse patterns to build lists, making integration with list-processing code difficult.

**Independent Test**: Can be fully tested by converting patterns to lists and verifying that: (1) the pattern's value is included, (2) all element values are included in a flat list, (3) values appear in the correct order, and (4) conversion works for all pattern structures. This delivers the ability to extract all pattern values as a flat list (standard Foldable behavior).

**Acceptance Scenarios**:

1. **Given** an atomic pattern with value "test", **When** I convert it to a list, **Then** I get a list containing ["test"]
2. **Given** a pattern with multiple elements containing values ["a", "b", "c"], **When** I convert it to a list, **Then** I get a flat list containing all values including the pattern's own value
3. **Given** a nested pattern structure, **When** I convert it to a list, **Then** I get a flat list containing all values from all nesting levels
4. **Given** a pattern with integer values, **When** I convert it to a list, **Then** I get a flat list of integers in the correct order

---

### User Story 2a - Flatten All Values from Patterns (Priority: P1)

As a developer working with patterns, I need to extract all values from a pattern as a flat list (flattening all nesting levels) so that I can aggregate values, compute statistics, and work with all values in a single flat list when structural information is not needed.

**Why this priority**: Sometimes developers need all values in a flat list regardless of structure, such as when computing sums, counting elements, or passing to functions that expect flat lists. The standard `toList()` already provides this functionality, but `flatten()` can be provided as an explicit alias for clarity or future extensibility.

**Independent Test**: Can be fully tested by flattening patterns and verifying that: (1) all values from all nesting levels are included, (2) values appear in the correct order, (3) the result is a flat list (no nested lists), and (4) flattening works for all pattern structures. This delivers the ability to extract all pattern values as a flat list.

**Acceptance Scenarios**:

1. **Given** an atomic pattern with value "test", **When** I flatten it, **Then** I get a list containing ["test"]
2. **Given** a pattern with multiple elements containing values ["a", "b", "c"], **When** I flatten it, **Then** I get a flat list containing all values including the pattern's own value
3. **Given** a nested pattern structure, **When** I flatten it, **Then** I get a flat list containing values from all nesting levels
4. **Given** a pattern with integer values, **When** I flatten it, **Then** I get a flat list of integers in the correct order

---

### User Story 2b - Extract Pattern as Tuple (Priority: P2)

As a developer working with patterns, I need to extract a pattern as a tuple preserving its structure, so that I can work with the pattern's value and elements separately while maintaining the structural relationship.

**Why this priority**: Converting patterns to tuples provides direct access to the pattern's value and elements list without flattening. This enables structure-preserving operations and makes the pattern's composition explicit. The tuple representation `(v, [Pattern v])` directly reflects the Pattern's structure.

**Independent Test**: Can be fully tested by converting patterns to tuples and verifying that: (1) the pattern's value is the first element of the tuple, (2) the pattern's elements list is the second element of the tuple, (3) the structure is preserved (elements remain as Pattern values), and (4) conversion works for all pattern structures. This delivers the ability to extract patterns as tuples preserving structure.

**Acceptance Scenarios**:

1. **Given** an atomic pattern with value "test", **When** I convert it to a tuple, **Then** I get a tuple containing ("test", [])
2. **Given** a pattern with multiple elements, **When** I convert it to a tuple, **Then** I get a tuple containing the pattern's value and the list of element patterns
3. **Given** a nested pattern structure, **When** I convert it to a tuple, **Then** I get a tuple where the second element contains nested Pattern structures
4. **Given** a pattern with integer values, **When** I convert it to a tuple, **Then** I get a tuple with integer value and list of Pattern Int

---

### User Story 3 - Fold with Right-Associative Operations (Priority: P1)

As a developer using pattern folding, I need to fold patterns using right-associative operations (foldr) so that I can correctly process values in right-to-left order and handle operations that depend on processing order.

**Why this priority**: Right-associative folding is essential for operations where order matters, such as building lists, constructing data structures, or applying functions that depend on processing order. Without foldr, developers cannot reliably process patterns in right-to-left order.

**Independent Test**: Can be fully tested by applying foldr to patterns and verifying that: (1) values are processed in the correct order, (2) right-associative operations produce correct results, and (3) foldr works for all pattern structures. This delivers reliable right-associative folding over patterns.

**Acceptance Scenarios**:

1. **Given** a pattern with values [1, 2, 3], **When** I fold right with addition, **Then** I get the correct sum regardless of processing order
2. **Given** a pattern with string values, **When** I fold right to build a list, **Then** values are collected in the correct order
3. **Given** a nested pattern structure, **When** I fold right, **Then** values from all levels are processed correctly

---

### User Story 4 - Fold with Left-Associative Operations (Priority: P1)

As a developer using pattern folding, I need to fold patterns using left-associative operations (foldl) so that I can correctly process values in left-to-right order and handle operations that require strict left-to-right evaluation.

**Why this priority**: Left-associative folding is essential for operations where strict left-to-right processing is required, such as accumulating state, computing running totals, or applying functions that depend on processing order. Without foldl, developers cannot reliably process patterns in left-to-right order.

**Independent Test**: Can be fully tested by applying foldl to patterns and verifying that: (1) values are processed in left-to-right order, (2) left-associative operations produce correct results, and (3) foldl works for all pattern structures. This delivers reliable left-associative folding over patterns.

**Acceptance Scenarios**:

1. **Given** a pattern with values [1, 2, 3], **When** I fold left with addition, **Then** I get the correct sum with left-to-right processing
2. **Given** a pattern with integer values, **When** I fold left to compute a running total, **Then** values are processed in the correct order
3. **Given** a nested pattern structure, **When** I fold left, **Then** values from all levels are processed in left-to-right order

---

### User Story 5 - Map Values to Monoids and Combine (Priority: P2)

As a developer working with patterns, I need to map values to monoids and combine them (foldMap) so that I can efficiently aggregate values using monoid operations without explicitly writing fold functions.

**Why this priority**: foldMap is a powerful abstraction that enables efficient aggregation using monoid operations. While foldr and foldl provide explicit control, foldMap provides a more declarative approach for common aggregation patterns. This enables cleaner code for operations like summing, concatenating, or counting.

**Independent Test**: Can be fully tested by applying foldMap to patterns and verifying that: (1) values are mapped to monoids correctly, (2) monoid combination produces correct results, and (3) foldMap works for all pattern structures. This delivers efficient monoid-based aggregation over patterns.

**Acceptance Scenarios**:

1. **Given** a pattern with integer values, **When** I foldMap with Sum monoid, **Then** I get the sum of all values
2. **Given** a pattern with string values, **When** I foldMap with list monoid, **Then** I get a concatenated string of all values
3. **Given** a pattern with boolean values, **When** I foldMap with All monoid, **Then** I get the logical AND of all values
4. **Given** a nested pattern structure, **When** I foldMap, **Then** values from all levels are mapped and combined correctly

---

### Edge Cases

- What happens when folding an atomic pattern (pattern with no elements)?
- What happens when folding a pattern with an empty elements list?
- What happens when folding a pattern with a single element (singular pattern)?
- What happens when folding a pattern with many elements?
- How does folding handle patterns with different value types (strings, integers, custom types)?
- What happens when the folding operation is not commutative (order matters)?
- How does folding preserve or respect element order in patterns with multiple elements?
- What happens when folding nested patterns with varying depths?
- How does folding handle patterns where the aggregation function produces different types?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a way to fold over all values in a pattern structure
- **FR-002**: System MUST support right-associative folding (foldr) that processes values in correct order
- **FR-003**: System MUST support left-associative folding (foldl) that processes values in correct order
- **FR-004**: System MUST support mapping values to monoids and combining them (foldMap)
- **FR-005**: System MUST provide a way to extract values from a pattern as a flat list (toList, standard Foldable behavior)
- **FR-005a**: System MUST provide a way to extract all values from a pattern as a flat list (flatten)
- **FR-005b**: System MUST provide a way to extract a pattern as a tuple preserving structure (toTuple)
- **FR-006**: System MUST include all values in folding operations, including the pattern's own value and all element values
- **FR-007**: System MUST process values from all nesting levels when folding nested patterns
- **FR-008**: System MUST work with patterns containing any value type (strings, integers, custom types)
- **FR-009**: System MUST handle atomic patterns (patterns with no elements) correctly
- **FR-010**: System MUST handle patterns with elements correctly
- **FR-011**: System MUST handle deeply nested patterns correctly
- **FR-012**: System MUST preserve or respect element order during folding operations
- **FR-013**: System MUST satisfy foldable laws and properties expected by standard foldable interfaces

### Key Entities

- **Pattern**: A recursive data structure representing a decorated sequence. Contains a value (decoration) and a list of pattern elements. The elements form the pattern itself; the value describes what kind of pattern it is.
- **Folding Operation**: An operation that aggregates values from a pattern structure using a binary function and an initial value (or monoid). Processes all values in the pattern, including the pattern's own value and all element values at all nesting levels.
- **Foldable Instance**: A typeclass instance that enables folding operations over pattern structures. Provides foldr, foldl, foldMap, toList, and other standard foldable operations.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can fold over any pattern structure (atomic, with elements, nested) to aggregate values in a single operation without manual traversal
- **SC-002**: foldr correctly processes all values from patterns with 100% accuracy, including values from all nesting levels
- **SC-003**: foldl correctly processes all values from patterns with 100% accuracy, including values from all nesting levels, in left-to-right order
- **SC-004**: foldMap correctly maps values to monoids and combines them with 100% accuracy for all pattern structures
- **SC-005**: toList correctly extracts all values from patterns as a flat list with 100% accuracy (standard Foldable behavior)
- **SC-005a**: flatten correctly extracts all values from patterns as flat lists with 100% accuracy, including values from all nesting levels
- **SC-005b**: toTuple correctly extracts patterns as tuples with 100% accuracy, preserving the pattern's value and elements structure
- **SC-006**: Folding operations work correctly for patterns with values of any type (strings, integers, custom types) with 100% success rate
- **SC-007**: Folding of nested patterns (3+ levels deep) completes successfully and correctly processes values at all levels
- **SC-008**: Element order is preserved or respected during folding operations with 100% consistency
- **SC-009**: Foldable instance satisfies expected laws and properties for all pattern structures

## Assumptions

- Folding operations process all values in the pattern structure, including the pattern's own value and all element values
- The order of value processing follows standard Foldable semantics (foldr processes right-to-left, foldl processes left-to-right)
- foldMap uses monoid operations to combine mapped values efficiently
- The implementation will follow standard foldable patterns for recursive data structures
- Property-based testing will be used to verify foldable operations across many pattern structures
- The specification focuses on developer needs (folding capabilities) rather than implementation details
- Folding operations are pure (no side effects) - this is standard for foldable instances
- The foldable implementation will work with any value type that supports the folding operations (no additional constraints beyond what standard foldable interfaces require)

## Dependencies

- **Prerequisites**: Pattern data type must exist (✅ Complete - Feature 1)
- **Prerequisites**: Pattern must have Eq instance for testing (✅ Complete - Feature 2)
- **Prerequisites**: Pattern must have Show instance for debugging (✅ Complete - Feature 2)
- **Prerequisites**: Pattern must have Functor instance (✅ Complete - Feature 4)
- **No blocking dependencies**: This feature can be implemented independently

## Design Decisions

### toList() Behavior: Standard Foldable

**Decision**: `toList()` follows standard Foldable behavior, extracting all values as a flat list `[a]`.

**Rationale**:
- Consistency with standard Foldable behavior (standard `toList` flattens values)
- Pattern structure is already preserved by the Pattern type itself
- Enables interoperability with standard list-processing functions
- Simple and predictable behavior that matches Haskell conventions

**Behavior**:
- Atomic pattern: Returns `[value]` (single-element list)
- Pattern with elements: Returns `[value, elem1_value, elem2_value, ...]` (flat list with all values)
- Nested patterns: Returns flat list with all values from all nesting levels

**Example**:
```haskell
-- Pattern structure
p = patternWith "root" [patternWith "inner" [pattern "value"]]

-- toList extracts all values as flat list (standard Foldable behavior)
toList p
-- ["root", "inner", "value"]
```

### flatten() Function: Explicit Flattening

**Decision**: Add `flatten()` function that extracts all values as a flat list, explicitly flattening all nesting levels.

**Rationale**:
- Provides explicit flattening when structure is not needed
- Complements `toList()` which preserves structure
- Useful for aggregation operations (sum, count, etc.)
- Makes flattening intentional and clear

**Behavior**:
- Extracts all values from all nesting levels into a single flat list
- Includes pattern's own value and all element values recursively
- Result is always a flat list (no nested lists)

**Example**:
```haskell
-- Pattern structure
p = patternWith "root" [patternWith "inner" [pattern "value"]]

-- flatten extracts all values as flat list
flatten p
-- ["root", "inner", "value"]
```

### toTuple() Function: Structure-Preserving Tuple Extraction

**Decision**: Add `toTuple()` function that extracts a pattern as a tuple `(v, [Pattern v])`, preserving the pattern's structure.

**Rationale**:
- Directly reflects the Pattern's structure: value + list of patterns
- Enables structure-preserving operations without flattening
- Provides explicit access to pattern's value and elements separately
- Makes pattern composition explicit and clear

**Behavior**:
- Returns a tuple `(v, [Pattern v])` where the first element is the pattern's value and the second is the list of element patterns
- Preserves all structural information (elements remain as Pattern values, not flattened)
- Works for all pattern structures (atomic, with elements, nested)

**Example**:
```haskell
-- Pattern structure
p = patternWith "root" [patternWith "inner" [pattern "value"]]

-- toTuple preserves structure as tuple
toTuple p
-- ("root", [Pattern {value = "inner", elements = [Pattern {value = "value", elements = []}]}])
```

**Type Signature**:
```haskell
toTuple :: Pattern v -> (v, [Pattern v])
toTuple (Pattern v els) = (v, els)
```

## Out of Scope

- Traversable instance (deferred to Feature 6)
- Custom folding functions beyond standard Foldable interface
- Performance optimization for large patterns (basic implementation sufficient)
- Specialized folding operations for specific use cases
- Folding operations that modify pattern structure (only values are processed, structure is not modified)
