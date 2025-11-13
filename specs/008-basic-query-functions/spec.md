# Feature Specification: Basic Query Functions

**Feature Branch**: `008-basic-query-functions`  
**Created**: 2025-01-27  
**Status**: Draft  
**Input**: User description: "implement basic query functions as described in @TODO.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Query Pattern Sequence Length (Priority: P1)

Developers need to determine how many direct elements a pattern contains in its sequence. This is essential for understanding pattern structure and validating pattern composition.

**Why this priority**: The `length` function provides the most basic structural information about a pattern - how many elements are in its sequence. This is fundamental for pattern validation, iteration, and understanding pattern composition.

**Independent Test**: Can be fully tested by calling `length` on patterns with various element counts (0, 1, 2, many) and verifying it returns the correct count. This delivers immediate value for pattern introspection without requiring other query functions.

**Acceptance Scenarios**:

1. **Given** an atomic pattern (no elements), **When** `length` is called, **Then** it returns 0
2. **Given** a pattern with one element, **When** `length` is called, **Then** it returns 1
3. **Given** a pattern with multiple elements, **When** `length` is called, **Then** it returns the exact count of direct elements
4. **Given** a nested pattern structure, **When** `length` is called on the root pattern, **Then** it returns only the count of direct child elements (not nested descendants)

---

### User Story 2 - Query Total Pattern Size (Priority: P1)

Developers need to count the total number of nodes in a pattern structure, including all nested patterns. This provides a complete picture of pattern complexity.

**Why this priority**: The `size` function enables developers to understand the total complexity of a pattern structure, which is essential for performance analysis, memory estimation, and pattern comparison.

**Independent Test**: Can be fully tested by calling `size` on patterns with various nesting depths and verifying it counts all nodes (root + all descendants). This delivers value for understanding pattern complexity independently.

**Acceptance Scenarios**:

1. **Given** an atomic pattern (single node), **When** `size` is called, **Then** it returns 1
2. **Given** a pattern with direct elements but no nesting, **When** `size` is called, **Then** it returns 1 + the number of direct elements
3. **Given** a deeply nested pattern structure, **When** `size` is called, **Then** it returns the total count of all nodes at all nesting levels
4. **Given** patterns with varying nesting depths in different branches, **When** `size` is called, **Then** it counts all nodes across all branches

---

### User Story 3 - Query Maximum Nesting Depth (Priority: P2)

Developers need to determine the maximum depth of nesting in a pattern structure. This helps understand pattern complexity and validate depth constraints.

**Why this priority**: While important for understanding pattern structure, depth is less frequently needed than length or size. It's valuable for validation and complexity analysis but not essential for basic pattern operations.

**Independent Test**: Can be fully tested by calling `depth` on patterns with various nesting structures and verifying it returns the maximum path depth. This delivers value for depth validation independently.

**Acceptance Scenarios**:

1. **Given** an atomic pattern (no nesting), **When** `depth` is called, **Then** it returns 0 (root only, no nesting levels below root)
2. **Given** a pattern with one level of nesting, **When** `depth` is called, **Then** it returns the appropriate depth value
3. **Given** a pattern with multiple branches of different depths, **When** `depth` is called, **Then** it returns the maximum depth across all branches
4. **Given** a deeply nested pattern structure, **When** `depth` is called, **Then** it returns the maximum nesting depth

---

### User Story 4 - Extract All Pattern Values (Priority: P1)

Developers need to extract all values from a pattern structure as a flat list. This enables value aggregation, analysis, and transformation operations.

**Why this priority**: The `values` function provides essential functionality for working with pattern values. While `toList` from Foldable exists, having an explicit `values` function makes value extraction intentional and clear.

**Independent Test**: Can be fully tested by calling `values` on patterns with various structures and verifying it returns all values in the correct order. This delivers immediate value for value extraction independently.

**Acceptance Scenarios**:

1. **Given** an atomic pattern, **When** `values` is called, **Then** it returns a list containing only the pattern's value
2. **Given** a pattern with multiple elements, **When** `values` is called, **Then** it returns all values from the pattern and its elements
3. **Given** a nested pattern structure, **When** `values` is called, **Then** it returns all values from all nodes at all nesting levels
4. **Given** patterns with varying nesting depths, **When** `values` is called, **Then** it returns values in a consistent, predictable order

---

### User Story 5 - Access Pattern Value (Priority: P1)

Developers need to access the decoration value of a pattern. This is already available through the `value` field accessor, but needs verification and documentation.

**Why this priority**: While the `value` accessor already exists from the data type definition, verifying it works correctly and documenting its behavior is essential for basic pattern operations.

**Independent Test**: Can be fully tested by accessing the `value` field on patterns with various value types and verifying it returns the correct value. This delivers immediate value for pattern value access.

**Acceptance Scenarios**:

1. **Given** a pattern with a string value, **When** the `value` field is accessed, **Then** it returns the string value
2. **Given** a pattern with an integer value, **When** the `value` field is accessed, **Then** it returns the integer value
3. **Given** a pattern with a custom type value, **When** the `value` field is accessed, **Then** it returns the custom type value
4. **Given** nested patterns, **When** the `value` field is accessed on each level, **Then** each returns its respective decoration value

---

### Edge Cases

- What happens when `length` is called on an atomic pattern? (Should return 0)
- What happens when `size` is called on an atomic pattern? (Should return 1)
- What happens when `depth` is called on an atomic pattern? (Should return 0 - root only, no nesting)
- How does `values` handle patterns with duplicate values? (Should return all values including duplicates)
- How does `size` count nodes in patterns with shared substructures? (Each occurrence counts separately)
- How does `depth` handle patterns with branches of different depths? (Returns maximum depth)
- How do query functions handle very deeply nested patterns? (Should work correctly without stack overflow)
- How do query functions handle patterns with many direct elements? (Should work efficiently)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a `length` function that returns the number of direct elements in a pattern's sequence
- **FR-002**: System MUST provide a `size` function that returns the total number of nodes in a pattern structure (including all nested patterns)
- **FR-003**: System MUST provide a `depth` function that returns the maximum nesting depth of a pattern structure
- **FR-004**: System MUST provide a `values` function that returns all values from a pattern structure as a flat list
- **FR-005**: System MUST verify that the `value` field accessor works correctly for accessing pattern decoration values
- **FR-006**: The `length` function MUST return 0 for atomic patterns (patterns with no elements)
- **FR-007**: The `size` function MUST return 1 for atomic patterns (single node)
- **FR-008**: The `depth` function MUST handle patterns with varying branch depths and return the maximum depth
- **FR-009**: The `values` function MUST include values from all nodes at all nesting levels
- **FR-010**: All query functions MUST handle edge cases correctly (empty patterns, single nodes, deep nesting, many elements)
- **FR-011**: All query functions MUST be tested with comprehensive test cases covering various pattern structures
- **FR-012**: All query functions MUST have appropriate documentation explaining their behavior and usage

### Key Entities *(include if feature involves data)*

- **Pattern**: The recursive data structure being queried, containing a value and a list of element patterns
- **Query Result**: The result returned by query functions (Int for length/size/depth, [v] for values)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can query pattern length and receive accurate results for all pattern structures (atomic, with elements, nested) in under 1 millisecond per query
- **SC-002**: Developers can query pattern size and receive accurate total node counts for patterns with up to 1000 nodes in under 10 milliseconds
- **SC-003**: Developers can query pattern depth and receive accurate maximum depth values for patterns with up to 100 levels of nesting in under 5 milliseconds
- **SC-004**: Developers can extract all values from patterns and receive complete, ordered lists of all values in under 10 milliseconds for patterns with up to 1000 nodes
- **SC-005**: All query functions handle edge cases correctly (100% test coverage for atomic patterns, single nodes, deep nesting, many elements)
- **SC-006**: Query functions are documented with clear examples and usage patterns, enabling developers to use them effectively without consulting implementation code

## Assumptions

- Depth counting convention: Depth of 0 for atomic patterns (root only, no nesting), depth of 1 for one level of nesting, etc. This follows standard tree depth conventions where the root is at depth 0.
- The `values` function should return values in a consistent order (pattern's value first, then element values in order, recursively). This matches the behavior of `toList` from Foldable.
- Query functions should be pure (no side effects) and work with patterns of any value type.
- Performance targets assume typical pattern structures; very large or deeply nested patterns may take longer but should still complete successfully.

## Dependencies

- Pattern data type must be fully implemented (already complete from previous features)
- Test infrastructure must be available (already in place)

## Out of Scope

- Advanced query functions (filtering, searching, path queries)
- Query optimization or caching
- Query functions for specific pattern interpretations (graph views, etc.)
- Modification of pattern structure through queries
