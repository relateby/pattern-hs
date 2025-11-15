# Feature Specification: Comonad Instance for Pattern

**Feature Branch**: `014-comonad-instance`  
**Created**: 2025-01-28  
**Status**: Draft  
**Input**: User description: "Work on Feature 10 (Comonad Instance) as described in @TODO.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Extract Values from Patterns (Priority: P1) 🎯 MVP

As a developer working with patterns, I need to extract the decoration value from a pattern so that I can access the value at the focus point of a pattern structure, enabling context-aware computations that start from the pattern's decoration value.

**Why this priority**: The `extract` operation is fundamental to the Comonad instance. It provides the core capability of extracting the value at the focus point, which is essential for all context-aware computations. Without `extract`, developers cannot access the decoration value in a comonadic context, making the Comonad instance incomplete.

**Independent Test**: Can be fully tested by implementing `extract` that returns the pattern's decoration value, testing on atomic patterns, patterns with elements, and nested patterns. This delivers the ability to extract values from patterns in a comonadic context.

**Acceptance Scenarios**:

1. **Given** an atomic pattern with value `5`, **When** I call `extract` on it, **Then** I get `5` (the decoration value)
2. **Given** a pattern with elements and value `"test"`, **When** I call `extract` on it, **Then** I get `"test"` (the decoration value, not the element values)
3. **Given** a deeply nested pattern structure, **When** I call `extract` on it, **Then** I get the root pattern's decoration value
4. **Given** patterns with different value types (String, Int, custom types), **When** I call `extract` on them, **Then** I get the appropriate value type for each pattern

---

### User Story 2 - Create Context-Aware Computations (Priority: P1)

As a developer working with patterns, I need to perform computations that have access to the full structural context (parent, siblings, depth, indices) around each value, so that I can compute results based on position and structure, not just the value itself.

**Why this priority**: The `extend` operation enables context-aware computations where functions have access to the entire pattern structure at each position. This extends beyond `Foldable` (which only provides values) to enable computations that consider structural context, depth, position, and relationships between pattern elements. This is the core value proposition of the Comonad instance.

**Independent Test**: Can be fully tested by implementing `extend` that applies a context-aware function to each position in a pattern, creating a new pattern where each position contains the result of applying the function to the pattern structure at that position. This delivers the ability to perform context-aware transformations.

**Acceptance Scenarios**:

1. **Given** a pattern and a context-aware function (e.g., computing depth at each position), **When** I call `extend` with the function, **Then** I get a new pattern where each position contains the result of applying the function to the pattern structure at that position
2. **Given** a pattern and a function that computes the size of the subtree at each position, **When** I call `extend` with the function, **Then** I get a pattern where each position contains the size of its subtree
3. **Given** a pattern and a function that computes the indices from root at each position, **When** I call `extend` with the function, **Then** I get a pattern where each position contains its indices from root
4. **Given** a nested pattern structure, **When** I call `extend` with a context-aware function, **Then** the function is applied at all positions in the structure, including root and all nested positions
5. **Given** an atomic pattern, **When** I call `extend` with a context-aware function, **Then** the function is applied to the atomic pattern structure

---

### User Story 3 - Create Patterns of Contexts (Priority: P1)

As a developer working with patterns, I need to create a pattern where each position contains the full pattern structure focused at that position, so that I can work with context structures and enable context-aware computations through `extend`.

**Why this priority**: The `duplicate` operation creates a pattern of contexts, where each position contains the full pattern structure focused at that position. This is essential for enabling `extend` operations and provides the foundation for context-aware computations. Without `duplicate`, developers cannot create context structures needed for comonadic operations.

**Independent Test**: Can be fully tested by implementing `duplicate` that creates a pattern where each position contains the pattern structure focused at that position, testing on atomic patterns, patterns with elements, and nested patterns. This delivers the ability to create context structures.

**Acceptance Scenarios**:

1. **Given** an atomic pattern, **When** I call `duplicate` on it, **Then** I get a pattern containing the atomic pattern itself (context at the only position)
2. **Given** a pattern with elements, **When** I call `duplicate` on it, **Then** I get a pattern where the root contains the full pattern, and each element contains the pattern structure focused at that element's position
3. **Given** a nested pattern structure, **When** I call `duplicate` on it, **Then** I get a pattern where each position (at all nesting levels) contains the pattern structure focused at that position
4. **Given** any pattern, **When** I call `extract` on the result of `duplicate`, **Then** I get the original pattern back (verifying context structure)

---

### User Story 4 - Verify Comonad Laws (Priority: P1)

As a developer using comonadic pattern operations, I need confidence that the Comonad instance follows mathematical comonad laws (extract-extend, extend-extract, extend composition) so that I can reason about pattern operations and compose them safely without unexpected behavior.

**Why this priority**: Comonad laws are fundamental mathematical properties that ensure operations behave predictably. Without these laws, pattern operations could produce inconsistent results, making the feature unreliable for production use. These laws enable safe composition of operations and predictable behavior when working with context-aware computations.

**Independent Test**: Can be fully tested by verifying that: (1) extract-extend law holds (`extract . extend f = f`), (2) extend-extract law holds (`extend extract = id`), and (3) extend composition law holds (`extend f . extend g = extend (f . extend g)`). This delivers mathematical correctness guarantees for comonadic pattern operations.

**Acceptance Scenarios**:

1. **Given** any pattern and any context-aware function f, **When** I apply `extract . extend f`, **Then** I get the same result as applying `f` directly (extract-extend law)
2. **Given** any pattern, **When** I apply `extend extract` to it, **Then** I get the same pattern back (extend-extract law, identity)
3. **Given** any pattern and any two context-aware functions f and g, **When** I apply `extend f . extend g`, **Then** I get the same result as applying `extend (f . extend g)` (extend composition law)
4. **Given** patterns of various structures (atomic, with elements, nested), **When** I verify comonad laws hold, **Then** all laws pass for all pattern structures
5. **Given** context-aware functions of various types (depth, size, indices, custom computations), **When** I verify comonad laws hold, **Then** all laws pass for all function types

---

### User Story 5 - Context-Aware Operations (Priority: P2)

As a developer working with patterns, I need convenient helper functions for common context-aware operations (depth, size, indices) so that I can easily compute structural metadata at each position without manually implementing context-aware functions.

**Why this priority**: While context-aware operations are useful, they are secondary to the core Comonad instance (User Stories 1-4). Helper functions like `depthAt`, `sizeAt`, and `indicesAt` provide convenience but can be implemented using `extend` with custom functions. The core Comonad instance must be complete and verified before adding convenience functions.

**Independent Test**: Can be fully tested by implementing helper functions that use `extend` to compute common context information (depth, size, indices) at each position, testing on various pattern structures. This delivers convenient access to common context-aware computations.

**Acceptance Scenarios**:

1. **Given** a pattern structure, **When** I call `depthAt` on it, **Then** I get a pattern where each position contains its depth (nesting level) from the root
2. **Given** a pattern structure, **When** I call `sizeAt` on it, **Then** I get a pattern where each position contains the size (total nodes) of its subtree
3. **Given** a pattern structure, **When** I call `indicesAt` on it, **Then** I get a pattern where each position contains its indices from root (list of indices)
4. **Given** an atomic pattern, **When** I call context-aware operations, **Then** operations return appropriate results (depth 0, size 1, indices [])
5. **Given** a nested pattern structure, **When** I call context-aware operations, **Then** operations return correct results at all positions including root and nested positions

---

### Edge Cases

- What happens when calling `extract` on an atomic pattern?
- What happens when calling `extract` on a pattern with elements?
- What happens when calling `extend` with a function that accesses parent context on the root pattern?
- What happens when calling `extend` with a function that accesses sibling context on an atomic pattern?
- What happens when calling `duplicate` on an atomic pattern?
- What happens when calling `duplicate` on a deeply nested pattern (100+ levels)?
- What happens when calling `extend` with functions that transform value types (e.g., Pattern Int -> String)?
- What happens when calling context-aware operations on patterns with empty elements lists?
- What happens when calling context-aware operations on patterns with many elements (100+ elements)?
- How do comonad operations handle patterns with different value types (String, Int, custom types)?
- What happens when `extend` is called with functions that access structural information (depth, size, indices) at each position?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide `extract :: Pattern v -> v` function that extracts the decoration value from a pattern
- **FR-002**: System MUST provide `duplicate :: Pattern v -> Pattern (Pattern v)` function that creates a pattern where each position contains the pattern structure focused at that position
- **FR-003**: System MUST provide `extend :: (Pattern v -> w) -> Pattern v -> Pattern w` function that applies a context-aware function to each position in a pattern, creating a new pattern where each position contains the result
- **FR-004**: System MUST satisfy the comonad extract-extend law: `extract . extend f = f` for all context-aware functions f
- **FR-005**: System MUST satisfy the comonad extend-extract law: `extend extract = id` for all patterns
- **FR-006**: System MUST satisfy the comonad extend composition law: `extend f . extend g = extend (f . extend g)` for all context-aware functions f and g
- **FR-007**: System MUST implement Comonad instance for Pattern type
- **FR-008**: System MUST handle atomic patterns (patterns with no elements) correctly in all comonad operations
- **FR-009**: System MUST handle patterns with elements correctly in all comonad operations
- **FR-010**: System MUST handle deeply nested patterns correctly in all comonad operations
- **FR-011**: System MUST work with patterns containing values of any type (strings, integers, custom types) in comonad operations
- **FR-012**: System MUST provide comprehensive tests verifying comonad laws with property-based testing
- **FR-013**: System MUST provide tests verifying extract on atomic patterns, patterns with elements, and nested patterns
- **FR-014**: System MUST provide tests verifying extend with various context-aware functions (depth, size, indices, custom computations)
- **FR-015**: System MUST provide tests verifying duplicate creates correct context structures for all pattern types
- **FR-016**: System MUST provide tests for edge cases: empty patterns, deeply nested patterns, single element patterns
- **FR-017**: System SHOULD provide helper functions for common context-aware operations: `depthAt :: Pattern v -> Pattern Int` (depth at each position)
- **FR-018**: System SHOULD provide helper functions for common context-aware operations: `sizeAt :: Pattern v -> Pattern Int` (size of subtree at each position)
- **FR-019**: System SHOULD provide helper functions for common context-aware operations: `indicesAt :: Pattern v -> Pattern [Int]` (indices from root at each position)
- **FR-020**: System SHOULD provide tests verifying context-aware operations produce correct results for all pattern structures
- **FR-021**: System MUST document comonad operations with clear examples showing usage patterns and expected behavior

### Key Entities

- **Pattern**: A recursive data structure representing a decorated sequence. Contains a value (decoration) and a list of pattern elements. The elements form the pattern itself; the value describes what kind of pattern it is.
- **Context**: The full structural information available at a position in a pattern, including the pattern structure focused at that position, parent context, sibling context, depth, indices, and size.
- **Context-Aware Function**: A function that takes a Pattern and returns a result based on the full structural context at that position, not just the value. Examples include functions that compute depth, size, indices, or custom structural metadata.
- **Extract Operation**: An operation that extracts the decoration value from a pattern, providing the value at the focus point.
- **Duplicate Operation**: An operation that creates a pattern where each position contains the full pattern structure focused at that position, enabling context-aware computations.
- **Extend Operation**: An operation that applies a context-aware function to each position in a pattern, creating a new pattern where each position contains the result of applying the function to the pattern structure at that position.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can extract decoration values from patterns using `extract`, enabling access to values at focus points in comonadic contexts
- **SC-002**: Developers can create context-aware computations using `extend`, enabling transformations based on structural context (depth, size, indices, relationships)
- **SC-003**: Developers can create patterns of contexts using `duplicate`, enabling context structure creation for comonadic operations
- **SC-004**: Comonad extract-extend law holds for 100% of test cases across all pattern structures and context-aware functions
- **SC-005**: Comonad extend-extract law holds for 100% of test cases across all pattern structures
- **SC-006**: Comonad extend composition law holds for 100% of test cases across all pattern structures and context-aware functions
- **SC-007**: Comonad operations work correctly for patterns with values of any type (strings, integers, custom types) with 100% success rate
- **SC-008**: Comonad operations work correctly for atomic patterns, patterns with elements, and nested patterns with 100% success rate
- **SC-009**: Comonad operations handle edge cases (empty patterns, deeply nested patterns, single element patterns) according to defined semantics with 100% success rate
- **SC-010**: Context-aware helper functions (depthAt, sizeAt, indicesAt) produce correct results for all pattern structures with 100% success rate (if implemented)
- **SC-011**: All comonad operations are documented with clear examples showing usage patterns, expected behavior, and relationship to Foldable/Traversable instances

## Assumptions

- Comonad operations will follow standard Haskell Comonad instance patterns
- The `extract` function will extract the pattern's decoration value (the `value` field)
- The `duplicate` function will create a pattern where each position contains the pattern structure focused at that position
- The `extend` function will apply context-aware functions recursively to all positions in the pattern structure
- Context-aware functions will have access to the full pattern structure at each position, enabling computations based on depth, size, indices, parent, siblings, and other structural information
- Comonad operations will be applied recursively to nested patterns, similar to how Functor and Traversable operations work
- Property-based testing will be used to verify comonad laws across many pattern structures and context-aware functions
- The implementation will handle type transformations (e.g., applying `Pattern Int -> String` functions via `extend` produces `Pattern String`)
- Context-aware operations (depthAt, sizeAt, indicesAt) are optional convenience functions that can be implemented using `extend` with custom functions
- Comonad operations extend beyond Foldable (which only provides values) to enable computations with access to full structural context

## Dependencies

- **Prerequisites**: Pattern data type must exist (✅ Complete - Feature 1)
- **Prerequisites**: Pattern must have Functor instance (✅ Complete - Feature 4)
- **Prerequisites**: Pattern must have Foldable instance (✅ Complete - Feature 5)
- **Prerequisites**: Pattern must have Traversable instance (✅ Complete - Feature 6)
- **Prerequisites**: Pattern must have Eq instance for testing (✅ Complete - Feature 2)
- **Prerequisites**: Pattern must have Show instance for debugging (✅ Complete - Feature 2)
- **Prerequisites**: Pattern must have basic query functions (size, depth) for context-aware operations (✅ Complete - Feature 7)
- **No blocking dependencies**: This feature can be implemented independently after basic Pattern type and typeclass instances are complete

## Notes

- Comonad enables context-aware folding where computations have access to the full structural context (parent, siblings, depth, indices) around each value, not just the value itself. This extends beyond `Foldable` which only provides values in sequence.
- The Comonad instance provides a dual to the Monad instance, enabling context-aware computations where functions have access to the full pattern structure at each position.
- Context-aware operations (depthAt, sizeAt, indicesAt) are optional convenience functions that demonstrate the power of the Comonad instance but are not required for the core Comonad functionality.
- The implementation should follow standard comonad patterns for tree structures, similar to `Data.Tree` comonad implementation.
