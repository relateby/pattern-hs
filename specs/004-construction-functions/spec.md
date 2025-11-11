# Feature Specification: Construction Functions

**Feature Branch**: `004-construction-functions`  
**Created**: 2025-01-28  
**Status**: Draft  
**Input**: User description: "continue with @TODO.md 3.1, constructing common patterns with 0, 1, 2, or more elements"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Create Atomic Patterns Conveniently (Priority: P1)

As a developer using the Pattern library, I need a simple function to create atomic patterns (patterns with no elements) so that I can construct basic patterns more concisely without writing verbose record syntax.

**Why this priority**: Creating atomic patterns is the most common operation when building graph structures. Currently, developers must write `Pattern { value = x, elements = [] }` for every atomic pattern, which is verbose and repetitive. A simple constructor function makes the code more readable and reduces boilerplate.

**Independent Test**: Can be fully tested by calling the constructor function with various value types and verifying that the resulting pattern has the correct value and an empty elements list. This delivers a more convenient way to create the fundamental building blocks of pattern structures.

**Acceptance Scenarios**:

1. **Given** a value of any type, **When** I call the atomic pattern constructor with that value, **Then** the resulting pattern contains that value and has no elements
2. **Given** an atomic pattern created with the constructor, **When** I inspect its structure, **Then** it behaves identically to a pattern created with record syntax
3. **Given** values of different types (strings, integers, custom types), **When** I create atomic patterns with the constructor, **Then** each pattern correctly stores its value regardless of type

---

### User Story 2 - Create Patterns with Elements Conveniently (Priority: P1)

As a developer using the Pattern library, I need a function to create patterns with elements so that I can construct complex patterns (singular patterns, relationships, subgraphs) more concisely without writing verbose record syntax.

**Why this priority**: Creating patterns with elements is essential for building relationships and nested structures. Currently, developers must write `Pattern { value = x, elements = [p1, p2, ...] }` which becomes unwieldy for complex structures. A constructor function makes the code more readable and maintainable.

**Independent Test**: Can be fully tested by calling the constructor function with a value and a list of pattern elements, then verifying that the resulting pattern has the correct value and all elements are accessible in the correct order. This delivers a more convenient way to create patterns with any number of elements.

**Acceptance Scenarios**:

1. **Given** a value and a list of existing patterns, **When** I call the pattern-with-elements constructor, **Then** the resulting pattern contains the value and all elements are accessible
2. **Given** a pattern created with the constructor, **When** I inspect its structure, **Then** it behaves identically to a pattern created with record syntax
3. **Given** patterns with varying numbers of elements (zero, one, two, many), **When** I create them with the constructor, **Then** all patterns correctly store their elements regardless of count

---

### User Story 3 - Create Patterns from Lists of Values (Priority: P1)

As a developer using the Pattern library, I need a function to create patterns from lists of raw values so that I can easily convert data structures (like lists from APIs or user input) into patterns without manually mapping `pattern` over each value.

**Why this priority**: Converting lists of values to patterns is a common operation when working with external data sources or user input. Currently, developers must write `patternWith decoration (map pattern values)` which is verbose and requires understanding function composition. A dedicated `fromList` function makes this operation more discoverable and concise.

**Independent Test**: Can be fully tested by calling `fromList` with a decoration value and a list of values, then verifying that the resulting pattern has the correct decoration and all values are converted to atomic pattern elements in order. This delivers a convenient way to create patterns from raw data.

**Acceptance Scenarios**:

1. **Given** a decoration value and a list of values, **When** I call `fromList`, **Then** the resulting pattern contains the decoration and all values are converted to atomic pattern elements
2. **Given** a pattern created with `fromList`, **When** I inspect its structure, **Then** it behaves identically to `patternWith decoration (map pattern values)`
3. **Given** lists with varying numbers of values (zero, one, two, many), **When** I create patterns with `fromList`, **Then** all patterns correctly store their elements regardless of count

---

### User Story 4 - Support All Common Pattern Structures (Priority: P2)

As a developer using the Pattern library, I need the constructor functions to work correctly for all common pattern structures (atomic patterns with 0 elements, singular patterns with 1 element, pairs with 2 elements, and extended patterns with many elements) so that I can use them consistently across all use cases.

**Why this priority**: While the primary use cases are covered by the two main constructors, ensuring they work correctly for edge cases (empty lists, single elements, etc.) provides confidence and prevents bugs. This is important for library reliability but not critical for basic functionality.

**Independent Test**: Can be fully tested by creating patterns with 0, 1, 2, and many elements using the constructors, then verifying each produces the expected structure. This delivers comprehensive coverage of all common pattern creation scenarios.

**Acceptance Scenarios**:

1. **Given** an empty list of elements, **When** I call the pattern-with-elements constructor, **Then** the resulting pattern behaves like an atomic pattern (no elements)
2. **Given** a list with exactly one element, **When** I call the pattern-with-elements constructor, **Then** the resulting pattern is a singular pattern (one element)
3. **Given** a list with exactly two elements, **When** I call the pattern-with-elements constructor, **Then** the resulting pattern contains both elements in order
4. **Given** a list with many elements, **When** I call the pattern-with-elements constructor, **Then** the resulting pattern contains all elements in the correct order

---

### Edge Cases

- What happens when `patternWith` or `fromList` receives an empty list? (Should behave like atomic pattern constructor)
- How do the constructors handle nested patterns? (Should work recursively)
- Do the constructors preserve element order? (Yes, order must be preserved)
- Can constructors be used with all value types? (Yes, should work with any type)
- Does `fromList` correctly convert each value to an atomic pattern? (Yes, uses `pattern` for each value)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a function that creates an atomic pattern (pattern with no elements) from a single value
- **FR-002**: System MUST provide a function that creates a pattern with elements from a value and a list of pattern elements
- **FR-003**: System MUST provide a function that creates a pattern from a value and a list of values (converting each value to an atomic pattern)
- **FR-004**: The atomic pattern constructor MUST produce patterns that are functionally identical to patterns created with `Pattern { value = x, elements = [] }`
- **FR-005**: The pattern-with-elements constructor MUST produce patterns that are functionally identical to patterns created with `Pattern { value = x, elements = ps }`
- **FR-006**: The from-list constructor MUST produce patterns that are functionally identical to `patternWith decoration (map pattern values)`
- **FR-007**: All constructors MUST preserve the type parameter of the value (e.g., `Pattern String`, `Pattern Int`)
- **FR-008**: The pattern-with-elements and from-list constructors MUST preserve the order of elements/values in the input list
- **FR-009**: The pattern-with-elements and from-list constructors MUST handle empty lists (producing an atomic pattern)
- **FR-010**: The pattern-with-elements and from-list constructors MUST handle lists with any number of elements (0, 1, 2, or more)
- **FR-011**: All constructors MUST work with values of any type (strings, integers, custom types, etc.)

### Key Entities

- **Pattern**: The core data structure representing a decorated sequence. Has a value field (decoration) and an elements field (the sequence itself).
- **Atomic Pattern**: A pattern with no elements (`elements == []`), created by the atomic pattern constructor.
- **Pattern with Elements**: A pattern containing one or more pattern elements, created by the pattern-with-elements constructor. Includes singular patterns (1 element), pairs (2 elements), and extended patterns (many elements).
- **List Conversion**: The process of converting a list of values into a pattern by creating atomic patterns from each value and combining them into a single pattern.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can create atomic patterns using a single function call instead of record syntax, reducing code verbosity by at least 50% for atomic pattern creation
- **SC-002**: Developers can create patterns with elements using a single function call, reducing code verbosity by at least 40% for pattern-with-elements creation
- **SC-003**: Developers can create patterns from lists of values using a single function call, reducing code verbosity by at least 60% compared to manually mapping `pattern` over a list
- **SC-004**: All constructor functions work correctly for patterns with 0, 1, 2, and many elements (100% of test cases pass)
- **SC-005**: Patterns created with constructors are functionally identical to patterns created with record syntax (100% behavioral equivalence verified through tests)
- **SC-006**: Constructor functions support all value types used in the codebase (strings, integers, custom types) without type errors

## Assumptions

- The constructors will be pure functions (no side effects)
- The constructors will follow standard Haskell naming conventions (`fromList` follows `Set.fromList`, `Map.fromList` pattern)
- The constructors will be exported from the main Pattern module for easy access
- The constructors will have comprehensive Haddock documentation with examples
- The constructors will be tested with property-based tests where applicable
- The constructors will maintain type safety (enforced by Haskell's type system)
- `fromList` will be implemented as a combination of `pattern` and `patternWith`: `fromList decoration values = patternWith decoration (map pattern values)`

## Dependencies

- **Feature 001**: Core Pattern data type must be implemented (✅ Complete)
- **Feature 002**: Basic Pattern type with Show and Eq instances must be implemented (✅ Complete)
- Test infrastructure (HSpec, QuickCheck) must be available (✅ Available)

## Out of Scope

- Pattern validation or classification functions (e.g., `isNode`, `isRelationship`) - these are planned for future features
- Pattern transformation or manipulation functions - these are planned for future features
- Pattern query functions (e.g., `length`, `size`, `depth`) - these are planned for Feature 7
- Typeclass instances (Functor, Foldable, Traversable) - these are planned for Features 4-6
- Graph view interpretations - these are planned for Feature 9
