# Feature Specification: Subject Identity and Serialization

**Feature Branch**: `020-subject-serialization`
**Created**: 2025-11-29
**Status**: Draft
**Input**: User description: "Begin \"subject identity and serialization\" as described in @TODO.md"

## User Scenarios & Testing *(mandatory)*

<!--
  IMPORTANT: User stories should be PRIORITIZED as user journeys ordered by importance.
  Each user story/journey must be INDEPENDENTLY TESTABLE - meaning if you implement just ONE of them,
  you should still have a viable MVP (Minimum Viable Product) that delivers value.
-->

### User Story 1 - Round-trip Serialization (Priority: P1)

As a developer using the library, I need to serialize `Subject` instances to gram notation and parse them back so that I can persist and retrieve graph data without loss of information.

**Why this priority**: This is the core functionality. Without reliable round-trip serialization, the library cannot effectively communicate with external systems or storage.

**Independent Test**: Can be tested by generating random `Subject` instances, serializing them to text, parsing the text back, and asserting equality.

**Acceptance Scenarios**:

1. **Given** a `Subject` with a specific identifier, labels, and properties, **When** I serialize it to gram notation, **Then** the output string matches the expected gram syntax.
2. **Given** a serialized gram string of a specific subject, **When** I parse it, **Then** I get back a `Subject` object identical to the original.
3. **Given** a `Subject` with nested relationships, **When** I serialize and then parse it, **Then** the structure and all values are preserved.

---

### User Story 2 - Handling Anonymous Subjects (Priority: P2)

As a user writing gram patterns, I want to define patterns with anonymous nodes (e.g., `()-[:KNOWS]->()`) and have them parsed into valid `Subject` objects so that I don't have to manually assign IDs when they are not needed for my query.

**Why this priority**: Gram syntax supports and encourages anonymous nodes for pattern matching. The library must handle this common case to be compliant with the language.

**Independent Test**: Can be tested by providing gram strings with anonymous nodes and verifying that the parsed `Subject` objects have valid, non-conflicting identifiers assigned.

**Acceptance Scenarios**:

1. **Given** a gram string with an anonymous node `()`, **When** I parse it, **Then** the resulting `Subject` has a generated unique identifier.
2. **Given** a gram string with multiple anonymous nodes `()-[]->()`, **When** I parse it, **Then** each resulting `Subject` has a distinct unique identifier.

---

### User Story 3 - Identity Preservation (Priority: P3)

As a system integrator, I want to ensure that when I serialize a `Subject` with a specific ID, that same ID is present in the output text, so that external systems can recognize the entity.

**Why this priority**: Crucial for interoperability, though slightly less fundamental than the basic mechanics of serialization.

**Independent Test**: Verify that specific ID strings appear in the serialized output.

**Acceptance Scenarios**:

1. **Given** a `Subject` with ID "user-123", **When** I serialize it, **Then** the string "user-123" appears in the identifier position of the gram output.

---

### Edge Cases

- What happens when a `Subject` contains characters that need escaping in gram notation (e.g., quotes in property values)?
- How does the system handle parsing errors or invalid gram syntax?
- What happens if a generated ID for an anonymous subject collides with an existing explicit ID (unlikely if using UUIDs, but possible)?
- Handling of empty or minimal subjects.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST implement `toGram :: Subject -> String` to serialize a `Subject` and its structure to valid gram notation.
- **FR-002**: System MUST implement `fromGram :: String -> Either ParseError Subject` to parse gram notation into a `Subject`.
- **FR-003**: System MUST automatically assign unique identifiers to any anonymous subjects encountered during parsing, as the `Subject` type requires an identity.
- **FR-004**: Serialization MUST properly escape special characters in string values to ensure valid gram syntax.
- **FR-005**: Parsing MUST correctly handle all valid gram value types (strings, integers, decimals, booleans) within a Subject.
- **FR-006**: The serialization/parsing cycle MUST satisfy the round-trip property: `fromGram(toGram(s)) == Right s` for any valid `Subject` s.
- **FR-007**: The system MUST support a strategy for identity generation (e.g., sequential, random/UUID) that ensures local uniqueness within a parse operation.

### Key Entities *(include if feature involves data)*

- **Subject**: The core data structure representing a node/entity, containing an identifier (Id), a set of labels, and a map of properties.
- **Gram Notation**: The string representation of the graph data.
- **Identity (Id)**: A unique string or value identifying a Subject.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of valid `Subject` instances defined in the test suite can be successfully round-tripped (serialized then parsed) without error or data loss.
- **SC-002**: Parsing of valid gram strings with anonymous subjects succeeds 100% of the time, producing `Subject` instances with non-empty identifiers.
- **SC-003**: Serialization performance is sufficient to handle batch operations (e.g., < 10ms per simple subject on standard hardware - though strict benchmarking isn't the primary goal here, it shouldn't be noticeably slow).
- **SC-004**: All tests for special character escaping pass.
