# Feature Specification: Gram Pattern and Path Semantics

**Feature Branch**: `018-pattern-path-semantics`
**Created**: 2025-11-29
**Status**: Draft
**Input**: User description: "Review, research and critique the semantics of pattern and path notation as initially described in @design/SEMANTICS.md and @design/EXTENDED-SEMANTICS.md . Refine the definitions, provide examples, documentation and extensive test cases. Get agreement from the user, then progressively implement validation of parsed gram from basic use cases with individual elements, to complex multi-statement gram that mixes notation in both positive and negative examples."

## User Scenarios & Testing

### User Story 1 - Semantic Validation of Basic Patterns (Priority: P1)

As a Gram language user, I need the system to validate my pattern definitions so that I don't accidentally create ambiguous or conflicting data structures.

**Why this priority**: This is the foundation of the Gram language semantics. Without basic pattern validation, more complex structures cannot be reliably interpreted.

**Independent Test**: Can be tested by feeding a series of valid and invalid simple pattern strings (e.g., `[a]`, `[a][a]`) to the validator and checking the output.

**Acceptance Scenarios**:

1. **Given** a source with a single pattern definition `[a]`, **When** validated, **Then** it succeeds.
2. **Given** a source with duplicate pattern definitions `[a][a]`, **When** validated, **Then** it returns a "Duplicate Definition" error.
3. **Given** a source with a reference to an undefined pattern `[b | a]`, **When** validated, **Then** it returns an "Undefined Reference" error (unless forward references are explicitly handled/allowed in a specific pass, but final validation should fail if never defined).
4. **Given** a source with a self-reference `[a | a]`, **When** validated, **Then** it returns a "Self-Reference" error.

---

### User Story 2 - Path Notation Semantics (Priority: P2)

As a Gram user, I want to use path notation to define graphs, where nodes and relationships are automatically defined on first use, so that I can write intuitive graph data.

**Why this priority**: Path notation is a core feature for graph usability.

**Independent Test**: Can be tested with path strings like `(a)-[r]->(b)` and checking if `a`, `b`, and `r` are correctly registered in the symbol table.

**Acceptance Scenarios**:

1. **Given** a path `(a)-[r]->(b)`, **When** validated, **Then** it succeeds and defines `a`, `b`, and `r`.
2. **Given** a path `(a)-[r]->(b)` followed by `(b)-[r]->(c)`, **When** validated, **Then** it fails because `r` is being redefined with different endpoints (unless `r` is a label, but here it is an identifier).
3. **Given** a path `(a)-[:knows]->(b)`, **When** validated, **Then** it succeeds (anonymous relationship).

---

### User Story 3 - Mixed Notation Consistency (Priority: P3)

As a Gram user, I want to mix pattern and path notations in the same file, so that I can describe data in the most appropriate format for each part, while ensuring they don't contradict each other.

**Why this priority**: Ensures the two syntaxes integrate seamlessly.

**Independent Test**: Test files containing both `[...]` and `()-[]->()` syntax referencing the same identifiers.

**Acceptance Scenarios**:

1. **Given** a definition `[knows | a, b]` and a path `(a)-[knows]->(b)`, **When** validated, **Then** it succeeds (consistent).
2. **Given** a definition `[knows | a, c]` and a path `(a)-[knows]->(b)`, **When** validated, **Then** it fails with "Inconsistent Definition".
3. **Given** a path `(a)-[r]->(b)` and a later modification `[r {weight: 1}]`, **When** validated, **Then** it fails with "Immutability Violation" (cannot modify defined pattern).

### Edge Cases

- **Circular Dependencies**: Indirect recursion `[a | b], [b | a]` is valid, but direct `[a | a]` is invalid.
- **Forward References**: `[a | b], [b]` is valid. `[a | b]` alone is invalid.
- **Mixed Direction Paths**: `(a)-[r]->(b)<-[r]-(a)` - valid if `r` implies `[r | a, b]` in both cases? No, the second one implies `[r | b, a]` which contradicts.
- **Anonymous Re-use**: `(a)-[:knows]->(b)` and `(a)-[:knows]->(b)` creates two distinct anonymous relationships, not one.

## Requirements

### Functional Requirements

- **FR-001**: System MUST enforce the **Single Definition Rule**: An identifier can be defined exactly once in a scope.
- **FR-002**: System MUST enforce **Referential Integrity**: All references must resolve to a defined identifier (allowing for forward references within the same file/scope).
- **FR-003**: System MUST enforce **Immutability**: Once defined (structure, labels, properties), a pattern cannot be modified or extended.
- **FR-004**: System MUST interpret **Path Definitions**: The first appearance of an identifier in a path context (node or relationship) counts as its definition if not already defined.
- **FR-005**: System MUST enforce **Path-Pattern Consistency**: A relationship identifier used in a path `(a)-[r]->(b)` MUST correspond to a pattern structure equivalent to `[r | a, b]`.
- **FR-006**: System MUST support **Anonymous Elements**: `[]` and `()`/`-[]-` without identifiers must be treated as unique instances.
- **FR-007**: System MUST detect **Direct Cycles**: A pattern cannot contain itself as a direct child element (e.g., `[a | a]`). Indirect cycles via references are allowed.

### Key Entities

- **Symbol Registry**: Tracks defined identifiers, their classification, and their definition state.
- **Validation State**: Accumulates errors and warnings during the traversal of the parsed Gram AST.

## Success Criteria

### Measurable Outcomes

- **SC-001**: The validator correctly identifies 100% of the error cases defined in the `design/SEMANTICS.md` and `design/EXTENDED-SEMANTICS.md` documents.
- **SC-002**: The validator accepts 100% of the valid example cases defined in the design documents.
- **SC-003**: A new comprehensive test suite is created with at least 50 distinct test cases covering edge cases and mixed notations.
- **SC-004**: Updated `SEMANTICS.md` documentation is produced, clarifying any ambiguities found during implementation.
