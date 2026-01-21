# Feature Specification: Separation of Container and Content Parsing

**Feature Branch**: `030-separate-container-content-parsing`  
**Created**: 2026-01-20  
**Status**: Draft  
**Input**: User description: "Refactoring the parsing behavior to separate container versus content parsing as described above"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Parsing Multiple Patterns as a Stream (Priority: P1)

Users should be able to parse a Gram document containing multiple top-level patterns and receive them as a list of distinct `Pattern Subject` structures, rather than a single wrapped root pattern.

**Why this priority**: This is the fundamental change requested to align with `gram-rs` and provides a more natural way to work with Gram files containing multiple independent patterns.

**Independent Test**: Can be fully tested by providing a Gram string with two separate nodes `(a) (b)` and verifying that the parser returns a list of two patterns.

**Acceptance Scenarios**:

1. **Given** a Gram document with `(a) (b)`, **When** parsed using the new stream-aware parser, **Then** the output is a list containing two patterns: one for `(a)` and one for `(b)`.
2. **Given** an empty Gram document, **When** parsed, **Then** the output is an empty list of patterns.

---

### User Story 2 - Explicit Header Metadata Handling (Priority: P1)

Users should be able to explicitly separate a leading bare record in a Gram document from the subsequent patterns, treating it as document-level metadata.

**Why this priority**: Alignment with `gram-rs` and improved support for document metadata without relying on magic labels.

**Independent Test**: Can be fully tested by providing a document `{version: "1.0"} (a)` and verifying that the metadata `{version: "1.0"}` and the pattern `(a)` are returned separately.

**Acceptance Scenarios**:

1. **Given** a Gram document with `{key: "value"} (a)`, **When** parsed with header-aware parsing, **Then** the header metadata `{key: "value"}` and the list of patterns `[(a)]` are returned as a tuple.
2. **Given** a Gram document without a leading record `(a) (b)`, **When** parsed with header-aware parsing, **Then** the metadata is `Nothing` and the list of patterns is `[(a), (b)]`.

---

### User Story 3 - Serializing Multiple Patterns and Headers (Priority: P2)

Users should be able to serialize a list of patterns, optionally including a header record, into a valid Gram document.

**Why this priority**: Completes the round-trip capability for the new container/content model.

**Independent Test**: Can be fully tested by providing a list of patterns and a metadata map and verifying the generated Gram string.

**Acceptance Scenarios**:

1. **Given** a list of patterns `[(a), (b)]` and a metadata map `{v: 1}`, **When** serialized with the new header-aware serializer, **Then** the output is `{v: 1}\n(a)\n(b)`.

---

### Edge Cases

- What happens when a bare record appears in the middle of a list of patterns? (It should be treated as a bare pattern, not a header).
- How does the system handle a document containing only a bare record? (It should be treated as metadata with an empty pattern list).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a parser that returns a list of `Pattern Subject` from a Gram document.
- **FR-002**: System MUST provide a parser that separates an optional leading bare record from subsequent patterns.
- **FR-003**: System MUST provide a serializer for a list of patterns.
- **FR-004**: System MUST provide a serializer for a header record followed by a list of patterns.
- **FR-005**: System MUST maintain backward compatibility for existing single-pattern parsing/serialization via legacy wrappers or the existing `Gram.Root` mapping.

### Key Entities *(include if feature involves data)*

- **Gram Document**: Represents the entire file content, consisting of an optional header and a sequence of patterns.
- **Header Metadata**: A set of key-value pairs at the beginning of a document representing document-level attributes.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Parser can process a document with 10,000 top-level patterns without performance degradation.
- **SC-002**: Round-trip consistency: `parse(serialize(header, patterns)) == (header, patterns)`.
- **SC-003**: Alignment with `gram-rs`: The API signatures and behavior for multi-pattern and header handling match the Rust implementation.
