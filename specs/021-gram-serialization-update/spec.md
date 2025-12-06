# Feature Specification: Update Gram Serialization for Tree-Sitter 0.2.7

**Feature Branch**: `021-gram-serialization-update`
**Created**: 2025-12-06
**Status**: Draft
**Input**: User description: "Update the gram serialization to conform with the latest tree-sitter-gram 0.2.7 which is available as a git submodule. Though a minor version, the change is significant, flattening the top-level pattern elements so that subject_pattern, node_pattern, relationship_pattern, and annotation_pattern can all appear as top-level elements. Also, annotation_pattern no longer accepts comma-separated elements. The README.md in tree-sitter-gram summarizes the hierarchy well. Consider the changes needed to @gram"

## User Scenarios & Testing

### User Story 1 - Serialize Flat Top-Level Patterns (Priority: P1)

As a developer using the gram library, I want patterns to be serialized with a flat structure where nodes, relationships, and annotations can all appear at the top level, so that the output is compatible with the latest grammar definition.

**Why this priority**: Essential for compatibility with `tree-sitter-gram` 0.2.7. Failure to implement this invalidates the serialization for the new parser version.

**Independent Test**: Can be tested by serializing a pattern containing a mix of nodes, relationships, and annotations at the root, and verifying the output structure is a simple sequence without extra nesting or grouping that was previously required, and that it parses successfully with the 0.2.7 parser.

**Acceptance Scenarios**:

1. **Given** a pattern with a node and a relationship at the top level, **When** it is serialized, **Then** the output contains the node and relationship in sequence at the top level.
2. **Given** a pattern with a top-level annotation, **When** it is serialized, **Then** the annotation appears at the top level alongside other elements.
3. **Given** a complex pattern with mixed elements, **When** it is serialized, **Then** the structure matches the "flattened" hierarchy of `tree-sitter-gram` 0.2.7.

---

### User Story 2 - Serialize Annotations Without Commas (Priority: P1)

As a developer, I want multiple annotations to be serialized as a whitespace-separated list (or whatever the new format implies, effectively just "no commas") rather than a comma-separated list, so that the output complies with the stricter syntax of the new grammar.

**Why this priority**: Comma-separated annotations are no longer valid in the new grammar version.

**Independent Test**: Serialize a pattern with multiple annotations and check for the absence of commas between them.

**Acceptance Scenarios**:

1. **Given** a pattern with multiple annotations, **When** it is serialized, **Then** the annotations are separated by whitespace (or valid delimiters), not commas.
2. **Given** a single annotation, **When** it is serialized, **Then** it appears correctly without trailing commas.

### Edge Cases

- **Empty Pattern**: How does the system handle an empty pattern structure? It should output an empty string or valid empty representation if allowed by the grammar.
- **Legacy Nested Structures**: If the internal data model allows nesting that is no longer valid in the grammar (e.g. a node inside a container that shouldn't exist), the serializer must flatten it or produce an error. (Assumption: Internal model can be mapped to flat structure).
- **Special Characters**: Ensure characters that might conflict with new delimiters (if any) are properly escaped.

## Requirements

### Functional Requirements

- **FR-001**: The system MUST serialize `subject_pattern`, `node_pattern`, `relationship_pattern`, and `annotation_pattern` as valid top-level elements in the output stream.
- **FR-002**: The system MUST NOT group top-level elements into containers that are no longer part of the `tree-sitter-gram` 0.2.7 grammar (flattening the hierarchy).
- **FR-003**: The system MUST serialize lists of annotations without using comma separators.
- **FR-004**: The serialized output MUST be syntactically valid according to `tree-sitter-gram` 0.2.7.

### Key Entities

- **GramSerializer**: The component responsible for converting internal pattern representations into string/text format.
- **Grammar**: The external definition (`tree-sitter-gram` 0.2.7) that acts as the source of truth for validity.

## Success Criteria

### Measurable Outcomes

- **SC-001**: 100% of serialized patterns produced by the updated serializer pass validation/parsing by `tree-sitter-gram` 0.2.7.
- **SC-002**: Patterns containing all 4 types of top-level elements (subject, node, relationship, annotation) serialize successfully to the new flat format.
- **SC-003**: Annotation lists serialize without commas and are parsed correctly as multiple annotations.
