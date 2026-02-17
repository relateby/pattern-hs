# Feature Specification: Gram annotation-based identifiers and labels

**Feature Branch**: `032-gram-annotation-syntax`  
**Created**: 2026-02-17  
**Status**: Draft  
**Input**: User description: "Update parser to support new gram syntax for annotation based identifier and labels as illustrated in libs/gram/test-data/tree-sitter-gram/test/corpus/extended_annotations.txt"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Property-style annotations on patterns (Priority: P1)

A pattern author attaches a single key-value property to a pattern using the form `@key(value)` before the pattern. The value may be a number, a symbol, or a quoted string. The parser accepts such input and represents the annotation together with the pattern so downstream tools can use it.

**Why this priority**: Property-style annotations are the base form (single `@`) and support metadata like descriptions or numeric flags; they are the foundation for the extended annotation syntax.

**Independent Test**: Can be fully tested by parsing inputs such as `@x(1) ()`, `@desc(a) (a)`, and `@desc("historic route") (a)-->(b)` and verifying that the annotation key and value are captured and associated with the pattern.

**Acceptance Scenarios**:

1. **Given** a pattern string with one property-style annotation and a symbol value, **When** the parser parses it, **Then** the annotation key and symbol value are recognized and associated with the pattern.
2. **Given** a pattern string with one property-style annotation and an integer value, **When** the parser parses it, **Then** the annotation key and integer value are recognized and associated with the pattern.
3. **Given** a pattern string with one property-style annotation and a string literal value, **When** the parser parses it, **Then** the annotation key and string content are recognized and associated with the pattern.
4. **Given** a pattern string with property-style annotation applied to a relationship pattern (e.g. node–edge–node), **When** the parser parses it, **Then** the annotation is associated with that full pattern.

---

### User Story 2 - Identifier-only annotations (Priority: P2)

A pattern author attaches an identifier to a pattern using the double-at form `@@identifier` before the pattern (e.g. `@@p (a)` or `@@r1 (a)-[r]->(b)`). The parser accepts this and represents the identifier as part of the annotation so tools can refer to that pattern by name.

**Why this priority**: Identifier-only annotations allow naming patterns (e.g. for references or debugging) and are the simplest form of the double-at syntax.

**Independent Test**: Can be fully tested by parsing `@@p (a)` and `@@r1 (a)-[r]->(b)` and verifying that the annotation carries the given identifier and is associated with the node pattern or relationship pattern respectively.

**Acceptance Scenarios**:

1. **Given** a pattern string with `@@identifier` followed by a node pattern, **When** the parser parses it, **Then** the annotation contains the identifier and is associated with the node pattern.
2. **Given** a pattern string with `@@identifier` followed by a relationship pattern (node–edge–node), **When** the parser parses it, **Then** the annotation contains the identifier and is associated with the relationship pattern.

---

### User Story 3 - Labels-only annotations (Priority: P3)

A pattern author attaches one or more labels to a pattern using the double-at form with a colon and label(s), e.g. `@@:L (a)` or `@@::Label (a)`. The parser accepts this and represents the labels as part of the annotation so tools can categorize or filter patterns.

**Why this priority**: Labels-only annotations support tagging patterns without a separate identifier; they extend the annotation model after identifier-only is in place.

**Independent Test**: Can be fully tested by parsing `@@:L (a)` and `@@::Label (a)` and verifying that the annotation carries the specified label(s) and is associated with the pattern.

**Acceptance Scenarios**:

1. **Given** a pattern string with `@@:Label` (single colon and label) before a pattern, **When** the parser parses it, **Then** the annotation contains the given label(s) and is associated with the pattern.
2. **Given** a pattern string with `@@::Label` (double colon and label) before a pattern, **When** the parser parses it, **Then** the annotation contains the given label(s) and is associated with the pattern.

---

### User Story 4 - Identifier and labels combined (Priority: P4)

A pattern author attaches both an identifier and labels to a pattern using the form `@@identifier:Label(s) (pattern)` (e.g. `@@p:L (a)`). The parser accepts this and represents both the identifier and the labels in the annotation.

**Why this priority**: Combined form completes the annotation syntax so authors can name and tag a pattern in one place.

**Independent Test**: Can be fully tested by parsing `@@p:L (a)` and verifying that the annotation contains both the identifier and the label(s).

**Acceptance Scenarios**:

1. **Given** a pattern string with `@@identifier:Label` before a pattern, **When** the parser parses it, **Then** the annotation contains both the identifier and the label(s) and is associated with the pattern.

---

### Edge Cases

- When the double-at form is used with no identifier and no labels (e.g. `@@ (a)`), the parser MUST reject the input and report a parse error rather than producing a valid pattern.
- When property-style annotation has an empty or invalid value, the parser MUST reject or report an error in a way that allows the user to correct the input.
- When annotation syntax is mixed in an undefined or ambiguous way, the parser behavior MUST be specified (reject or define precedence) so that behavior is consistent and testable.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The parser MUST accept property-style annotations of the form `@key(value)` preceding a pattern, where value may be an integer, a symbol, or a string literal, and MUST associate that annotation with the following pattern.
- **FR-002**: The parser MUST accept identifier-only double-at annotations of the form `@@identifier` preceding a node pattern or relationship pattern and MUST record the identifier in the annotation for that pattern.
- **FR-003**: The parser MUST accept labels-only double-at annotations of the form `@@:Label` or `@@::Label` preceding a pattern and MUST record the label(s) in the annotation for that pattern.
- **FR-004**: The parser MUST accept combined double-at annotations of the form `@@identifier:Label(s)` preceding a pattern and MUST record both the identifier and the label(s) in the annotation for that pattern.
- **FR-005**: The parser MUST reject the double-at form when neither an identifier nor any labels are present (e.g. `@@` followed only by a pattern) and MUST report a parse error.
- **FR-006**: The parser MUST produce a representation of annotated patterns that preserves annotation type (property vs identified), key/value for properties, and identifier and/or labels for identified annotations, so that downstream tools can consume them without re-parsing the source string.
- **FR-007**: Parsing behavior for all annotation forms MUST be consistent with the semantics and examples described in the reference corpus (extended_annotations.txt).

### Assumptions

- The existing gram pattern language (node patterns, relationship patterns, identifiers, labels) remains unchanged; this feature only adds annotation syntax and its interpretation.
- The reference corpus (extended_annotations.txt) is the authoritative source for valid and invalid annotation examples.
- Downstream tools expect a structured representation of annotations (property vs identified, key/value, identifier, labels) rather than raw source text.

### Key Entities

- **Property annotation**: A key-value pair attached to a pattern (e.g. key and integer/symbol/string value), represented so that key and value are distinguishable.
- **Identified annotation**: An annotation that carries an optional identifier and/or one or more labels, attached to a pattern and represented so that identifier and labels are distinguishable.
- **Pattern**: A node pattern or relationship pattern (node–edge–node) that may be preceded by one or more annotations in the supported syntax.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Every valid annotation form illustrated in the extended_annotations reference corpus (property-style, identifier-only, labels-only, identifier+labels) parses successfully and produces the expected structure when validated against that corpus.
- **SC-002**: The invalid case (empty double-at header, e.g. `@@ (a)`) is rejected by the parser with a clear error, and no valid pattern is produced.
- **SC-003**: Downstream consumers can distinguish property-style annotations from identified annotations and can read annotation keys, values, identifiers, and labels from the parser output without parsing the original gram text.
- **SC-004**: Parsing of annotated patterns completes in a time proportional to input size so that typical pattern documents (e.g. dozens of annotated patterns) parse in under one second.
