# Feature Specification: Gram Serialization Library

**Feature Branch**: `014-gram-serialization`  
**Created**: 2025-01-27  
**Status**: Draft  
**Input**: User description: "A serialization library for patterns with subject values based on the gram notation. Should support all syntax supported by tree-sitter-gram. Should evaluate whether to use tree-sitter-gram or an alternative Haskell parsing library. Mandatory to use the test corpus provided by https://github.com/gram-data/tree-sitter-gram"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Serialize Pattern Subject to Gram Notation (Priority: P1)

Developers need to convert Pattern Subject data structures into gram notation text format for storage, transmission, or human-readable representation. They should be able to serialize any valid Pattern Subject structure, including simple subjects, subjects with properties, nested patterns, and relationship patterns, with all value types properly formatted.

**Why this priority**: Serialization is the primary output capability of the library. Without serialization, developers cannot convert their data structures to gram notation format, which is the core purpose of this feature. This must be completed first as it enables data export and interoperability.

**Independent Test**: Can be fully tested by creating Pattern Subject structures, calling the serialization function, and verifying the output matches expected gram notation syntax. Delivers the ability to convert Haskell data structures to gram notation text.

**Acceptance Scenarios**:

1. **Given** a Pattern Subject with a simple subject (identity and labels), **When** serialized to gram notation, **Then** the output matches the expected gram syntax (e.g., `(n:Person)`)
2. **Given** a Pattern Subject with properties containing standard value types (integers, decimals, booleans, strings, symbols), **When** serialized to gram notation, **Then** all property values are correctly formatted according to gram notation rules
3. **Given** a Pattern Subject with properties containing extended value types (tagged strings, arrays, maps, ranges, measurements), **When** serialized to gram notation, **Then** all extended value types are correctly formatted
4. **Given** a Pattern Subject with nested patterns (patterns containing other patterns as elements), **When** serialized to gram notation, **Then** the nested structure is correctly represented in the output
5. **Given** a Pattern Subject representing a relationship pattern (e.g., `(a)-[r:KNOWS]->(b)`), **When** serialized to gram notation, **Then** the relationship syntax is correctly formatted with source, relationship, and target subjects
6. **Given** a Pattern Subject with anonymous identity (empty symbol), **When** serialized to gram notation, **Then** the output handles anonymous subjects appropriately (either omitting identity or generating placeholder)

---

### User Story 2 - Parse Gram Notation to Pattern Subject (Priority: P1)

Developers need to convert gram notation text format into Pattern Subject data structures for reading stored data, parsing user input, or processing gram notation files. They should be able to parse any valid gram notation syntax, including all value types, nested patterns, and relationship patterns, with proper error handling for invalid syntax.

**Why this priority**: Deserialization is equally critical as serialization for round-trip data conversion. Without parsing, developers cannot read gram notation data back into Haskell data structures, limiting the library's usefulness. This should be completed alongside serialization to enable full bidirectional conversion.

**Independent Test**: Can be fully tested by providing gram notation strings, calling the parsing function, and verifying the output matches expected Pattern Subject structures. Delivers the ability to convert gram notation text to Haskell data structures.

**Acceptance Scenarios**:

1. **Given** a valid gram notation string with a simple subject, **When** parsed, **Then** the result is a Pattern Subject with the correct identity, labels, and empty properties
2. **Given** a valid gram notation string with properties containing standard value types, **When** parsed, **Then** the result contains properties with correctly typed values
3. **Given** a valid gram notation string with properties containing extended value types, **When** parsed, **Then** the result contains properties with correctly typed extended values
4. **Given** a valid gram notation string with nested patterns, **When** parsed, **Then** the result correctly represents the nested structure as Pattern elements
5. **Given** a valid gram notation string representing a relationship pattern, **When** parsed, **Then** the result correctly represents the relationship with source, relationship, and target subjects
6. **Given** a gram notation string with anonymous subjects (no identity), **When** parsed, **Then** the result assigns appropriate identity (either empty symbol or generated identifier)
7. **Given** an invalid gram notation string (syntax errors, malformed structure), **When** parsed, **Then** the result is a parse error with clear error message indicating the problem

---

### User Story 3 - Support All Tree-Sitter-Gram Syntax (Priority: P1)

Developers need confidence that the serialization library handles all syntax features supported by the tree-sitter-gram parser. The library should support all gram notation features including comments, all value types, pattern structures, and edge cases, ensuring compatibility with the official gram notation specification.

**Why this priority**: Complete syntax support is mandatory per the feature requirements. Without full syntax support, the library cannot be considered complete or reliable. This must be verified using the test corpus from tree-sitter-gram to ensure comprehensive coverage.

**Independent Test**: Can be fully tested by running the test corpus from tree-sitter-gram repository, verifying that all test cases pass (both parsing and serialization), and confirming that all syntax features are handled correctly. Delivers comprehensive syntax support matching the official gram notation specification.

**Acceptance Scenarios**:

1. **Given** the test corpus from tree-sitter-gram repository, **When** all test cases are executed, **Then** all tests pass for both serialization and parsing operations
2. **Given** gram notation with line comments (`//`), **When** parsed and serialized, **Then** comments are handled appropriately (preserved, stripped, or configurable)
3. **Given** gram notation with end-of-line comments, **When** parsed and serialized, **Then** comments are handled appropriately
4. **Given** all value types from gram notation (integers, decimals, booleans, strings, symbols, tagged strings, arrays, maps, ranges, measurements), **When** serialized and parsed, **Then** round-trip conversion preserves all value types correctly
5. **Given** complex nested pattern structures, **When** serialized and parsed, **Then** the structure is preserved correctly through round-trip conversion
6. **Given** edge cases from the test corpus (empty patterns, deeply nested structures, large patterns), **When** processed, **Then** all edge cases are handled correctly without errors

---

### User Story 4 - Evaluate and Select Parsing Library (Priority: P2)

Developers need a reliable, maintainable parsing solution for gram notation. The library should evaluate whether to use tree-sitter-gram (with Haskell bindings) or an alternative Haskell parsing library (e.g., Parsec, Megaparsec), considering factors such as syntax coverage, performance, maintainability, and integration complexity.

**Why this priority**: The parsing library choice affects long-term maintainability and development velocity. While important, this evaluation can be done in parallel with initial implementation planning. The decision should be made before final implementation to avoid rework.

**Independent Test**: Can be fully tested by creating evaluation criteria, comparing tree-sitter-gram and alternative libraries against those criteria, documenting the decision rationale, and verifying that the selected library can handle all required syntax. Delivers a well-reasoned parsing library selection.

**Acceptance Scenarios**:

1. **Given** evaluation criteria are defined (syntax coverage, performance, maintainability, integration complexity, stability), **When** tree-sitter-gram and alternatives are evaluated, **Then** a comparison document is created with pros and cons for each option
2. **Given** tree-sitter-gram is evaluated, **When** Haskell bindings are tested, **Then** the evaluation includes assessment of binding stability, integration complexity, and FFI requirements
3. **Given** alternative Haskell parsing libraries are evaluated, **When** syntax coverage is tested, **Then** the evaluation confirms whether they can handle all gram notation syntax features
4. **Given** a parsing library is selected, **When** the decision is documented, **Then** the rationale includes clear justification based on evaluation criteria and project requirements
5. **Given** the selected parsing library, **When** integration is implemented, **Then** it successfully handles all syntax features required by the test corpus

---

### Edge Cases

- What happens when serializing a Pattern Subject with deeply nested structures (100+ levels)?
- How does the system handle Pattern Subject with very large property records (1000+ properties)?
- What happens when parsing gram notation with malformed syntax (unclosed brackets, mismatched delimiters)?
- How does the system handle gram notation with special characters in strings, symbols, or property keys?
- What happens when serializing Pattern Subject with circular references (if such structures are possible)?
- How does the system handle gram notation with Unicode characters in strings, labels, or symbols?
- What happens when parsing gram notation with inconsistent whitespace or formatting?
- How does the system handle Pattern Subject with empty labels set vs. no labels in gram notation?
- What happens when serializing Pattern Subject with anonymous identity vs. gram notation anonymous subjects?
- How does the system handle round-trip conversion (serialize then parse) for all value types and structures?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a serialization function that converts Pattern Subject to gram notation string format
- **FR-002**: System MUST provide a parsing function that converts gram notation string to Pattern Subject with proper error handling
- **FR-003**: System MUST support serialization of all standard value types (integers, decimals, booleans, strings, symbols) in property records
- **FR-004**: System MUST support serialization of all extended value types (tagged strings, arrays, maps, ranges, measurements) in property records
- **FR-005**: System MUST support serialization of Pattern Subject with identity, labels, and properties
- **FR-006**: System MUST support serialization of nested Pattern structures (patterns containing other patterns as elements)
- **FR-007**: System MUST support serialization of relationship patterns (source-relationship-target structures)
- **FR-008**: System MUST handle anonymous subjects appropriately during serialization (either omit identity or generate placeholder)
- **FR-009**: System MUST support parsing of all standard value types from gram notation
- **FR-010**: System MUST support parsing of all extended value types from gram notation
- **FR-011**: System MUST support parsing of Pattern Subject with identity, labels, and properties from gram notation
- **FR-012**: System MUST support parsing of nested Pattern structures from gram notation
- **FR-013**: System MUST support parsing of relationship patterns from gram notation
- **FR-014**: System MUST handle anonymous subjects appropriately during parsing (assign identity or preserve empty symbol)
- **FR-015**: System MUST provide clear error messages when parsing fails due to syntax errors
- **FR-016**: System MUST support all syntax features documented in tree-sitter-gram (including comments, all value types, pattern structures)
- **FR-017**: System MUST use the test corpus from tree-sitter-gram repository for validation
- **FR-018**: System MUST evaluate tree-sitter-gram vs. alternative Haskell parsing libraries before final implementation
- **FR-019**: System MUST document the parsing library selection decision with clear rationale
- **FR-020**: System MUST ensure round-trip conversion (serialize then parse) preserves Pattern Subject structure and values for all supported syntax

### Key Entities *(include if feature involves data)*

- **Pattern Subject**: A Pattern data structure with Subject values, representing the primary data type for serialization. Contains recursive structure (elements) and Subject decoration (value) with identity, labels, and properties.

- **Gram Notation**: Text format representation of Pattern Subject structures, following the gram notation specification. Includes syntax for subjects, properties, values, patterns, and relationships.

- **Value Types**: Rich value system supporting standard types (integers, decimals, booleans, strings, symbols) and extended types (tagged strings, arrays, maps, ranges, measurements) for property records.

- **Parse Error**: Error type representing parsing failures, including syntax errors, unexpected tokens, incomplete input, and other parsing issues.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Serialization function successfully converts 100% of valid Pattern Subject structures to gram notation format, including all value types, nested patterns, and relationship patterns
- **SC-002**: Parsing function successfully converts 100% of valid gram notation strings to Pattern Subject structures, handling all syntax features from tree-sitter-gram
- **SC-003**: All test cases from tree-sitter-gram test corpus pass for both serialization and parsing operations, demonstrating complete syntax support
- **SC-004**: Round-trip conversion (serialize then parse) preserves Pattern Subject structure and values with 100% accuracy for all supported syntax features
- **SC-005**: Parsing function provides clear, actionable error messages for 100% of invalid gram notation inputs, enabling developers to identify and fix syntax errors
- **SC-006**: Evaluation of parsing libraries is completed with documented comparison and selection rationale before final implementation begins
- **SC-007**: Selected parsing library successfully handles all syntax features required by tree-sitter-gram test corpus without limitations or workarounds
- **SC-008**: Serialization and parsing functions handle edge cases correctly, including deeply nested structures (100+ levels), large property records (1000+ properties), and special characters in strings/symbols
- **SC-009**: Library documentation clearly explains serialization and parsing usage, including examples for all value types and pattern structures
- **SC-010**: Performance meets practical usage requirements: serialization completes in under 1 second for patterns with 10,000+ nodes, parsing completes in under 1 second for gram notation files up to 1MB

## Assumptions

- Pattern and Subject data types are already implemented and stable (from previous features)
- All value types (standard and extended) are already implemented in the Subject library
- The tree-sitter-gram test corpus is accessible and can be integrated into the test suite
- Gram notation specification is stable and well-documented (via tree-sitter-gram)
- Anonymous subjects in gram notation can be handled by either omitting identity or generating placeholders during serialization
- Round-trip conversion may not preserve exact formatting (whitespace, comments) but must preserve structure and values
- The parsing library evaluation will consider both technical feasibility and long-term maintainability
- Comments in gram notation may be stripped during parsing (not preserved in Pattern Subject structure)
- The library will focus on Pattern Subject serialization (not generic Pattern serialization for other value types)

## Dependencies

- Pattern library must be complete (Feature 001-015) with stable Pattern type and operations
- Subject library must be complete with Subject type, Value types, and all value constructors
- Test infrastructure must be in place to run tree-sitter-gram test corpus
- Parsing library (tree-sitter-gram or alternative) must be available and integrable with Haskell project

## Out of Scope

- Generic serialization for Pattern with arbitrary value types (focus is on Pattern Subject only)
- Preservation of comments during round-trip conversion (comments may be stripped)
- Preservation of exact formatting/whitespace during round-trip conversion (structure and values are preserved, formatting may vary)
- Performance optimization beyond practical usage requirements (focus is on correctness and completeness)
- Support for gram notation features not covered by tree-sitter-gram
- Interactive parsing or incremental parsing (focus is on complete string parsing)
- Serialization to alternative formats (JSON, YAML, etc.) - only gram notation is supported
