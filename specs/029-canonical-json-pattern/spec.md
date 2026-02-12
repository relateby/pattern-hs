# Feature Specification: Canonical JSON Pattern Representation

**Feature Branch**: `029-canonical-json-pattern`  
**Created**: 2026-01-10  
**Status**: Draft  
**Input**: User description: "Work on first-class support for a canonical JSON representation of Pattern<Subject> which includes roundtrip testing to/from Gram notation and enhancements to `gramref` to both use the format and produce a JSON schema for it, as described in this conversation."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Access Canonical JSON Format Documentation (Priority: P1)

As a developer porting the pattern-hs project to another language, I need a formally specified canonical JSON representation of Pattern<Subject> so that I have a clear, unambiguous target format to implement in my port.

**Why this priority**: This is foundational - without a formal specification, downstream projects must reverse-engineer the JSON format from code, leading to inconsistencies and bugs. A canonical format enables interoperability between all gram implementations.

**Independent Test**: Can be fully tested by requesting the JSON schema from gramref, verifying it contains complete definitions for all Pattern<Subject> components (value, elements, symbol, labels, properties, value types), and confirming it validates against known good JSON outputs.

**Acceptance Scenarios**:

1. **Given** gramref is installed, **When** I run a schema command with JSON Schema output format, **Then** I receive a complete JSON Schema document defining the Pattern<Subject> structure
2. **Given** an existing valid Pattern JSON output, **When** I validate it against the generated schema, **Then** the validation passes
3. **Given** the JSON schema document, **When** I examine it, **Then** it includes definitions for all value types (integer, decimal, boolean, string, symbol, tagged string, array, map, range, measurement)
4. **Given** the schema document, **When** I check its versioning, **Then** it includes a version number matching the gramref version
5. **Given** an invalid Pattern JSON (missing required fields), **When** I validate it against the schema, **Then** the validation fails with clear error messages

---

### User Story 2 - Convert Between Gram Notation and JSON (Priority: P1)

As a developer working with gram patterns, I need to reliably convert patterns between gram notation and canonical JSON format so that I can work in whichever format is most convenient for my use case while maintaining data integrity.

**Why this priority**: Bidirectional conversion is essential for interoperability. Developers need to use gram notation for human readability and JSON for machine processing, and must be confident the conversion is lossless.

**Independent Test**: Can be fully tested by converting a pattern from gram notation to JSON, then back to gram notation, and verifying the result is semantically identical to the original (roundtrip test). This works independently without other features.

**Acceptance Scenarios**:

1. **Given** a pattern in gram notation, **When** I convert it to canonical JSON, **Then** the JSON output contains all pattern information (identity, labels, properties, nested elements)
2. **Given** a pattern in canonical JSON, **When** I convert it to gram notation, **Then** the gram notation output represents the complete pattern
3. **Given** a simple pattern, **When** I perform a roundtrip conversion (gram → JSON → gram), **Then** the final output is semantically identical to the original
4. **Given** a complex nested pattern with all value types, **When** I perform a roundtrip conversion, **Then** all data is preserved without loss
5. **Given** a pattern with special characters in properties, **When** I perform roundtrip conversion, **Then** the special characters are correctly preserved and escaped

---

### User Story 3 - Verify JSON Serialization Correctness (Priority: P1)

As a maintainer of the gram reference implementation, I need automated roundtrip tests for JSON serialization so that I can verify that the JSON format correctly represents all possible patterns and that conversions are lossless.

**Why this priority**: Without comprehensive roundtrip testing, there's no guarantee the JSON format is correct or complete. This could lead to silent data loss or corruption, breaking downstream applications. Automated testing ensures reliability.

**Independent Test**: Can be fully tested by running the test suite against a corpus of patterns, verifying that every pattern successfully roundtrips (gram → JSON → gram produces equivalent output), and confirming that all value types and pattern structures are covered. This is independently valuable for quality assurance.

**Acceptance Scenarios**:

1. **Given** a test suite with diverse pattern structures, **When** I run roundtrip tests, **Then** 100% of valid patterns successfully roundtrip without data loss
2. **Given** a pattern with all supported value types, **When** I perform roundtrip testing, **Then** every value type is correctly serialized and deserialized
3. **Given** deeply nested patterns, **When** I run roundtrip tests, **Then** the nesting structure is preserved exactly
4. **Given** patterns with edge cases (empty properties, anonymous subjects, implicit root), **When** I test roundtrips, **Then** all edge cases are handled correctly
5. **Given** the existing tree-sitter-gram test corpus, **When** I run roundtrip tests on it, **Then** all corpus examples pass roundtrip validation

---

### User Story 4 - Generate Type Definitions from Schema (Priority: P2)

As a developer creating a port in another language, I need to generate native type definitions from the JSON schema so that I can implement type-safe Pattern structures in my target language without manual translation errors.

**Why this priority**: Type generation from schema accelerates ports and ensures consistency. However, developers can manually implement types from the schema, so this is lower priority than having the schema itself.

**Independent Test**: Can be fully tested by generating type definitions for a specific language format (TypeScript, Rust, etc.), verifying they compile successfully in that language, and confirming they correctly represent the Pattern<Subject> structure. This feature can be delivered independently after the schema exists.

**Acceptance Scenarios**:

1. **Given** the JSON schema is available, **When** I request TypeScript type definitions, **Then** I receive valid TypeScript interface definitions for Pattern<Subject>
2. **Given** the JSON schema is available, **When** I request Rust type definitions with serde annotations, **Then** I receive valid Rust struct definitions
3. **Given** generated type definitions, **When** I use them in my project, **Then** they correctly model the Pattern<Subject> structure
4. **Given** type definitions in my target language, **When** I parse JSON using them, **Then** type checking catches invalid JSON at compile time or with clear runtime errors
5. **Given** schema updates over time, **When** I regenerate type definitions, **Then** they reflect all schema changes

---

### User Story 5 - Validate Custom JSON Output (Priority: P2)

As a developer creating a gram port in another language, I need to validate my implementation's JSON output against the canonical schema so that I can verify my port produces correct output before investing in comprehensive functional tests.

**Why this priority**: Schema-based validation enables early detection of implementation issues in ports. However, it's supplementary to functional testing, so lower priority than the foundational schema and roundtrip testing.

**Independent Test**: Can be fully tested by taking JSON output from a port implementation, running schema validation, and verifying that valid outputs pass and invalid outputs fail with helpful error messages. This works independently as long as the schema exists.

**Acceptance Scenarios**:

1. **Given** JSON output from my port implementation that matches the canonical format, **When** I validate it against the schema, **Then** validation passes
2. **Given** JSON output with a missing required field, **When** I validate it against the schema, **Then** validation fails with a clear error indicating which field is missing
3. **Given** JSON output with an incorrect value type, **When** I validate it against the schema, **Then** validation fails with a clear error indicating the type mismatch
4. **Given** a batch of JSON outputs from my test suite, **When** I validate them all, **Then** I receive a summary report showing which outputs are valid and which have errors
5. **Given** JSON output that's valid but uses non-canonical formatting, **When** I validate it, **Then** validation passes (format doesn't matter, only structure)

---

### Edge Cases

- What happens when roundtrip conversion is attempted on malformed gram notation? (Should fail gracefully with clear parse error)
- What happens when JSON is missing optional fields like labels or properties? (Should successfully roundtrip with empty sets/maps)
- How does the system handle patterns with circular references if they exist? (Should either prevent them or handle them explicitly in schema)
- What happens when converting extremely large patterns (thousands of nested elements)? (Should handle without performance degradation or stack overflow)
- How are schema versions handled when older JSON outputs are validated against newer schemas? (Should support versioning and backward compatibility)
- What happens when requesting schema generation for an unsupported format? (Should fail with clear error listing supported formats)
- How does canonical JSON handle different value types that don't exist in basic JSON (like symbols or ranges)? (Should use object representation with type discriminator)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST define a canonical JSON representation for Pattern<Subject> that includes value (subject), elements (nested patterns), symbol, labels (set), properties (map), and all supported value types
- **FR-002**: System MUST support bidirectional conversion between gram notation and canonical JSON format
- **FR-003**: System MUST ensure roundtrip conversion (gram → JSON → gram) produces semantically equivalent output for all valid patterns
- **FR-004**: System MUST provide a way to generate JSON Schema documents that formally specify the canonical JSON format
- **FR-005**: System MUST ensure generated JSON Schema includes version information
- **FR-006**: System MUST support roundtrip testing across the entire tree-sitter-gram test corpus
- **FR-007**: System MUST handle all value types in JSON representation: integer, decimal, boolean, string, symbol, tagged string, array, map, range, measurement
- **FR-008**: System MUST represent nested pattern structures in JSON with arbitrary depth
- **FR-009**: System MUST support schema generation in multiple output formats: JSON Schema, TypeScript, Rust with serde annotations
- **FR-010**: System MUST validate JSON outputs against the canonical schema to verify correctness
- **FR-011**: System MUST provide clear error messages when JSON validation fails, indicating which fields or types are incorrect
- **FR-012**: System MUST preserve all pattern information during roundtrip conversion including: identity (symbol), labels, properties with all value types, element order, and nesting structure
- **FR-013**: System MUST handle edge cases in roundtrip conversion: empty properties, empty labels, anonymous subjects, implicit root patterns
- **FR-014**: System MUST ensure canonical JSON format uses deterministic serialization (sorted keys, consistent formatting) to enable reliable comparison
- **FR-015**: System MUST document the canonical JSON format specification including structure, value type representations, and versioning scheme

### Key Entities

- **Canonical JSON Pattern**: A standardized JSON representation of Pattern<Subject> that includes a "value" object (containing symbol, labels array, properties map) and an "elements" array (containing nested patterns in the same format). All value types use consistent JSON representations with type discriminators where needed.

- **JSON Schema Document**: A formal specification document in JSON Schema format that defines the structure, required fields, optional fields, value types, and validation rules for the canonical JSON pattern format. Includes version information and is machine-readable for automated validation.

- **Roundtrip Test**: A verification test that converts a pattern from one format to another and back, then compares the result to the original to ensure no data loss or corruption. For this feature: gram notation → JSON → gram notation, with semantic equivalence checking.

- **Value Type Representation**: The specific JSON structure used to represent each gram value type. Simple types (integer, decimal, boolean, string) use native JSON types. Complex types (symbol, tagged string, range, measurement) use JSON objects with "type" discriminator fields.

- **Schema Generation Output**: Type definitions or schema documents generated from the canonical JSON Schema in various target formats (TypeScript interfaces, Rust structs, JSON Schema documents) for use in downstream projects.

## Assumptions

- The existing JSON serialization in gramref CLI (`Gramref.CLI.JSON`) serves as the basis for the canonical format
- JSON Schema version 2020-12 or later will be used for schema generation
- Semantic equivalence for roundtrip testing means structural and data equivalence, not necessarily textual equivalence (formatting differences are acceptable)
- Type generation will support commonly used languages and serialization frameworks (TypeScript, Rust/serde) initially
- The canonical format will use standard JSON (no JSON5 or other extensions)
- Version information in schema will follow semantic versioning (major.minor.patch)
- Downstream projects have access to standard JSON Schema validators in their target languages

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can obtain a complete JSON Schema document for Pattern<Subject> that successfully validates 100% of valid JSON outputs from gramref
- **SC-002**: Roundtrip conversion testing achieves 100% pass rate across the entire tree-sitter-gram test corpus (all valid patterns roundtrip without data loss)
- **SC-003**: Developers can convert any valid pattern from gram notation to JSON and back with verified semantic equivalence
- **SC-004**: Generated type definitions compile successfully in their target language and correctly model the Pattern<Subject> structure
- **SC-005**: Downstream port implementations can validate their JSON output against the canonical schema and receive clear error messages for any discrepancies
- **SC-006**: The canonical JSON format specification is complete enough that developers can implement Pattern<Subject> serialization/deserialization in any language without referring to the Haskell source code
- **SC-007**: Roundtrip testing identifies any value type or pattern structure that isn't correctly handled by JSON serialization (0% silent data loss)
- **SC-008**: Documentation clearly explains how each gram value type is represented in JSON, enabling developers to understand the format without trial and error
