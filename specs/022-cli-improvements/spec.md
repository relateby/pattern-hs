# Feature Specification: CLI Tool Improvements for Language Porting

**Feature Branch**: `022-cli-improvements`  
**Created**: 2025-12-27  
**Status**: Draft  
**Input**: User description: "improve the gramref CLI tool to make it easier to port this project to other languages. See @design/pattern-hs-cli-improvements.md for details"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Compare Outputs Without Metadata (Priority: P1)

As a developer porting the pattern-hs project to another language, I need to compare outputs between implementations without metadata fields that change on every execution, so that I can verify equivalence between implementations using simple text comparison.

**Why this priority**: This is the most fundamental capability needed for equivalence checking. Without the ability to exclude metadata, developers must write custom comparison logic to ignore timestamp and hash fields, making automated testing and validation significantly more complex. This blocks all automated equivalence checking workflows.

**Independent Test**: Can be fully tested by running the CLI with `--value-only` flag on a known input, verifying that the output contains only the result value without metadata wrapper, and confirming that identical inputs produce identical outputs across multiple runs. This delivers the foundational capability for automated equivalence checking between implementations.

**Acceptance Scenarios**:

1. **Given** a gramref CLI command that produces JSON output with metadata, **When** I add the `--value-only` flag, **Then** the output contains only the result value (or error) without any metadata wrapper
2. **Given** the same input run multiple times with `--value-only`, **When** I compare the outputs, **Then** they are identical (no changing timestamps or hashes)
3. **Given** a command that produces an error, **When** I use `--value-only`, **Then** the output contains only the error object without metadata
4. **Given** multiple commands with `--value-only`, **When** I compare their outputs, **Then** I can use simple text comparison tools to verify equivalence

---

### User Story 2 - Generate Deterministic Test Suites (Priority: P1)

As a developer porting the pattern-hs project to another language, I need to generate test suites with deterministic outputs so that I can create comprehensive test cases for validating my implementation against the reference implementation.

**Why this priority**: Test suite generation is essential for creating comprehensive test coverage when porting. Without this capability, developers must manually create test cases, which is time-consuming and error-prone. The ability to generate deterministic test suites enables automated test data extraction and ensures consistent test cases across development cycles.

**Independent Test**: Can be fully tested by running the generate command with `--type suite` and a fixed seed, verifying that the output conforms to the test suite format specification, and confirming that the same seed produces identical test cases across multiple runs. This delivers automated test case generation for port validation.

**Acceptance Scenarios**:

1. **Given** the generate command with `--type suite` and a seed value, **When** I run it, **Then** the output contains a valid test suite in the specified format with test cases
2. **Given** the same seed value used multiple times, **When** I generate test suites, **Then** the output is identical across all runs (deterministic)
3. **Given** different complexity levels specified, **When** I generate test suites, **Then** the generated test cases reflect the requested complexity level
4. **Given** a generated test suite, **When** I validate it against the test suite format specification, **Then** it conforms to the required schema
5. **Given** a generated test suite, **When** I examine the test cases, **Then** they cover various pattern structures and operations suitable for validation

---

### User Story 3 - Produce Canonical JSON for Reliable Comparison (Priority: P1)

As a developer porting the pattern-hs project to another language, I need canonical JSON output with sorted keys and consistent formatting so that I can reliably compare outputs using exact text matching without dealing with non-deterministic key ordering.

**Why this priority**: JSON key ordering can vary between implementations and runs, making comparison unreliable. Canonical output ensures that equivalent data structures produce identical JSON strings, enabling reliable automated comparison. This is essential for snapshot testing and equivalence verification.

**Independent Test**: Can be fully tested by running commands with `--canonical` flag, verifying that JSON keys are sorted at all nesting levels, and confirming that equivalent data structures produce identical JSON output. This delivers reliable comparison capability for automated testing.

**Acceptance Scenarios**:

1. **Given** a command that produces JSON output, **When** I add the `--canonical` flag, **Then** all JSON keys are sorted alphabetically at every nesting level
2. **Given** the same data structure output multiple times with `--canonical`, **When** I compare the outputs, **Then** they are byte-for-byte identical
3. **Given** `--canonical` combined with `--value-only`, **When** I run a command, **Then** the output is both canonical and metadata-free
4. **Given** JSON output with nested structures, **When** I use `--canonical`, **Then** keys are sorted at all nesting levels, not just the top level

---

### Edge Cases

- What happens when `--value-only` is used with a command that produces no result? (Should output empty object or appropriate empty structure)
- What happens when `--deterministic` is combined with `--canonical`? (Should produce fully deterministic output with sorted keys)
- What happens when test suite generation is requested with count=0? (Should produce valid empty test suite structure)
- What happens when test suite generation uses an invalid seed? (Should handle gracefully with clear error message)
- What happens when `--canonical` is used with malformed data? (Should still produce valid canonical JSON or clear error)
- What happens when multiple output format flags conflict? (Should have clear precedence rules or error message)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST support a `--value-only` flag that outputs only the result value (or error) without metadata wrapper when JSON format is used
- **FR-002**: System MUST support a `--deterministic` flag that ensures all metadata uses fixed values or is excluded to produce deterministic output
- **FR-003**: System MUST allow `--value-only` and `--deterministic` flags to be combined
- **FR-004**: System MUST implement `generate --type suite` command that outputs test cases in the test suite format specification
- **FR-005**: System MUST ensure test suite generation is deterministic when using `--seed` parameter (same seed produces identical output)
- **FR-006**: System MUST support `--complexity` parameter for test suite generation with levels: minimal, basic, standard, complex, adversarial
- **FR-007**: System MUST ensure generated test suites conform to the test suite format specification schema
- **FR-008**: System MUST support a `--canonical` flag that produces JSON with sorted keys at all nesting levels
- **FR-009**: System MUST ensure `--canonical` output has consistent formatting (no optional whitespace, no trailing commas)
- **FR-010**: System MUST allow `--canonical` to be combined with `--value-only` and `--deterministic` flags
- **FR-011**: System MUST ensure `--canonical` is automatically enabled when `--deterministic` is used

### Key Entities *(include if feature involves data)*

- **Test Suite**: A structured collection of test cases that includes metadata (version), test case definitions with names, descriptions, inputs, expected outputs, and operations. Used for automated validation of implementations against the reference implementation.

- **Canonical JSON**: JSON output format where all object keys are sorted alphabetically at every nesting level, formatting is consistent, and output is deterministic across runs for equivalent data structures.

- **Result Value**: The core output data from a CLI command operation, excluding metadata such as timestamps, hashes, and command information. Represents the actual pattern, error, or operation result.

## Assumptions

- The test suite format specification exists and defines the required schema for test suite output
- JSON serialization libraries support key sorting and consistent formatting
- Seed-based random generation is available for deterministic test case generation
- Users will combine flags appropriately (e.g., `--canonical --value-only` for clean comparison output)
- Test suite generation will cover various pattern structures and operations as needed for comprehensive testing

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can use `--value-only` flag to produce output that contains only result values, enabling 100% of equivalence checks to use simple text comparison without custom metadata filtering
- **SC-002**: Developers can generate test suites with `--type suite` that conform to the test suite format specification, with 100% of generated suites passing schema validation
- **SC-003**: Test suite generation produces identical output when using the same seed value, with 100% determinism across multiple runs
- **SC-004**: Developers can use `--canonical` flag to produce JSON output where equivalent data structures produce byte-for-byte identical JSON strings, enabling 100% reliable automated comparison
- **SC-005**: All new flags can be combined appropriately (e.g., `--canonical --value-only --deterministic`) to produce clean, deterministic, comparable output suitable for automated testing
- **SC-006**: Generated test suites include test cases covering various complexity levels (minimal through adversarial) when requested, enabling comprehensive validation coverage
