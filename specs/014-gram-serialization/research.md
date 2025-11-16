# Research: Gram Serialization Library

**Feature**: 014-gram-serialization  
**Date**: 2025-01-27

## Overview

This research document addresses the technical decisions required for implementing the gram serialization library, with a focus on parsing library evaluation and integration strategies.

## Research Questions

### 1. Parsing Library Selection: tree-sitter-gram vs. Alternative Haskell Parsing Libraries

**Question**: Should we use tree-sitter-gram (with Haskell bindings) or an alternative Haskell parsing library (Parsec, Megaparsec) for parsing gram notation?

**Research Approach**:
- Evaluate tree-sitter-gram Haskell bindings (stability, FFI complexity, integration)
- Evaluate Parsec and Megaparsec for gram notation syntax coverage
- Compare syntax coverage, performance, maintainability, and integration complexity
- Consider long-term maintainability and development velocity

**Decision**: NEEDS EVALUATION - Decision will be made during implementation planning based on:
- Syntax coverage: Can the library handle all gram notation features?
- Integration complexity: How difficult is it to integrate with the Haskell project?
- Stability: Are Haskell bindings stable and well-maintained?
- Performance: Does it meet performance requirements (1 second for 1MB files)?
- Maintainability: Long-term support and community

**Alternatives Considered**:
- **tree-sitter-gram with Haskell bindings**: Pros: Comprehensive syntax support (designed for gram notation), incremental parsing capabilities. Cons: FFI complexity, potentially unstable Haskell bindings, C-based core.
- **Parsec**: Pros: Mature, stable, native Haskell, well-documented. Cons: May require manual grammar implementation, no incremental parsing, potentially slower for large files.
- **Megaparsec**: Pros: Better error messages than Parsec, active development, native Haskell. Cons: Similar to Parsec, requires manual grammar implementation.

**Rationale**: The evaluation must be completed before final implementation to avoid rework. The decision will be documented with clear justification based on evaluation criteria.

### 2. Tree-Sitter-Gram Test Corpus Integration

**Question**: How should we integrate the tree-sitter-gram test corpus for validation?

**Research Approach**:
- Examine tree-sitter-gram repository structure and test corpus format
- Determine integration strategy (git submodule, direct copy, test data files)
- Identify test corpus location and format
- Plan test execution strategy (unit tests, integration tests)

**Decision**: Integrate test corpus as test data files in the test suite. Tests will:
- Load test corpus files from a dedicated test data directory
- Execute serialization and parsing operations on each test case
- Verify round-trip conversion (serialize then parse) preserves structure
- Report failures with clear diagnostics

**Rationale**: Direct integration ensures comprehensive coverage of all syntax features. Test data files provide version control and easy updates when tree-sitter-gram test corpus changes.

**Alternatives Considered**:
- Git submodule: More complex dependency management, requires submodule updates
- Direct copy: Simpler but requires manual updates when corpus changes
- Test data files: Balanced approach, easy to maintain and version control

### 3. Anonymous Subject Handling Strategy

**Question**: How should we handle anonymous subjects (gram notation allows subjects without identity, but Subject data type requires identity)?

**Research Approach**:
- Review gram notation specification for anonymous subject syntax
- Examine Subject data type requirements (identity is mandatory)
- Evaluate strategies: omit identity in serialization, generate placeholders, preserve empty symbol

**Decision**: Use empty Symbol ("") to represent anonymous subjects in Pattern Subject. During serialization:
- If identity is empty Symbol (""), omit identity in gram notation output (anonymous subject syntax)
- During parsing, if gram notation has anonymous subject, assign empty Symbol ("") as identity

**Rationale**: This approach maintains consistency with Subject data type requirements while supporting gram notation anonymous subjects. The empty Symbol serves as a sentinel value for anonymous subjects.

**Alternatives Considered**:
- Generate unique identifiers: Breaks round-trip conversion, adds complexity
- Optional identity in Subject: Requires breaking change to Subject data type
- Placeholder identifiers: Similar issues to generated identifiers

### 4. Comment Handling Strategy

**Question**: How should comments in gram notation be handled during parsing and serialization?

**Research Approach**:
- Review tree-sitter-gram comment syntax (line comments `//`, end-of-line comments)
- Determine whether comments should be preserved, stripped, or configurable
- Consider impact on round-trip conversion

**Decision**: Comments are stripped during parsing (not preserved in Pattern Subject structure). During serialization, comments are not generated (output is comment-free). This is acceptable per specification (comments may be stripped, structure and values are preserved).

**Rationale**: Comments are formatting/metadata, not part of the data structure. Stripping comments simplifies parsing and maintains focus on structure and values. The specification explicitly allows comment stripping.

**Alternatives Considered**:
- Preserve comments: Requires additional data structure to store comments, adds complexity
- Configurable comment handling: Adds API complexity for minimal value
- Generate comments: Not necessary, adds serialization complexity

### 5. Round-Trip Conversion Strategy

**Question**: How should we ensure round-trip conversion (serialize then parse) preserves Pattern Subject structure and values?

**Research Approach**:
- Identify potential loss points (formatting, comments, whitespace)
- Design serialization to produce canonical output
- Design parsing to handle format variations
- Plan property-based tests for round-trip verification

**Decision**: Round-trip conversion will preserve structure and values, but not exact formatting (whitespace, comments). Property-based tests will verify:
- Structure preservation (Pattern elements, Subject components)
- Value preservation (all value types correctly preserved)
- Edge cases (deep nesting, large property records, special characters)

**Rationale**: Structure and values are the essential data; formatting is presentation. This approach balances correctness with practical implementation. Property-based tests provide comprehensive verification.

**Alternatives Considered**:
- Preserve exact formatting: Requires complex whitespace tracking, adds significant complexity
- Canonical formatting: Simpler approach, focuses on essential data preservation

### 6. Value Type Serialization Strategy

**Question**: How should all value types (standard and extended) be serialized to gram notation?

**Research Approach**:
- Review gram notation specification for value type syntax
- Map each Value constructor to gram notation syntax
- Handle nested values (arrays, maps containing other values)
- Ensure proper escaping and quoting

**Decision**: Each Value type will have a dedicated serialization function:
- Standard types: Direct mapping to gram notation (integers, decimals, booleans, strings with proper escaping, symbols)
- Extended types: Follow gram notation syntax (tagged strings with backtick syntax, arrays with brackets, maps with curly braces, ranges with `..` syntax, measurements with unit syntax)

**Rationale**: Direct mapping ensures correctness and maintainability. Each value type has clear gram notation syntax that can be implemented systematically.

**Alternatives Considered**:
- Generic serialization: Less clear, harder to maintain and verify
- Template-based: Adds complexity without clear benefit

### 7. Error Handling Strategy

**Question**: How should parsing errors be represented and reported?

**Research Approach**:
- Review existing ParseError type in Gram.Parse module
- Determine error information needed (position, context, error type)
- Design error messages for developer usability

**Decision**: Use ParseError type with descriptive error messages. Error messages will include:
- Error type (syntax error, unexpected token, incomplete input)
- Position information (line, column if available from parser)
- Context (what was expected, what was found)
- Clear, actionable guidance for fixing errors

**Rationale**: Clear error messages are essential for developer experience. The ParseError type provides a foundation that can be extended with position information if the selected parser supports it.

**Alternatives Considered**:
- Minimal error messages: Poor developer experience
- Complex error types: May be overkill for initial implementation, can be extended later

## Technical Decisions Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Parsing Library | NEEDS EVALUATION | Must evaluate tree-sitter-gram vs. Parsec/Megaparsec before implementation |
| Test Corpus Integration | Test data files | Direct integration ensures comprehensive coverage, easy to maintain |
| Anonymous Subjects | Empty Symbol ("") | Maintains Subject data type requirements while supporting gram notation |
| Comment Handling | Strip comments | Comments are formatting, not data; specification allows stripping |
| Round-Trip Conversion | Preserve structure/values, not formatting | Focuses on essential data preservation |
| Value Type Serialization | Direct mapping per type | Clear, maintainable, verifiable |
| Error Handling | ParseError with descriptive messages | Essential for developer experience |

## Next Steps

1. Complete parsing library evaluation (tree-sitter-gram vs. Parsec/Megaparsec)
2. Document parsing library selection decision with rationale
3. Integrate tree-sitter-gram test corpus as test data files
4. Implement serialization functions for all value types
5. Implement parsing functions with comprehensive error handling
6. Implement property-based tests for round-trip conversion

## References

- tree-sitter-gram repository: https://github.com/gram-data/tree-sitter-gram
- Gram notation specification (via tree-sitter-gram)
- Subject library: `libs/subject/` (Subject type, Value types)
- Pattern library: `libs/pattern/` (Pattern type)

