# Implementation Plan: Codefence Multiline Strings

**Branch**: `024-codefence-strings` | **Date**: 2025-12-17 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/024-codefence-strings/spec.md`

## Summary

Add support for multiline strings using triple-backtick codefence syntax in both parsing (Gram.Parse) and serialization (Gram.Serialize). Plain codefences produce `VString` values; tagged codefences produce `VTaggedString` values. The serializer will automatically use codefence format for strings exceeding 120 characters (total length including newlines).

## Technical Context

**Language/Version**: Haskell (GHC 9.10.3)  
**Primary Dependencies**: Megaparsec (parser combinators), pattern library, subject library  
**Storage**: N/A (in-memory transformation)  
**Testing**: Hspec (unit tests), QuickCheck (property-based tests)  
**Target Platform**: Cross-platform (library)  
**Project Type**: Multi-library mono-repo (libs/gram within pattern-hs)  
**Performance Goals**: N/A (text processing, correctness is priority)  
**Constraints**: Must maintain 100% backward compatibility with existing gram files  
**Scale/Scope**: Small feature addition to existing parser/serializer

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ Design provides clear separation between codefence parsing and existing string parsing. New functions will have full Haddock documentation with examples.
- **Testing Standards (NON-NEGOTIABLE)**: ✅ Testing strategy includes unit tests for parser (happy paths, edge cases), unit tests for serializer (threshold behavior), property-based tests for round-trip correctness, and integration tests with example files.
- **Conceptual Consistency**: ✅ Design uses existing `VString` and `VTaggedString` types without modification, preserving the value type system's structure.
- **Mathematical Clarity**: ✅ Clear formal rules: codefence format iff length > 120 chars (total count including newlines). No ambiguity in threshold behavior.
- **Multi-Language Reference Alignment**: ✅ Codefence syntax aligns with tree-sitter-gram grammar used by all implementations. No Haskell-specific concerns in core design.

**Violations**: None identified.

## Project Structure

### Documentation (this feature)

```text
specs/024-codefence-strings/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
libs/gram/
├── src/
│   └── Gram/
│       ├── Parse.hs        # MODIFY: Add parseFencedString, parseTaggedFencedString
│       └── Serialize.hs    # MODIFY: Add codefence output for long strings
├── tests/
│   └── Spec/
│       └── Gram/
│           ├── ParseSpec.hs      # MODIFY: Add codefence parsing tests
│           └── SerializeSpec.hs  # MODIFY: Add codefence serialization tests

examples/
└── markdown.gram         # EXISTING: Test file with tagged codefence (already present)
```

**Structure Decision**: Single library within mono-repo. Changes are localized to the `gram` library's Parse and Serialize modules with corresponding test additions.

## Complexity Tracking

> No Constitution violations. Design is minimal and focused.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none) | - | - |
