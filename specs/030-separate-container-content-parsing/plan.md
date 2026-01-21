# Implementation Plan: Separation of Container and Content Parsing

**Branch**: `030-separate-container-content-parsing` | **Date**: 2026-01-20 | **Spec**: [specs/030-separate-container-content-parsing/spec.md]
**Input**: Feature specification from `/specs/030-separate-container-content-parsing/spec.md`

## Summary

The core requirement is to refactor the `gram-hs` parsing and serialization behavior to distinguish between a Gram Document (container) and its constituent Patterns (content). This aligns `gram-hs` with the `gram-rs` implementation and improves document-level metadata handling.

## Technical Context

**Language/Version**: Haskell 2010 (GHC 9.4+)
**Primary Dependencies**: `megaparsec`, `containers`, `pattern-core`, `subject`
**Storage**: N/A (Codec library)
**Testing**: `hspec`, `QuickCheck`
**Target Platform**: Multi-platform (reference library)
**Project Type**: Library + CLI
**Performance Goals**: SC-001 (Efficiently handle 10,000+ top-level patterns)
**Constraints**: Cross-language alignment with `gram-rs`.

## Constitution Check

*GATE: Passed.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: Pass. The separation improves clarity.
- **Testing Standards (NON-NEGOTIABLE)**: Pass. Will add round-trip property tests for headers and streams.
- **Conceptual Consistency**: Pass. Explicitly models Gram documents as pattern streams.
- **Mathematical Clarity**: Pass. The mapping between `[Pattern]` and the legacy `Gram.Root` pattern is well-defined.
- **Multi-Language Reference Alignment**: Pass. This is the primary driver for the change.

## Project Structure

### Documentation (this feature)

```text
specs/030-separate-container-content-parsing/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── checklists/          # Validation checklists
└── contracts/           # API contracts
```

### Reference Documentation (global)

- `docs/reference/PORTING-GUIDE.md`: Update Phase 3 to include container-aware parsing and serialization.
- `docs/reference/features/gram-serialization.md`: Document the new `fromGramList`, `fromGramWithHeader`, etc.
- `docs/reference/SPECIFICATION.md`: Update the Gram library feature table to include container support.

### Source Code (repository root)

```text
libs/gram/
├── src/
│   ├── Gram/
│   │   ├── Parse.hs      # Main changes to parsing API
│   │   ├── Serialize.hs  # Main changes to serialization API
│   │   ├── Transform.hs  # Support for new CST structure
│   │   └── CST.hs        # Potential updates to CST
│   └── Gram.hs           # Exporting new functions
└── tests/
    └── Spec/
        └── Gram/
            ├── ParseSpec.hs      # New tests for fromGramList/Header
            ├── SerializeSpec.hs  # New tests for toGramList/Header
            └── RoundtripSpec.hs  # Updated round-trip tests
```

**Structure Decision**: Standard Haskell library structure in `libs/gram`.

## Complexity Tracking

> No violations of Constitution Check.
