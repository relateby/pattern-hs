# Implementation Plan: Subject Identity and Serialization

**Branch**: `020-subject-serialization` | **Date**: 2025-11-29 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/020-subject-serialization/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement robust round-trip serialization for `Subject` instances to/from gram notation. Key requirements include:
1.  `toGram`: Serialize `Subject` to valid gram string, handling escaping and structure.
2.  `fromGram`: Parse gram string to `Subject`, ensuring all subjects (including anonymous ones) receive a unique identity (FR-003).
3.  Ensure round-trip consistency (`fromGram . toGram == id`).

## Technical Context

**Language/Version**: Haskell (GHC 9.8/9.10)
**Primary Dependencies**: `megaparsec` (parsing), `containers` (Map/Set), `text` (likely for efficiency, though current code uses String)
**Storage**: N/A (In-memory data structures)
**Testing**: `hspec` for unit/property testing
**Target Platform**: Cross-platform (Library)
**Project Type**: Library (`libs/gram`, `libs/subject`)
**Performance Goals**: Efficient parsing/serialization for moderate graph sizes.
**Constraints**: Must maintain purity of `fromGram` (or use a pure State monad) to fit existing functional patterns.
**Scale/Scope**: Core library functionality.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: The design extends existing well-structured modules (`Gram.Parse`, `Gram.Serialize`, `Gram.Transform`).
- **Testing Standards (NON-NEGOTIABLE)**: Will include round-trip property tests and unit tests for anonymous ID generation.
- **Conceptual Consistency**: Adheres to `Subject` as the canonical data structure for graph attributes.
- **Mathematical Clarity**: ID generation strategy (sequential) ensures deterministic behavior, preserving referential transparency of the parse function.
- **Multi-Language Reference Alignment**: The generated ID strategy should be simple enough to implement in other languages (e.g., counter-based).

**Violations must be documented in Complexity Tracking section below.**

## Project Structure

### Documentation (this feature)

```text
specs/020-subject-serialization/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output
```

### Source Code (repository root)

```text
libs/
├── gram/
│   ├── src/
│   │   └── Gram/
│   │       ├── Parse.hs       # Update to return CST
│   │       ├── Serialize.hs   # Update/Verify serialization
│   │       └── Transform.hs   # Update to handle ID generation
│   └── tests/
│       └── Spec/
│           └── Gram/
│               ├── ParseSpec.hs     # Add anonymous ID tests
│               └── SerializeSpec.hs # Add round-trip tests
└── subject/
    └── src/
        └── Subject/
            └── Core.hs        # Reference for Subject type
```

**Structure Decision**: Enhance existing `gram` library modules. No new projects/libraries needed.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | | |
