# Implementation Plan: Gram Serializer Updates

**Branch**: `017-gram-serializer-updates` | **Date**: 2025-11-28 | **Spec**: [specs/017-gram-serializer-updates/spec.md](spec.md)
**Input**: Feature specification from `specs/017-gram-serializer-updates/spec.md`

## Summary

The `gram` library's serializer needs to be updated to support the new Edge and Walk pattern structures introduced during the parsing conformance phase. Currently, the serializer only produces generic bracket notation (e.g., `[r | a, b]`). The goal is to produce idiomatic Gram path notation (e.g., `(a)-[r]->(b)`) for these specific structures, enabling round-trip correctness (`parse . serialize . parse == parse`) and improved readability.

The technical approach involves modifying `Gram.Serialize.toGram` to detect `Pattern` structures that match the definitions of Edge and Walk patterns and emit the corresponding path syntax.

## Technical Context

**Language/Version**: Haskell (GHC 9.6+)
**Primary Dependencies**: `pattern` (core library), `subject`, `text`, `containers`
**Testing**: `hspec` for unit and property tests, `QuickCheck` for properties
**Project Type**: Library (part of `pattern-hs` monorepo)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: The new serialization logic will reside in `Gram.Serialize`, keeping it encapsulated. Helper functions for detecting Edge/Walk patterns will be clearly named and documented.
- **Testing Standards (NON-NEGOTIABLE)**: The plan includes specific tests for round-trip correctness and verifying the output format of edges and walks. Existing corpus tests provide a baseline.
- **Conceptual Consistency**: The serialization mapping is the inverse of the parsing transformation defined in `Gram.Transform`. It respects the structure of the Pattern type.
- **Mathematical Clarity**: The mapping between text representation and Pattern structure is 1:1 for the canonical forms.
- **Multi-Language Reference Alignment**: This logic is specific to the textual representation but relies on the core Pattern structure, which is the cross-language reference.

**Violations**: None.

## Project Structure

### Documentation (this feature)

```text
specs/017-gram-serializer-updates/
├── plan.md              # This file
└── spec.md              # Feature specification
```

### Source Code (repository root)

```text
libs/gram/
├── src/
│   └── Gram/
│       ├── Serialize.hs    # MAIN EDIT: Update toGram logic
│       └── Transform.hs    # REFERENCE: Definitions of Edge/Walk structure
└── tests/
    └── Spec/
        └── Gram/
            ├── SerializeSpec.hs   # UPDATE: Add new serialization tests
            └── ParseSpec.hs       # VERIFY: Round-trip properties
```

**Structure Decision**: Modifying existing files in `libs/gram` as this is an update to existing functionality.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | N/A | N/A |

## Phases

### Phase 0: Research & Analysis
**Goal**: Identify the exact structure of Edge and Walk patterns to detect them reliably.

1. **Analyze `Gram.Transform`**: Determine the exact structural signature of "Edge" and "Walk" patterns (element counts, labels, nested structures).
2. **Review `Gram.Serialize`**: Understand the current recursive serialization logic to determine where to inject the path detection.

### Phase 1: Design
**Goal**: Design the detection and formatting logic.

1. **Define Predicates**:
    - `isEdgePattern :: Pattern Subject -> Maybe (Pattern Subject, Pattern Subject, Pattern Subject)` (returns relation, left, right)
    - `isWalkPattern :: Pattern Subject -> Maybe [Pattern Subject]` (returns list of edge patterns)
2. **Update `serializePattern`**: Design the branching logic to prioritize Path/Walk syntax over generic Pattern syntax.

### Phase 2: Implementation - Edge Serialization
**Goal**: Implement serialization for single edges.

1. **Implement `isEdgePattern`**: Based on Phase 0 analysis.
2. **Update `toGram`**: Add case for Edge Pattern serialization.
    - Format: `(left)-[relation]->(right)`
    - Handle anonymous relations: `(left)-->(right)`
3. **Add Tests**: Verify `(a)-[r]->(b)` output.

### Phase 3: Implementation - Walk Serialization
**Goal**: Implement serialization for walks.

1. **Implement `isWalkPattern`**: Detect the "Gram.Walk" label and structure.
2. **Update `toGram`**: Add case for Walk Pattern serialization.
    - Logic: Iterate through edges, ensuring connectivity `(a)->(b)` then `(b)->(c)` becomes `(a)->(b)->(c)`.
    - Handle disconnected segments (fallback).
3. **Add Tests**: Verify `(a)->(b)->(c)` output.

### Phase 4: Verification & Polish
**Goal**: Ensure round-trip correctness and clean up.

1. **Run Round-Trip Tests**: Verify `parse . serialize . parse == parse` on the corpus.
2. **Regression Testing**: Ensure standard patterns (nodes, records) still serialize correctly.
3. **Documentation**: Update module Haddock and `SYNTAX_NOTES.md`.
