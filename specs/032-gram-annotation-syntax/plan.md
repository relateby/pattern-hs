# Implementation Plan: Gram annotation-based identifiers and labels

**Branch**: `032-gram-annotation-syntax` | **Date**: 2026-02-17 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/032-gram-annotation-syntax/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Update the Haskell Gram parser to support the extended annotation syntax defined by the tree-sitter-gram grammar and corpus: property-style `@key(value)` (existing, keep), and identified/labeled `@@` forms (identifier-only, labels-only, identifier+labels). The parser must reject empty `@@` and produce a CST that distinguishes annotation kinds so downstream tools can consume them. Alignment is with `libs/gram/test-data/tree-sitter-gram/test/corpus/extended_annotations.txt` and the tree-sitter-gram 003-extended-annotation contract.

**Phase 0 Complete**: Research completed — see [research.md](./research.md)  
**Phase 1 Complete**: Design artifacts created — see [data-model.md](./data-model.md), [contracts/](./contracts/), [quickstart.md](./quickstart.md)

## Technical Context

**Language/Version**: Haskell (GHC 9.10.3)  
**Primary Dependencies**: megaparsec (parsing), hspec (testing)  
**Storage**: N/A (in-memory CST)  
**Testing**: hspec unit tests; tree-sitter-gram corpus runner (libs/gram test suite)  
**Target Platform**: Cross-platform (library)  
**Project Type**: Library (single project)  
**Performance Goals**: Parse typical pattern documents (dozens of annotated patterns) in under one second (SC-004)  
**Constraints**: No tree-sitter runtime dependency; pure Haskell parser must match tree-sitter-gram semantics  
**Scale/Scope**: Annotation rule changes in Gram.Parse and Gram.CST; corpus file extended_annotations.txt and existing corpus integration

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- **Code Quality (NON-NEGOTIABLE)**: Parser and CST changes will follow existing Gram.Parse/Gram.CST style; public types and parsers documented with purpose and invariants.
- **Testing Standards (NON-NEGOTIABLE)**: Corpus tests (extended_annotations.txt) plus unit tests for each annotation form and edge case (empty `@@`, invalid property).
- **Conceptual Consistency**: CST annotation model aligns with tree-sitter-gram AST (property_annotation vs identified_annotation; identifier/labels as direct fields).
- **Mathematical Clarity**: N/A for this parsing feature.
- **Multi-Language Reference Alignment**: Matching the canonical tree-sitter-gram grammar and corpus satisfies reference alignment.

**Status**: ✅ Constitution checks pass. No violations.

**Post Phase 1**: Re-checked after data-model, contracts, and quickstart. Design remains compliant; no additional complexity or violations.

## Project Structure

### Documentation (this feature)

```text
specs/032-gram-annotation-syntax/
├── plan.md              # This file
├── research.md          # Phase 0
├── data-model.md        # Phase 1
├── quickstart.md        # Phase 1
├── contracts/           # Phase 1
└── tasks.md             # Phase 2 output (/speckit.tasks — not created by plan)
```

### Source Code (repository root)

```text
libs/gram/
├── src/
│   └── Gram/
│       ├── CST.hs       # Extend Annotation to sum type (PropertyAnnotation | IdentifiedAnnotation)
│       └── Parse.hs     # Add @@ parsing; annotation stack (at most one identified, then property*)
├── tests/
│   └── Spec/
│       └── ...          # Corpus tests (extended_annotations.txt); unit tests for annotations
└── test-data/
    └── tree-sitter-gram/
        └── test/corpus/
            └── extended_annotations.txt   # Reference corpus
```

**Structure Decision**: Single library. Changes are localized to `libs/gram`: CST.hs (annotation data), Parse.hs (annotation and annotated-pattern parsing). Corpus is already under test-data/tree-sitter-gram; corpus runner from 016-gram-parsing-conformance is reused.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (None)    | —          | —                                   |
