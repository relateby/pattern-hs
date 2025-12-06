# Tasks: Update Gram Serialization for Tree-Sitter 0.2.7

## Phase 1: Setup

- [x] T001 Verify tree-sitter-gram submodule status in project root

## Phase 2: Foundational

- [x] T002 Update `Gram.Transform` to handle `AnnotatedPattern` by mapping annotations to Subject properties in `libs/gram/src/Gram/Transform.hs`
- [x] T003 Ensure `Gram.Serialize` handles `Gram.Root` label by flattening elements without brackets in `libs/gram/src/Gram/Serialize.hs`

## Phase 3: User Story 1 - Serialize Flat Top-Level Patterns

**Goal**: Patterns should be serialized as a flat sequence of top-level elements (nodes, subjects, paths) without wrapper brackets, and Annotated Patterns should be serialized as Subject Patterns with properties.

- [x] T004 [US1] Create test case for flat top-level serialization (nodes, paths) in `libs/gram/tests/Gram/SerializeSpec.hs`
- [x] T005 [US1] Create test case for annotated pattern serialization (verifying `[{props} | content]` output) in `libs/gram/tests/Gram/SerializeSpec.hs`
- [x] T006 [US1] Implement flat serialization logic in `toGram` in `libs/gram/src/Gram/Serialize.hs`
- [x] T007 [US1] Implement annotated pattern transformation in `libs/gram/src/Gram/Transform.hs`

## Phase 4: User Story 2 - Serialize Annotations Without Commas

**Goal**: This story is effectively superseded by the semantic mapping strategy where annotations become standard properties (which use commas). However, we must ensure the `parse -> transform -> serialize` pipeline is consistent.

- [x] T008 [US2] Verify round-trip: Input `@k(v) (n)` parses, transforms to `[{k:v} | (n)]`, and serializes to `[{k:v} | (n)]` in `libs/gram/tests/Gram/RoundTripSpec.hs`

## Final Phase: Polish

- [x] T009 Run full test suite to ensure no regressions in `libs/gram/tests/`
- [x] T010 Tighten parser to reject top-level comma separators (strict tree-sitter-gram 0.2.7 compliance)
- [x] T011 Update SemanticsSpec tests to use newline separators instead of commas

## Dependencies

1. T002 & T003 (Foundational) must be completed before T006 & T007 (US1 Implementation).
2. T004 & T005 (Tests) should be written before implementation (TDD).

## Implementation Strategy

We will focus on T004-T007 as the primary MVP. The key insight is that `AnnotatedPattern` in CST is semantically equivalent to a wrapper `SubjectPattern` with properties. This simplifies serialization as we can rely on existing Subject serialization, only needing to ensure `Gram.Root` is flattened.

