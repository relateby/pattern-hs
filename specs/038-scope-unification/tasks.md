# Tasks: Scope Unification for Structure-Aware Operations

**Input**: Design documents from `specs/038-scope-unification/`  
**Branch**: `038-scope-unification`  
**Prerequisites**: `plan.md` ✓, `spec.md` ✓, `research.md` ✓, `data-model.md` ✓, `contracts/` ✓, `quickstart.md` ✓

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this belongs to (`US1`, `US2`, `US3`)
- Every task includes exact file paths

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Confirm the current export surface, test touchpoints, and implementation files before changing the public fold APIs.

- [X] T001 Audit current exports and re-export touchpoints in `libs/pattern/src/Pattern/Core.hs`, `libs/pattern/src/Pattern/Graph/Transform.hs`, `libs/pattern/src/Pattern/Graph.hs`, `libs/pattern/src/Pattern.hs`, and `libs/pattern/pattern.cabal`
- [X] T002 Audit existing fold behavior and insertion points in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` and `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` so new scope-unification tests extend the current suites rather than replace them

**Checkpoint**: Export and test touchpoints are confirmed. Foundational implementation can begin safely.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Add the shared scope primitives and graph adapter scaffolding that all user stories depend on.

**⚠️ CRITICAL**: No user story work should begin until this phase is complete.

- [X] T003 Add subtree helper functions and public API declarations for `ScopeQuery`, `TrivialScope`, `trivialScope`, and `paraWithScope` in `libs/pattern/src/Pattern/Core.hs`
- [X] T004 Implement `ScopeQuery TrivialScope`, `paraWithScope`, and the `para` wrapper in `libs/pattern/src/Pattern/Core.hs` so tree folds preserve bottom-up child-result order
- [X] T005 Add an internal `GraphView`-backed scope adapter scaffold and private indexing/helpers in `libs/pattern/src/Pattern/Graph/Transform.hs` without changing the public `GraphQuery(..)` record
- [X] T006 Align export lists for the new public generic scope APIs in `libs/pattern/src/Pattern/Core.hs` and `libs/pattern/src/Pattern.hs` while keeping the graph adapter internal

**Checkpoint**: The shared scope primitive exists, `para` is backed by it, and graph-side adapter scaffolding is ready for story work.

---

## Phase 3: User Story 1 - Preserve existing fold behavior through one unified scope model (Priority: P1) 🎯 MVP

**Goal**: Keep `para` and `paraGraph` behavior unchanged while introducing the shared scope model and wrapper-based implementation.

**Independent Test**: Run the existing `para` and `paraGraph` suites without changing call sites or expectations; confirm the same observable results are produced.

### Tests for User Story 1

- [X] T007 [US1] Add `TrivialScope` edge-case tests to `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` asserting `containers` and `siblings` return `[]` and never fail for subtree-only scope
- [X] T008 [US1] Add `paraWithScope` compatibility tests to `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` covering bottom-up child-result order and representative equivalence between `paraWithScope (trivialScope p)` and `para`
- [X] T009 [US1] Add QuickCheck property tests to `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` proving `paraWithScope (trivialScope p)` is observationally equivalent to `para` for representative generated `Pattern` values
- [X] T010 [P] [US1] Add wrapper-preservation tests to `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` covering unchanged `paraGraph` and `paraGraphFixed` outputs under the unified scope implementation

### Implementation for User Story 1

- [X] T011 [US1] Complete the internal graph adapter integration and update `paraGraph`, `paraGraphWithSeed`, and `paraGraphFixed` in `libs/pattern/src/Pattern/Graph/Transform.hs` so they consume shared scope information while preserving `Map (Id v) r` output, `topoShapeSort`, and cycle soft-failure behavior
- [X] T012 [US1] Run `cabal test pattern-test` from the repo root to validate `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` and `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` for unchanged `para` and `paraGraph` behavior

**Checkpoint**: `para` and `paraGraph` still work unchanged, and the shared scope model is now the internal foundation.

---

## Phase 4: User Story 2 - Add new scope-aware behavior without inventing new fold primitives (Priority: P2)

**Goal**: Make the shared scope contract usable for new scope providers, including graph-wide generic scope answers via the internal `GraphView`-backed adapter.

**Independent Test**: Define a non-graph scope provider that answers the generic scope questions, run `paraWithScope` against it, and confirm no new specialized fold primitive is needed.

### Tests for User Story 2

- [X] T013 [P] [US2] Add custom-provider extensibility tests to `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` by defining a local `ScopeQuery` instance and proving `paraWithScope` works without introducing another fold API
- [X] T014 [P] [US2] Add graph-boundary tests to `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` covering `allElements`, `byIdentity`, `containers`, and `siblings` behavior for the internal `GraphView`-backed adapter inside a single `GraphView`

### Implementation for User Story 2

- [X] T015 [US2] Implement the full internal `GraphView`-backed `ScopeQuery` adapter behavior in `libs/pattern/src/Pattern/Graph/Transform.hs`, including fixed-scope `allElements`, scope-bounded identity lookup, direct-container lookup, and sibling derivation across classified graph elements
- [X] T016 [US2] Add Haddock guidance for implementing new scope providers and fixed-scope fold semantics in `libs/pattern/src/Pattern/Core.hs` and `libs/pattern/src/Pattern/Graph/Transform.hs`
- [X] T017 [US2] Run `cabal test pattern-test` from the repo root to validate the extensibility and graph-boundary scenarios in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` and `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs`

**Checkpoint**: New scope providers can participate in the same fold model, and graph-wide generic scope answers are available without widening `GraphQuery(..)`.

---

## Phase 5: User Story 3 - Pass scope behavior around as data when needed (Priority: P3)

**Goal**: Provide a first-class scope value that behaves the same as the original scope provider for generic scope operations.

**Independent Test**: Reify a scope into a first-class value, pass it to a helper, and verify containment, sibling lookup, identity lookup, and enumeration match the original provider.

### Tests for User Story 3

- [X] T018 [US3] Add `ScopeDict` observational-equivalence tests to `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` covering containment, siblings, identity lookup, and all-elements enumeration for subtree scope
- [X] T019 [US3] Add QuickCheck property tests to `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` proving `toScopeDict` preserves `containers`, `siblings`, `byIdentity`, and `allElements` for representative subtree scope providers
- [X] T020 [P] [US3] Add graph-backed scope reification tests to `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` proving a first-class scope value derived from the internal `GraphView`-backed adapter matches the direct adapter's generic scope answers
- [X] T021 [US3] Add higher-order helper tests to `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` proving a reified scope value from `toScopeDict` can be passed as data without changing observable behavior

### Implementation for User Story 3

- [X] T022 [US3] Implement `ScopeDict`, the `ScopeQuery ScopeDict` instance, and `toScopeDict` in `libs/pattern/src/Pattern/Core.hs`
- [X] T023 [US3] Run `cabal test pattern-test` from the repo root to validate the `ScopeDict` scenarios in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` and `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs`

**Checkpoint**: Scope behavior can be stored and passed around as data without changing generic scope semantics.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Refresh reference documentation, complete source-level mathematical docs, and validate the full feature end to end.

- [X] T024 [P] Update `docs/reference/features/paramorphism.md` with the unified scope model, `paraWithScope`, and the relationship between `para` and `TrivialScope`
- [X] T025 [P] Update `docs/reference/features/para-graph.md` with the internal graph adapter mental model and the preserved `paraGraph` scheduling and cycle semantics
- [X] T026 [P] Update `docs/reference/PORTING-GUIDE.md` with the typeclass ↔ dictionary/value-form ↔ trait/protocol mapping for scope abstractions
- [X] T027 Update module-level and per-symbol documentation in `libs/pattern/src/Pattern/Core.hs` and `libs/pattern/src/Pattern/Graph/Transform.hs` so every new public type and function includes purpose, invariants, categorical meaning, and at least one usage example
- [X] T028 Validate `specs/038-scope-unification/quickstart.md` against the implemented files and update any mismatched paths or verification steps in `specs/038-scope-unification/quickstart.md`
- [X] T029 Validate by code review that no extra traversal or scheduling regression was introduced in `libs/pattern/src/Pattern/Core.hs` and `libs/pattern/src/Pattern/Graph/Transform.hs` against the preserved-asymptotics and single-pass constraints in `specs/038-scope-unification/plan.md`
- [X] T030 Review `libs/pattern/src/Pattern/Core.hs`, `libs/pattern/src/Pattern/Graph/Transform.hs`, and related docs against `specs/038-scope-unification/spec.md` to confirm no broader cleanup work was introduced beyond this feature's agreed scope
- [X] T031 Run `cabal build all` and `cabal test pattern-test` from the repo root to verify the full implementation and all scenarios covered by `libs/pattern/tests/Spec/Pattern/CoreSpec.hs` and `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies; start immediately.
- **Phase 2 (Foundational)**: Depends on Phase 1 and blocks all user story phases.
- **Phase 3 (US1)**: Depends on Phase 2.
- **Phase 4 (US2)**: Depends on Phase 2; it is independently testable once the shared scope primitives exist.
- **Phase 5 (US3)**: Depends on Phase 2; it is independently testable once the shared scope contract exists.
- **Phase 6 (Polish)**: Depends on all selected user stories being complete.

### User Story Dependencies

- **US1 (P1)**: First deliverable and recommended MVP.
- **US2 (P2)**: Can start after Phase 2 and remains independently testable.
- **US3 (P3)**: Can start after Phase 2 and remains independently testable.

### Within Each User Story

- Tests should be added before or alongside implementation and must fail meaningfully before the behavior is finalized.
- Source implementation should precede the story validation command.
- Each story is complete only after its independent test criteria pass.

### Parallel Opportunities

- T010 can run in parallel with T007-T009 after Phase 2.
- T013 and T014 can run in parallel after Phase 2.
- T020 can run in parallel with T018-T019 after Phase 2.
- T024, T025, and T026 can run in parallel during the polish phase.

---

## Parallel Execution Examples

### Parallel Example: User Story 1

```text
T010 in libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs
while T007-T009 progress in libs/pattern/tests/Spec/Pattern/CoreSpec.hs
```

### Parallel Example: User Story 2

```text
T013 in libs/pattern/tests/Spec/Pattern/CoreSpec.hs
T014 in libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs
```

### Parallel Example: User Story 3

```text
T020 in libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs
while T018-T019 progress in libs/pattern/tests/Spec/Pattern/CoreSpec.hs
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1.
2. Complete Phase 2.
3. Complete Phase 3.
4. Stop and validate `para` and `paraGraph` compatibility before taking on extensibility or first-class dictionary support.

### Incremental Delivery

1. Setup + Foundational phases establish the shared scope primitive.
2. User Story 1 delivers the highest-value behavior-preservation result.
3. User Story 2 adds extensibility for new scope providers.
4. User Story 3 adds the first-class value form for dynamic composition.
5. Polish refreshes docs and validates the whole feature end to end.

### Suggested MVP Scope

- **MVP**: Phase 1 + Phase 2 + Phase 3 (`US1`)
- **Why**: This delivers the core outcome of the feature: one unified scope model with zero regression for existing `para` and `paraGraph` callers.

---

## Notes

- All task lines follow the required checklist format: checkbox, task ID, optional `[P]`, required story label for story tasks, and exact file paths.
- The task list intentionally keeps `GraphQuery(..)` unchanged and routes generic graph scope through an internal `GraphView`-backed adapter, matching the planning decision in `research.md`.
- QuickCheck property-test tasks are explicit because the constitution and plan require property-based verification where applicable.
- The main executable validation commands are `cabal test pattern-test` and `cabal build all` from the repo root.
