---
description: "Task list for Graph Transform feature implementation"
---

# Tasks: Graph Transform

**Input**: Design documents from `/specs/036-graph-transform/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/api.md

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Phase 1: Setup & Foundational (Shared Infrastructure)

**Purpose**: Project initialization and basic types required before transformation stories begin.
**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [ ] T001 Create structural file `src/Pattern/Graph/Types.hs` and define `Substitution v` type.
- [ ] T002 Create structural file `src/Pattern/Graph/Transform.hs` with module exports.
- [ ] T003 Create structural file `tests/Pattern/Graph/TransformSpec.hs` with hspec boilerplate.
- [ ] T004 Define `GraphView` type and `fromPatternGraph`, `materialize` signatures in `src/Pattern/Graph.hs`.
- [ ] T005 Implement `materialize` in `src/Pattern/Graph.hs`.
- [ ] T006 Implement `fromPatternGraph` GraphView construction in `src/Pattern/Graph.hs`.
- [ ] T007 Implement `fromGraphLens` GraphView construction in `src/Pattern/Graph.hs`.
- [ ] T008 [P] Add unit tests for `GraphView` initialization and `materialize` in `tests/Pattern/GraphSpec.hs`.

**Checkpoint**: Foundation ready - `GraphView` is constructable and materializable.

---

## Phase 2: User Story 1 - Constructing a Graph from External Data (Priority: P1) 🎯 MVP

**Goal**: Turn non-graph external data (like CSV rows or JSON documents) into a full graph without an impedance mismatch using `unfold` and `unfoldGraph`.

**Independent Test**: Can be independently tested by feeding nested, non-graph records into the construction functions and verifying that the resulting structures form a merged, coherent graph.

### Tests for User Story 1

- [ ] T009 [P] [US1] Add property tests for `unfold` in `tests/Pattern/CoreSpec.hs`.
- [ ] T010 [P] [US1] Add unit tests for `unfoldGraph` with mock seed data in `tests/Pattern/Graph/TransformSpec.hs`.

### Implementation for User Story 1

- [ ] T011 [P] [US1] Implement `unfold` anamorphism in `src/Pattern/Core.hs`.
- [ ] T012 [US1] Implement `unfoldGraph` in `src/Pattern/Graph/Transform.hs` using `mergeWithPolicy` and `unfold`.

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. You can now bootstrap graphs.

---

## Phase 3: User Story 2 - Composable Bulk Graph Transformations (Priority: P1)

**Goal**: Transform graph elements (nodes, relationships, walks) efficiently and compose multiple transformations (map, filter, fold) lazily.

**Independent Test**: Can be fully tested by applying successive map and filter operations on a large graph and verifying that changes compose properly and elements are mapped according to their category.

### Tests for User Story 2

- [ ] T013 [P] [US2] Add unit tests for `mapGraph` and `mapAllGraph` in `tests/Pattern/Graph/TransformSpec.hs`.
- [ ] T014 [P] [US2] Add unit tests for `filterGraph` covering Walk gap substitutions in `tests/Pattern/Graph/TransformSpec.hs`.
- [ ] T015 [P] [US2] Add unit tests for `foldGraph` aggregations in `tests/Pattern/Graph/TransformSpec.hs`.

### Implementation for User Story 2

- [ ] T016 [P] [US2] Implement `mapGraph` with `{-# INLINE #-}` in `src/Pattern/Graph/Transform.hs`.
- [ ] T017 [P] [US2] Implement `mapAllGraph` in `src/Pattern/Graph/Transform.hs`.
- [ ] T018 [US2] Implement `filterGraph` applying `Substitution` logic for containers in `src/Pattern/Graph/Transform.hs`.
- [ ] T019 [P] [US2] Implement `foldGraph` using Monoid accumulation in `src/Pattern/Graph/Transform.hs`.

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Bulk transforms are composable.

---

## Phase 4: User Story 3 - Context-Aware Data Enrichment (Priority: P2)

**Goal**: Compute metrics for individual nodes based on their surrounding graph context, without losing deterministic snapshot semantics.

**Independent Test**: Can be fully tested by applying transformations where an element's new value relies on querying its neighbors from the original graph.

### Tests for User Story 3

- [ ] T020 [P] [US3] Add unit tests verifying deterministic snapshot isolation for `mapWithContext` in `tests/Pattern/Graph/TransformSpec.hs`.

### Implementation for User Story 3

- [ ] T021 [US3] Implement `mapWithContext` in `src/Pattern/Graph/Transform.hs` passing the unmodified `viewQuery` to the mapping function.

**Checkpoint**: All transformation and enrichment tasks are functional.

---

## Phase 5: User Story 4 - Iterative Topology-Aware Algorithms (Priority: P3)

**Goal**: Compute iterative algorithms across a graph topology using pattern-centric bottom-up structural folding rounds.

**Independent Test**: Can be independently tested by implementing a basic label propagation algorithm and ensuring it converges accurately over a cyclic graph using structural rounds.

### Tests for User Story 4

- [ ] T022 [P] [US4] Add unit tests for `paraGraph` (single structural round) in `tests/Pattern/Graph/TransformSpec.hs`.
- [ ] T023 [US4] Add complex unit test for `paraGraphFixed` implementing a mock iterative convergence algorithm (e.g. PageRank-lite) in `tests/Pattern/Graph/TransformSpec.hs`.

### Implementation for User Story 4

- [ ] T024 [P] [US4] Implement `paraGraph` as a bottom-up structural fold in `src/Pattern/Graph/Transform.hs`.
- [ ] T025 [US4] Implement `paraGraphFixed` loop relying on the user-supplied convergence predicate (`r -> r -> Bool`) in `src/Pattern/Graph/Transform.hs`.

**Checkpoint**: Iterative algorithmic foundation is established.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T026 [P] Update `Pattern.Graph` module documentation with `GraphView` details.
- [ ] T027 [P] Export all new types and functions in top-level `src/Pattern.hs`.
- [ ] T028 Run compilation check, run all `hspec` suites.
- [ ] T029 Execute `quickstart.md` examples as a final integration sanity check.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup/Foundational)**: No dependencies - blocks all stories.
- **User Stories (Phases 2-5)**: All depend on Phase 1.
  - Phase 2 (US1) and Phase 3 (US2) can be executed in parallel.
  - Phase 4 (US3) depends conceptually on understanding the `GraphView` transformation loop established in Phase 3.
  - Phase 5 (US4) depends on Phase 1, but is highly isolated structurally.
- **Polish (Phase 6)**: Depends on all user stories.

### Parallel Opportunities

- Tests (T009, T010, T013, T014, T015, T020, T022) can all be written in parallel per story.
- Implementations like `mapAllGraph` (T017) and `foldGraph` (T019) are pure and map strictly over lists; they can be developed concurrently by separate developers without merge conflicts.