---
description: "Task list template for feature implementation"
---

# Tasks: Graph Classifier

**Input**: Design documents from `/specs/034-graph-classifier/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure for the new core classification vocabulary.

- [X] T001 Create `Pattern.Graph.GraphClassifier` module structure in `libs/pattern/src/Pattern/Graph/GraphClassifier.hs`
- [X] T002 [P] Create `GraphClassifierSpec` test module in `libs/pattern/tests/Spec/Pattern/Graph/GraphClassifierSpec.hs`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented.
**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T003 Define `GraphClass extra` and `GraphClassifier extra v` types in `libs/pattern/src/Pattern/Graph/GraphClassifier.hs`
- [X] T004 Implement `classifyByShape` and `canonicalClassifier` in `libs/pattern/src/Pattern/Graph/GraphClassifier.hs`
- [X] T005 Add tests for `classifyByShape` in `libs/pattern/tests/Spec/Pattern/Graph/GraphClassifierSpec.hs` (Must verify valid walks chain correctly and invalid shapes like "star patterns" fall back to `GOther`)

**Checkpoint**: Foundation ready - user story implementation can now begin.

---

## Phase 3: User Story 1 - Standard Graph Construction (Priority: P1) 🎯 MVP

**Goal**: Construct a graph from pattern data using standard built-in rules, so that users don't have to define complex classification logic for common use cases.

**Independent Test**: Can be fully tested by taking a set of standard data patterns, processing them with the default classifier, and verifying that items are correctly categorized into basic graph elements.

### Implementation for User Story 1

- [X] T006 [US1] Update `PatternGraph` data type in `libs/pattern/src/Pattern/PatternGraph.hs` to include `pgOther` map and the `extra` parameter
- [X] T007 [US1] Simplify `GraphValue` typeclass in `libs/pattern/src/Pattern/PatternGraph.hs` to only include `identify` (removing `classify`)
- [X] T008 [US1] Remove `MergeResult` and update `merge` / `fromPatterns` in `libs/pattern/src/Pattern/PatternGraph.hs` to accept a `GraphClassifier` and return `PatternGraph` directly
- [X] T009 [US1] Update tests in `libs/pattern/tests/Spec/Pattern/PatternGraphSpec.hs` to use `canonicalClassifier` and verify standard graph construction without data loss

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently.

---

## Phase 4: User Story 2 - Custom Classification Rules (Priority: P1)

**Goal**: Provide custom rules when viewing or storing a graph, to map patterns to domain-specific structures.

**Independent Test**: Can be fully tested by defining a custom rule set with novel domain categories and verifying that the resulting graph correctly assigns data elements to those custom categories in `pgOther`.

### Implementation for User Story 2

- [X] T010 [US2] Add tests in `libs/pattern/tests/Spec/Pattern/PatternGraphSpec.hs` demonstrating a custom classifier mapping patterns to domain-specific structures in `pgOther` preserving the `extra` tag

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently.

---

## Phase 5: User Story 3 - Legacy Compatibility (Priority: P2)

**Goal**: Existing on-demand graph views continue working without changing application setup, making upgrading seamless.

**Independent Test**: Can be fully tested by running existing workflows with binary rules against the new engine and verifying identical behavior without exposing the multi-category model.

### Implementation for User Story 3

- [X] T011 [US3] Re-derive `GraphLens` in `libs/pattern/src/Pattern/Graph.hs` to be backed by a two-category `GraphClassifier` built from the `testNode` predicate
- [X] T012 [US3] Re-implement all `GraphLens` operations (nodes, relationships, walks, neighbors, etc.) in `libs/pattern/src/Pattern/Graph.hs` to delegate to the underlying classifier
- [X] T013 [US3] Verify `GraphSpec` in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs` passes unchanged, ensuring full backward compatibility

**Checkpoint**: All user stories should now be independently functional.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [X] T014 [P] Fix any compiler warnings or hlint issues across modified modules in `libs/pattern/src/`
- [X] T015 Verify `quickstart.md` examples compile and run successfully

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - Phase 3 (US1) modifies the core `PatternGraph` structure
  - Phase 4 (US2) relies on US1 completion to test `pgOther` custom categories
  - Phase 5 (US3) modifies `GraphLens` and can be done in parallel with Phase 3/4
- **Polish (Final Phase)**: Depends on all desired user stories being complete

### Parallel Opportunities

- T001 and T002 can be done in parallel.
- Once Foundational (Phase 2) is complete, User Story 1 (Phase 3) and User Story 3 (Phase 5) can be worked on in parallel, as they modify different core data structures (`PatternGraph` vs `GraphLens`).

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test canonical `PatternGraph` construction independently

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → MVP!
3. Add User Story 2 → Test custom domain logic independently
4. Add User Story 3 → Verify legacy `GraphLens` backwards compatibility
5. Polish and clean up warnings