# Tasks: PatternGraph Data Structure

**Input**: Design documents from `/specs/033-pattern-graph/`  
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/, quickstart.md  

**Tests**: Included per Constitution (plan.md): unit tests for merge, fromPatterns, classification, conversion to GraphLens; property tests for merge idempotence and consistency.

**Organization**: Tasks are grouped by user story so each story can be implemented and tested independently.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files or independent work)
- **[Story]**: User story label (US1–US4) for story-phase tasks only
- Include exact file paths in descriptions

## Path Conventions

- Library: `libs/pattern/src/Pattern/`, tests: `libs/pattern/tests/Spec/Pattern/`
- Docs: `docs/guide/`, `docs/reference/` at repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Add PatternGraph module and test structure to the pattern library

- [ ] T001 Add `Pattern.PatternGraph` to `exposed-modules` in libs/pattern/pattern.cabal
- [ ] T002 Create libs/pattern/src/Pattern/PatternGraph.hs with module declaration and imports (Pattern.Core, Pattern.Graph, Pattern.Reconcile, Data.Map.Strict)
- [ ] T003 [P] Add Spec.Pattern.PatternGraphSpec and Spec.Pattern.PatternGraphProperties to libs/pattern/tests/Test.hs and create stub files libs/pattern/tests/Spec/Pattern/PatternGraphSpec.hs and libs/pattern/tests/Spec/Pattern/PatternGraphProperties.hs

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and operations that all user stories depend on. No user story work can begin until this phase is complete.

**Independent Test**: Empty graph exists; merge adds a node/relationship and returns MergeResult; fromPatterns builds a graph from a list; unrecognized patterns appear in result list, not in graph.

- [ ] T004 [P] Implement `PatternGraph` data type (pgNodes, pgRelationships, pgWalks, pgAnnotations as Map (Id v) (Pattern v)) and `PatternClass` (Node, Annotation, Relationship, Walk, Unrecognized) in libs/pattern/src/Pattern/PatternGraph.hs
- [ ] T005 [P] Implement `GraphValue` typeclass (type Id v, identify, classify) and `GraphValue Subject` instance with `Id Subject = Symbol` and classification by arity (0=Node, 1=Annotation, 2=Relationship, n=Walk, else Unrecognized) in libs/pattern/src/Pattern/PatternGraph.hs
- [ ] T006 Implement `MergeResult` type and `empty` in libs/pattern/src/Pattern/PatternGraph.hs
- [ ] T007 Implement `merge` (default policy, e.g. LastWriteWins) and `fromPatterns` in libs/pattern/src/Pattern/PatternGraph.hs: classify pattern, dispatch to correct map, reconcile existing identity via Pattern.Reconcile, return (graph, unrecognized); do not store Unrecognized
- [ ] T008 [P] Unit tests for empty, merge (node and relationship), fromPatterns, and classification in libs/pattern/tests/Spec/Pattern/PatternGraphSpec.hs (ensure tests pass)

**Checkpoint**: Foundation ready — merge and fromPatterns work; user story implementation can begin

---

## Phase 3: User Story 1 - Round-trip gram file via graph container (Priority: P1) — MVP

**Goal**: Parse a gram file into the graph container, modify, and serialize back; container keeps nodes and relationships addressable and consistent.

**Independent Test**: Parse a small gram file with nodes and relationships, load with fromPatterns, add one element, serialize back; re-parse and verify equivalent graph.

### Implementation for User Story 1

- [ ] T009 [US1] Add integration test: parse gram (via Gram.Parse), fromPatterns into PatternGraph, serialize (via Gram.Serialize or equivalent), re-parse and assert same logical graph in libs/pattern/tests/Spec/Pattern/PatternGraphSpec.hs or libs/gram/tests
- [ ] T010 [US1] Document round-trip flow (parse → fromPatterns → modify → serialize) and any helper usage in specs/033-pattern-graph/quickstart.md or add minimal example in libs/pattern/src/Pattern/PatternGraph.hs module doc

**Checkpoint**: User Story 1 deliverable — round-trip with gram is testable and documented

---

## Phase 4: User Story 2 - Merge-on-insert semantics (Priority: P2)

**Goal**: Merge with explicit reconciliation policy; duplicate identity produces one stored entry; walks and annotations recursively merge their components.

**Independent Test**: Insert same node twice → one entry (reconciled); insert walk → walk stored and component relationships and nodes merged; insert annotation → annotation stored and inner element merged.

### Implementation for User Story 2

- [ ] T011 [US2] Implement `mergeWithPolicy` and `fromPatternsWithPolicy` taking `ReconciliationPolicy (MergeStrategy v)` in libs/pattern/src/Pattern/PatternGraph.hs
- [ ] T012 [US2] In merge path: when merging a Walk, store walk in pgWalks and recursively merge each component relationship and their endpoint nodes in libs/pattern/src/Pattern/PatternGraph.hs
- [ ] T013 [US2] In merge path: when merging an Annotation, store annotation in pgAnnotations and recursively merge its single inner element into the appropriate category in libs/pattern/src/Pattern/PatternGraph.hs
- [ ] T014 [US2] Unit tests for duplicate identity (reconciled per policy), walk decomposition (components in pgRelationships/pgNodes), and annotation inner merge in libs/pattern/tests/Spec/Pattern/PatternGraphSpec.hs

**Checkpoint**: User Story 2 deliverable — merge-on-insert with policy and recursive decomposition is implemented and tested

---

## Phase 5: User Story 3 - Clear handling of unrecognized patterns (Priority: P3)

**Goal**: Unrecognized pattern shapes are never stored; they are reported in MergeResult so the caller can skip, error, or handle.

**Independent Test**: Feed mix of node/relationship and unrecognized shapes; graph contains only recognized; unrecognized list contains the rest.

### Implementation for User Story 3

- [ ] T015 [US3] Add unit tests: fromPatterns with list containing Unrecognized-shaped patterns; assert unrecognized list non-empty and no Unrecognized stored in pgNodes/pgRelationships/pgWalks/pgAnnotations in libs/pattern/tests/Spec/Pattern/PatternGraphSpec.hs
- [ ] T016 [US3] Document that Unrecognized is never stored and always returned in MergeResult.unrecognized in libs/pattern/src/Pattern/PatternGraph.hs (module or merge/fromPatterns doc)

**Checkpoint**: User Story 3 deliverable — unrecognized handling is tested and documented

---

## Phase 6: User Story 4 - Interoperability with existing graph view (Priority: P3)

**Goal**: Convert PatternGraph to GraphLens so existing graph algorithms (nodes, relationships, walks, traversal) work on the same data.

**Independent Test**: Build PatternGraph from patterns, call toGraphLens, then nodes/relationships/walks from lens match container contents.

### Implementation for User Story 4

- [ ] T017 [US4] Implement `toGraphLens` in libs/pattern/src/Pattern/PatternGraph.hs: build scope pattern from pgNodes, pgRelationships, pgWalks (and optionally pgAnnotations), and atomic-node predicate consistent with GraphValue classification
- [ ] T018 [US4] Unit tests: build PatternGraph with nodes and relationships, toGraphLens, assert nodes lens and relationships lens match container in libs/pattern/tests/Spec/Pattern/PatternGraphSpec.hs

**Checkpoint**: User Story 4 deliverable — conversion to GraphLens is implemented and tested

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Property tests, documentation, and quickstart validation

- [ ] T019 [P] Property tests for merge idempotence and consistency (e.g. merge same pattern twice ≈ merge once; fromPatterns order independence where applicable) in libs/pattern/tests/Spec/Pattern/PatternGraphProperties.hs
- [ ] T020 [P] Add or update PatternGraph usage docs: construction, merge, fromPatterns, round-trip, toGraphLens in docs/guide/pattern-graph-usage.md and docs/reference/features/pattern-graph.md (create if missing)
- [ ] T021 [P] Add .graph.gram notation reference: restrict to annotation, nodes, relationships, and paths only (no square-bracket pattern notation in file); describe resulting PatternGraph structures using pattern notation (flow: gram → parse → PatternGraph → explained with pattern notation) in docs/reference/graph-gram-notation.md or docs/guide/graph-gram-reference.md
- [ ] T022 Run through specs/033-pattern-graph/quickstart.md examples and fix any broken snippets or paths

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies — start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 — blocks all user stories
- **Phase 3 (US1)**: Depends on Phase 2 — round-trip builds on fromPatterns and gram parse/serialize
- **Phase 4 (US2)**: Depends on Phase 2 — mergeWithPolicy and recursive merge build on merge/fromPatterns
- **Phase 5 (US3)**: Depends on Phase 2 — unrecognized handling is part of foundational merge; this phase adds tests and docs
- **Phase 6 (US4)**: Depends on Phase 2 — toGraphLens uses PatternGraph structure
- **Phase 7 (Polish)**: Depends on Phases 3–6 (or at least 2 for property tests and docs)

### User Story Dependencies

- **US1 (P1)**: After Phase 2 only — independently testable (round-trip)
- **US2 (P2)**: After Phase 2 only — independently testable (merge policy and decomposition)
- **US3 (P3)**: After Phase 2 only — independently testable (unrecognized tests and docs)
- **US4 (P3)**: After Phase 2 only — independently testable (toGraphLens)

US2, US3, US4 can be implemented in parallel after Phase 2.

### Within Each User Story

- Implementation tasks in order (types/API first, then tests)
- Story complete before moving to next priority for sequential execution

### Parallel Opportunities

- Phase 1: T003 [P] (stub test files) can run after T001/T002
- Phase 2: T004, T005 [P] can run in parallel; T008 [P] after T007
- Phases 3–6: US1, US2, US3, US4 can be worked in parallel after Phase 2
- Phase 7: T019, T020, T021 [P] can run in parallel

---

## Parallel Example: User Story 1

```text
After Phase 2:
  T009: Integration test for round-trip (parse → fromPatterns → serialize)
  T010: Document round-trip in quickstart / module doc
```

## Parallel Example: User Stories 2–4 (after Phase 2)

```text
US2: T011 → T012 → T013 → T014 (mergeWithPolicy, decomposition, tests)
US3: T015, T016 (tests + doc)
US4: T017 → T018 (toGraphLens + tests)
US2, US3, US4 can be assigned to different implementers in parallel.
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup  
2. Complete Phase 2: Foundational (CRITICAL — blocks all stories)  
3. Complete Phase 3: User Story 1 (round-trip)  
4. **STOP and VALIDATE**: Run round-trip integration test and quickstart  
5. Demo: parse gram → fromPatterns → serialize → re-parse

### Incremental Delivery

1. Phase 1 + 2 → foundation ready  
2. Add US1 → test round-trip → MVP  
3. Add US2 → test merge policy and decomposition  
4. Add US3 → test unrecognized handling  
5. Add US4 → test toGraphLens  
6. Phase 7 → property tests and docs

### Parallel Team Strategy

- One developer: Phase 1 → 2 → 3 → 4 → 5 → 6 → 7 in order  
- Multiple developers: Phase 1+2 together; then split US1 / US2 / US3 / US4; finally Phase 7 together or in parallel

---

## Notes

- [P] tasks = different files or no dependency on other incomplete tasks  
- [USn] label maps task to user story for traceability  
- Each user story is independently testable per spec  
- Commit after each task or logical group (per constitution)  
- All tasks include explicit file paths  
- Format: every task has `- [ ]`, Task ID (T001–T022), optional [P] and [Story], and description with path
