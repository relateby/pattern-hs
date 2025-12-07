# Tasks: Graph Lens Implementation

**Input**: Design documents from `/specs/023-graph-lens/`
**Feature**: 023-graph-lens
**Status**: Ready for Implementation

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4, US5)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Project Preparation)

**Purpose**: Prepare project structure and verify dependencies

- [X] T001 Review existing Pattern library structure in `libs/pattern/`
- [X] T002 [P] Verify Pattern.Core module provides required Pattern type and functions
- [X] T003 [P] Verify test infrastructure (hspec, QuickCheck) is configured in `libs/pattern/pattern.cabal`
- [X] T004 [P] Review design document at `design/graph-lens.md` for implementation details
- [X] T005 [P] Review analysis recommendations at `specs/022-graph-lens-review/analysis.md`

**Checkpoint**: Project structure understood, dependencies confirmed, design documents reviewed

---

## Phase 2: Foundational (Core Data Structure)

**Purpose**: Implement core GraphLens data structure - required before all user stories

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T006 Implement GraphLens data structure in `libs/pattern/src/Pattern/Graph.hs` with scopePattern and isNode fields
- [X] T007 Add Haddock documentation for GraphLens data structure including categorical interpretation in `libs/pattern/src/Pattern/Graph.hs`
- [X] T008 Add module header documentation explaining Graph Lens concept in `libs/pattern/src/Pattern/Graph.hs`
- [X] T009 [P] Write unit test for GraphLens construction in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [X] T010 [P] Write edge case test for empty scopePattern in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [X] T011 Run tests with timeout: `timeout 60 cabal test` to verify foundational tests pass
- [ ] T012 Git commit: "feat: add GraphLens data structure - foundational"

**Checkpoint**: GraphLens data structure complete and tested - user story implementation can now begin

---

## Phase 3: User Story 1 - Create Graph Lens and Identify Nodes (Priority: P1) 🎯 MVP

**Goal**: Enable developers to create a GraphLens from a Pattern and identify which elements are nodes according to a predicate, enabling basic graph interpretation of Pattern structures.

**Independent Test**: Can be fully tested by creating a GraphLens with a simple predicate (e.g., atomic patterns as nodes), applying it to a Pattern, and verifying that nodes are correctly identified.

### Tests for User Story 1

- [X] T013 [P] [US1] Write unit test for `nodes` function with atomic predicate in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [X] T014 [P] [US1] Write unit test for `nodes` function with value-based predicate in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T015 [P] [US1] Write unit test for `isNode` function (context-aware) in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [X] T016 [P] [US1] Write edge case test for empty Pattern scope in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [X] T017 [P] [US1] Write edge case test for Pattern with no nodes (all fail predicate) in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T018 [P] [US1] Write property-based test for nodes function correctness in `libs/pattern/tests/Spec/Pattern/Properties.hs`

### Implementation for User Story 1

- [X] T019 [US1] Implement `nodes` function in `libs/pattern/src/Pattern/Graph.hs` (filters scopePattern elements by isNode predicate)
- [X] T020 [US1] Implement `isNode` function (context-aware version) in `libs/pattern/src/Pattern/Graph.hs` (handle field accessor conflict)
- [X] T021 [US1] Add Haddock documentation for `nodes` function in `libs/pattern/src/Pattern/Graph.hs`
- [X] T022 [US1] Add Haddock documentation for `isNode` function in `libs/pattern/src/Pattern/Graph.hs`
- [X] T023 [US1] Update module export list in `libs/pattern/src/Pattern/Graph.hs` to export nodes and isNode
- [X] T024 [US1] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 1 tests pass
- [ ] T025 [US1] Git commit: "feat: implement node identification - US1"

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Developers can create GraphLens and identify nodes.

---

## Phase 4: User Story 2 - Identify and Query Relationships (Priority: P1)

**Goal**: Enable developers to identify relationships (non-node patterns connecting exactly two nodes) and query them from a GraphLens, enabling graph structure analysis.

**Independent Test**: Can be fully tested by creating a GraphLens with nodes identified, providing Patterns that connect two nodes, and verifying that relationships are correctly identified and queried.

### Tests for User Story 2

- [ ] T026 [P] [US2] Write unit test for `isRelationship` function with valid relationship in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T027 [P] [US2] Write unit test for `isRelationship` function with pattern having more than 2 elements in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T028 [P] [US2] Write unit test for `isRelationship` function with pattern having non-node elements in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T029 [P] [US2] Write unit test for `relationships` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T030 [P] [US2] Write unit test for `source` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T031 [P] [US2] Write unit test for `target` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T032 [P] [US2] Write unit test for `reverseRel` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T033 [P] [US2] Write edge case test for empty relationships in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T034 [P] [US2] Write edge case test for source/target on non-relationship pattern in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T035 [P] [US2] Write property-based test for relationship structure laws in `libs/pattern/tests/Spec/Pattern/Properties.hs`

### Implementation for User Story 2

- [ ] T036 [US2] Implement `isRelationship` function in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T037 [US2] Implement `relationships` function in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T038 [US2] Implement `source` function in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T039 [US2] Implement `target` function in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T040 [US2] Implement `reverseRel` function in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T041 [US2] Add Haddock documentation for all relationship functions in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T042 [US2] Update module export list in `libs/pattern/src/Pattern/Graph.hs` to export relationship functions
- [ ] T043 [US2] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 2 tests pass
- [ ] T044 [US2] Git commit: "feat: implement relationship identification and querying - US2"

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Developers can identify nodes and relationships.

---

## Phase 5: User Story 3 - Navigate Graph Structure (Priority: P2)

**Goal**: Enable developers to navigate graph structure by finding neighbors of nodes, incident relationships, and computing node degrees, enabling graph exploration.

**Independent Test**: Can be fully tested by creating a GraphLens with nodes and relationships, selecting a node, and verifying that neighbors, incident relationships, and degree are correctly computed.

### Tests for User Story 3

- [ ] T045 [P] [US3] Write unit test for `neighbors` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T046 [P] [US3] Write unit test for `incidentRels` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T047 [P] [US3] Write unit test for `degree` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T048 [P] [US3] Write edge case test for isolated node (no relationships) in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T049 [P] [US3] Write edge case test for node with multiple relationships in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T050 [P] [US3] Write edge case test for node not in lens scope in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T051 [P] [US3] Write property-based test for navigation correctness in `libs/pattern/tests/Spec/Pattern/Properties.hs`

### Implementation for User Story 3

- [ ] T052 [US3] Implement `neighbors` function in `libs/pattern/src/Pattern/Graph.hs` (requires Eq v constraint)
- [ ] T053 [US3] Implement `incidentRels` function in `libs/pattern/src/Pattern/Graph.hs` (requires Eq v constraint)
- [ ] T054 [US3] Implement `degree` function in `libs/pattern/src/Pattern/Graph.hs` (requires Eq v constraint)
- [ ] T055 [US3] Add Haddock documentation for navigation functions in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T056 [US3] Update module export list in `libs/pattern/src/Pattern/Graph.hs` to export navigation functions
- [ ] T057 [US3] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 3 tests pass
- [ ] T058 [US3] Git commit: "feat: implement graph navigation - US3"

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently. Developers can navigate graph structure.

---

## Phase 6: User Story 4 - Identify and Analyze Walks (Priority: P2)

**Goal**: Enable developers to identify walks (sequences of consecutively connected relationships) and extract nodes from walks, enabling path analysis in graphs.

**Independent Test**: Can be fully tested by creating a GraphLens with relationships, providing a Pattern containing consecutively connected relationships, and verifying that walks are identified and nodes can be extracted.

### Tests for User Story 4

- [ ] T059 [P] [US4] Write unit test for `isWalk` function with valid walk in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T060 [P] [US4] Write unit test for `isWalk` function with disconnected relationships in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T061 [P] [US4] Write unit test for `isWalk` function with non-relationship elements in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T062 [P] [US4] Write unit test for `walks` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T063 [P] [US4] Write unit test for `walkNodes` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T064 [P] [US4] Write edge case test for empty walk pattern in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T065 [P] [US4] Write property-based test for walk validity laws in `libs/pattern/tests/Spec/Pattern/Properties.hs`

### Implementation for User Story 4

- [ ] T066 [US4] Implement `consecutivelyConnected` helper function in `libs/pattern/src/Pattern/Graph.hs` (requires Eq v constraint)
- [ ] T067 [US4] Implement `isWalk` function in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T068 [US4] Implement `walks` function in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T069 [US4] Implement `walkNodes` function in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T070 [US4] Add Haddock documentation for walk functions in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T071 [US4] Update module export list in `libs/pattern/src/Pattern/Graph.hs` to export walk functions
- [ ] T072 [US4] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 4 tests pass
- [ ] T073 [US4] Git commit: "feat: implement walk identification and analysis - US4"

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should all work independently. Developers can identify and analyze walks.

---

## Phase 7: User Story 5 - Analyze Graph Connectivity (Priority: P3)

**Goal**: Enable developers to analyze graph connectivity by finding connected components, performing breadth-first search, and finding paths between nodes, enabling comprehensive graph analysis.

**Independent Test**: Can be fully tested by creating a GraphLens with a graph containing multiple connected components, and verifying that connected components are correctly identified, BFS works correctly, and paths can be found between connected nodes.

### Tests for User Story 5

- [ ] T074 [P] [US5] Write unit test for `connectedComponents` function with multiple components in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T075 [P] [US5] Write unit test for `connectedComponents` function with single isolated node in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T076 [P] [US5] Write unit test for `bfs` function in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T077 [P] [US5] Write unit test for `findPath` function with connected nodes in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T078 [P] [US5] Write unit test for `findPath` function with disconnected nodes in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T079 [P] [US5] Write edge case test for `findPath` with same start and end node in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T080 [P] [US5] Write property-based test for connectivity correctness in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- [ ] T081 [P] [US5] Write performance test for connectedComponents (500+ nodes, 1000+ relationships) in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T082 [P] [US5] Write performance test for findPath (200+ nodes) in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`

### Implementation for User Story 5

- [ ] T083 [US5] Implement `connectedComponents` function in `libs/pattern/src/Pattern/Graph.hs` (requires Eq v constraint, use Set/Map from containers)
- [ ] T084 [US5] Implement `bfs` function in `libs/pattern/src/Pattern/Graph.hs` (requires Eq v constraint, use Set for visited tracking)
- [ ] T085 [US5] Implement `findPath` function in `libs/pattern/src/Pattern/Graph.hs` (requires Eq v constraint, use Set/Map for path finding)
- [ ] T086 [US5] Add Haddock documentation for connectivity analysis functions in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T087 [US5] Update module export list in `libs/pattern/src/Pattern/Graph.hs` to export connectivity functions
- [ ] T088 [US5] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 5 tests pass
- [ ] T089 [US5] Git commit: "feat: implement graph connectivity analysis - US5"

**Checkpoint**: All user stories should now be independently functional. Developers can perform comprehensive graph analysis.

---

## Phase 8: Integration & Polish

**Purpose**: Finalize implementation, update exports, add documentation, and verify completeness

- [ ] T090 Update `libs/pattern/src/Pattern.hs` to re-export Graph Lens functionality from Pattern.Graph
- [ ] T091 [P] Add comprehensive module documentation to `libs/pattern/src/Pattern/Graph.hs` with usage examples
- [ ] T092 [P] Verify all Haddock documentation is complete and accurate in `libs/pattern/src/Pattern/Graph.hs`
- [ ] T093 [P] Add examples to `libs/pattern/examples/examples.md` demonstrating Graph Lens usage
- [ ] T094 [P] Write integration test for multiple lenses on same Pattern in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T095 [P] Write integration test verifying scope-bounded operations in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T096 [P] Write property-based test for graph structure laws (relationship connectivity, walk validity, path reachability) in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- [ ] T097 [P] Write property-based test for component completeness in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- [ ] T098 Verify all edge cases from spec are tested in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- [ ] T099 Run quickstart.md validation - verify examples work correctly
- [ ] T100 Run full test suite with timeout: `timeout 60 cabal test` to verify all tests pass
- [ ] T101 Verify performance targets from success criteria (SC-001 through SC-006)
- [ ] T102 Code review: verify all functions follow scope-bounded principle
- [ ] T103 Code review: verify all predicates are pure functions (no side effects)
- [ ] T104 Code review: verify mathematical correctness of graph operations
- [ ] T105 Git commit: "docs: finalize Graph Lens implementation"

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - User Story 1 (P1): Can start immediately after Foundational
  - User Story 2 (P1): Can start immediately after Foundational (uses US1 concepts but independently testable)
  - User Story 3 (P2): Depends on US2 (uses relationships for navigation)
  - User Story 4 (P2): Depends on US2 (uses relationships for walks)
  - User Story 5 (P3): Depends on US3 (uses navigation for connectivity analysis)
- **Polish (Phase 8)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - Uses US1 concepts (nodes) but independently testable
- **User Story 3 (P2)**: Depends on US2 completion - Uses relationships for navigation
- **User Story 4 (P2)**: Depends on US2 completion - Uses relationships for walks
- **User Story 5 (P3)**: Depends on US3 completion - Uses navigation functions for connectivity

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Core functions before helper functions
- Implementation before documentation
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All test tasks within a user story marked [P] can run in parallel
- Different user stories can be worked on in parallel by different team members (after dependencies are met)
- Polish tasks marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Write unit test for nodes function with atomic predicate"
Task: "Write unit test for nodes function with value-based predicate"
Task: "Write unit test for isNode function (context-aware)"
Task: "Write edge case test for empty Pattern scope"
Task: "Write edge case test for Pattern with no nodes"
Task: "Write property-based test for nodes function correctness"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test User Story 1 independently
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Deploy/Demo
4. Add User Story 3 → Test independently → Deploy/Demo
5. Add User Story 4 → Test independently → Deploy/Demo
6. Add User Story 5 → Test independently → Deploy/Demo
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1
   - Developer B: User Story 2 (can start after Foundational, uses US1 concepts but independently testable)
3. After US1 and US2 complete:
   - Developer A: User Story 3
   - Developer B: User Story 4
4. After US3 completes:
   - Developer A: User Story 5

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- **Git commit after each user story completion** (see tasks T025, T044, T058, T073, T089)
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- All functions must be scope-bounded (only direct elements of scopePattern)
- All predicates must be pure functions (no side effects)
- Edge cases must be handled gracefully (empty results, False/Nothing returns)

## Testing Performance Guidelines

### Test Execution Timeouts

**CRITICAL**: Always use timeouts when running tests to prevent hanging:

- **First test run after implementation**: Use `timeout 60` (60 seconds) to catch any infinite loops or performance issues
- **Subsequent test runs**: Use `timeout 30` (30 seconds) for normal verification
- **Full test suite**: Should complete in under 1 minute total

### Test Performance Requirements

- **Unit tests**: Each test should complete in <100ms
- **Property-based tests**: Each property test should complete in <10ms (use bounded generators)
- **Full test suite**: Should complete in <1 minute total
- **Individual test phases**: Should complete in <10 seconds

### Test Execution Commands

```bash
# First run after implementation (with timeout):
timeout 60 cabal test

# Normal verification (with timeout):
timeout 30 cabal test
```

---

## Task Summary

**Total Tasks**: 105
**Completed Tasks**: 0
**Status**: Ready for Implementation

### Tasks by Phase

- **Setup (Phase 1)**: 5 tasks
- **Foundational (Phase 2)**: 7 tasks
- **User Story 1 (P1)**: 13 tasks (6 tests + 7 implementation)
- **User Story 2 (P1)**: 19 tasks (10 tests + 9 implementation)
- **User Story 3 (P2)**: 14 tasks (7 tests + 7 implementation)
- **User Story 4 (P2)**: 15 tasks (7 tests + 8 implementation)
- **User Story 5 (P3)**: 16 tasks (9 tests + 7 implementation)
- **Integration & Polish (Phase 8)**: 16 tasks

### Key Deliverables

- ✅ Complete Graph Lens implementation in `libs/pattern/src/Pattern/Graph.hs`
- ✅ Comprehensive test suite in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`
- ✅ Property-based tests in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- ✅ Updated exports in `libs/pattern/src/Pattern.hs`
- ✅ Documentation and examples

### Suggested MVP Scope

**MVP**: Phases 1-3 (Setup + Foundational + User Story 1)
- Enables basic graph interpretation (create lens, identify nodes)
- Provides complete, independently testable functionality
- Delivers immediate value to users
