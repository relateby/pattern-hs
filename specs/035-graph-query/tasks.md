# Tasks: GraphQuery — Portable, Composable Graph Query Interface

**Input**: Design documents from `/specs/035-graph-query/`  
**Branch**: `035-graph-query`  
**Prerequisites**: plan.md ✓, spec.md ✓, research.md ✓, data-model.md ✓, contracts/ ✓, quickstart.md ✓

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1–US5)
- Paths are relative to `libs/pattern/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Register new modules in cabal and create empty file stubs so all phases can compile incrementally.

- [X] T001 Add `Pattern.Graph.GraphQuery` and `Pattern.Graph.Algorithms` to `exposed-modules` in `libs/pattern/pattern.cabal`
- [X] T002 Add `Spec.Pattern.Graph.GraphQuerySpec` and `Spec.Pattern.Graph.AlgorithmsSpec` to `other-modules` in the `pattern-test` stanza of `libs/pattern/pattern.cabal`
- [X] T003 [P] Create empty module stub `libs/pattern/src/Pattern/Graph/GraphQuery.hs` with module declaration and exports list (no implementations yet)
- [X] T004 [P] Create empty module stub `libs/pattern/src/Pattern/Graph/Algorithms.hs` with module declaration and exports list (no implementations yet)
- [X] T005 [P] Create empty test file `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs` with module declaration
- [X] T006 [P] Create empty test file `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs` with module declaration
- [X] T007 Verify the project builds with stubs: run `cabal build pattern` from repo root

**Checkpoint**: Project builds with new empty modules — ready for foundational work.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and constructors that every user story depends on. MUST be complete before any user story phase begins.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [X] T008 Implement `TraversalDirection` data type (`Forward | Backward`) with `Eq`, `Show` instances in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`
- [X] T009 Implement `TraversalWeight v` type alias and the three canonical values (`undirected`, `directed`, `directedReverse`) in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`
- [X] T010 Implement `GraphQuery v` record type with all nine fields and full Haddock documentation (categorical interpretation, field semantics, invariants) in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`
- [X] T011 Implement `fromGraphLens :: (GraphValue v, Eq v) => GraphLens v -> GraphQuery v` in `libs/pattern/src/Pattern/Graph/GraphQuery.hs` — derive all fields from existing `Pattern.Graph` functions; `queryNodeById`/`queryRelationshipById` use O(n) scans; `queryContainers` scans relationships and walks
- [X] T012 Implement `fromPatternGraph :: (GraphValue v, Eq v) => PatternGraph extra v -> GraphQuery v` in `libs/pattern/src/Pattern/Graph/GraphQuery.hs` — read directly from `pgNodes`, `pgRelationships`, `pgWalks`, `pgAnnotations` maps; O(log n) lookups for `queryNodeById`/`queryRelationshipById`
- [X] T013 Export `fromPatternGraph` from `libs/pattern/src/Pattern/PatternGraph.hs` — NOTE: circular import prevents re-export; `fromPatternGraph` is available directly from `Pattern.Graph.GraphQuery`
- [X] T014 Remove `toGraphLens` and `toGraphLensWithScope` from `libs/pattern/src/Pattern/PatternGraph.hs`; migration path is `fromPatternGraph` (from `Pattern.PatternGraph` or `Pattern.Graph.GraphQuery`). *Implementation chose removal over deprecation; see research.md Decision 7 "Implementation deviation".*
- [X] T015 Write unit tests for `GraphQuery` construction in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: verify all nine fields return correct values for a known `PatternGraph` and a known `GraphLens`
- [X] T016 Write property test in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: `fromGraphLens` and `fromPatternGraph` on equivalent graphs produce the same `queryNodes`, `queryRelationships`, `querySource`, `queryTarget` results
- [X] T017 Write unit tests for canonical `TraversalWeight` values in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: verify `undirected` returns 1.0 for both directions; `directed` returns 1.0 forward and infinity backward; `directedReverse` is the inverse
- [X] T018 Verify the project builds and foundational tests pass: `cabal test pattern-test`

**Checkpoint**: Foundation ready — `GraphQuery`, `TraversalWeight`, `fromGraphLens`, `fromPatternGraph` all tested. User story phases can now begin.

---

## Phase 3: User Story 1 — Run Graph Algorithms on Any Representation (Priority: P1) 🎯 MVP

**Goal**: All graph algorithms work against `GraphQuery v` derived from either `GraphLens` or `PatternGraph`, without any intermediate conversion.

**Independent Test**: Construct a `GraphQuery` from a `PatternGraph`, call `shortestPath` and `connectedComponents`, verify correct results — without ever touching `GraphLens`.

- [X] T019 [US1] Implement internal neighbor-derivation helper in `libs/pattern/src/Pattern/Graph/Algorithms.hs`: given a `GraphQuery v`, a `TraversalWeight v`, and a node, return all reachable neighbor nodes (filters infinite-cost edges)
- [X] T020 [US1] Implement `bfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T021 [US1] Implement `dfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T022 [US1] Implement `shortestPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Maybe [Pattern v]` (Dijkstra) in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T023 [US1] Implement `hasPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Bool` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T024 [US1] Implement `allPaths :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> [[Pattern v]]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T025 [US1] Implement `isNeighbor :: Eq (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Bool` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T026 [US1] Implement `isConnected :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Bool` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T027 [US1] Implement `connectedComponents :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [[Pattern v]]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T028 [US1] Implement `topologicalSort :: Ord (Id v) => GraphQuery v -> Maybe [Pattern v]` (DFS-based; returns `Nothing` on cycle) in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T029 [US1] Implement `hasCycle :: Ord (Id v) => GraphQuery v -> Bool` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T030 [US1] Implement `minimumSpanningTree :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [Pattern v]` (Kruskal's or Prim's) in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T031 [US1] Implement `degreeCentrality :: Ord (Id v) => GraphQuery v -> Map (Id v) Double` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T032 [US1] Implement `betweennessCentrality :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Map (Id v) Double` (Brandes algorithm) in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T033 [US1] Write unit tests for all traversal and path algorithms in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: test `bfs`, `dfs`, `shortestPath`, `hasPath`, `allPaths` on a known graph derived from `PatternGraph` (not `GraphLens`)
- [X] T034 [US1] Write unit tests for structural algorithms in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: `connectedComponents`, `topologicalSort` (DAG and cyclic), `hasCycle`, `minimumSpanningTree`
- [X] T035 [US1] Write unit tests for centrality algorithms in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: `degreeCentrality` and `betweennessCentrality` on a small known graph
- [X] T036 [US1] Write property test: `fromGraphLens` and `fromPatternGraph` on equivalent graphs produce identical `connectedComponents` results in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`
- [X] T037 [US1] Write edge case tests in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: `shortestPath` returns `Nothing` when no path exists; `topologicalSort` returns `Nothing` on a cycle; `allPaths` on empty graph returns `[]`
- [X] T038 [US1] Add `{-# INLINE #-}` pragma to `reachableNeighbors` hot-path helper in `libs/pattern/src/Pattern/Graph/Algorithms.hs`; documented in `GraphQuery.hs` why `{-# UNPACK #-}` does not apply (all fields are function/boxed types)
- [X] T039 [US1] Run full test suite and verify all US1 tests pass: `cabal test pattern-test`

**Checkpoint**: All graph algorithms work on `GraphQuery` from either source. US1 independently verified.

---

## Phase 4: User Story 2 — Traversal Direction and Weight at the Call Site (Priority: P2)

**Goal**: The same `GraphQuery` produces different traversal results when called with `directed` vs `undirected` `TraversalWeight`, and custom weight functions work correctly.

**Independent Test**: Call `hasPath` on the same `GraphQuery` with `directed` and `undirected` weights on a graph with one-way relationships; verify results differ.

- [X] T040 [US2] Write directed/undirected differentiation tests in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: graph with A→B relationship; `hasPath directed A B = True`, `hasPath directed B A = False`, `hasPath undirected B A = True`
- [X] T041 [US2] Write custom `TraversalWeight` test in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: define a weight function reading a numeric property; verify `shortestPath` returns the minimum-weight path, not just the shortest-hop path
- [X] T042 [US2] Write property test in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: for any graph with at least one directed relationship, `connectedComponents undirected` has ≤ components than `connectedComponents directed`
- [X] T043 [US2] Write `directedReverse` test: `hasPath directedReverse B A = True` on the same A→B graph
- [X] T044 [US2] Run US2 tests: `cabal test pattern-test` — all TraversalWeight differentiation tests pass

**Checkpoint**: Traversal policy is confirmed to be call-site controlled. US2 independently verified.

---

## Phase 5: User Story 3 — Compose Graph Views Without New Types (Priority: P3)

**Goal**: `frameQuery` and `memoizeIncidentRels` produce correct `GraphQuery` values that algorithms treat as complete graphs.

**Independent Test**: Apply `frameQuery` with a predicate, call `queryNodes` on the result, verify only matching nodes are returned; run an algorithm and confirm it operates only within the frame.

- [X] T045 [US3] Implement `frameQuery :: (Pattern v -> Bool) -> GraphQuery v -> GraphQuery v` in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`
- [X] T046 [US3] Implement `memoizeIncidentRels :: Ord (Id v) => GraphQuery v -> GraphQuery v` in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`
- [X] T047 [US3] Write unit tests for `frameQuery` in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`
- [X] T048 [US3] Write unit test: `frameQuery` producing an empty graph — algorithms terminate and return empty results
- [X] T049 [US3] Write unit test for `memoizeIncidentRels` in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`
- [X] T050 [US3] Write composition test in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`
- [X] T051 [US3] Write property test: `frameQuery` preserves all `GraphQuery` invariants
- [X] T052 [US3] Run US3 tests: `cabal test pattern-test` — all frameQuery and memoizeIncidentRels tests pass

**Checkpoint**: Composability combinators work correctly. US3 independently verified.

---

## Phase 6: User Story 4 — Upward Context Traversal (Priority: P4)

**Goal**: `queryContainers` and the derived context helpers correctly identify all containing structures for any element.

**Independent Test**: Construct a graph with annotations and walks, call `queryContainers` on a node, verify all containing structures are returned.

- [X] T053 [US4] Implement `queryAnnotationsOf :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T054 [US4] Implement `queryWalksContaining :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T055 [US4] Implement `queryCoMembers :: GraphQuery v -> Pattern v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [X] T056 [US4] Write unit tests for `queryContainers` in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`
- [X] T057 [US4] Write unit tests for `queryAnnotationsOf` and `queryWalksContaining` in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`
- [X] T058 [US4] Write unit test for `queryCoMembers` in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`
- [X] T059 [US4] Run US4 tests: `cabal test pattern-test` — all context query helper tests pass

**Checkpoint**: Upward context traversal works correctly. US4 independently verified.

---

## Phase 7: User Story 5 — Backward-Compatible GraphLens Algorithms (Priority: P5)

**Goal**: Existing `bfs`, `findPath`, `connectedComponents` on `GraphLens` continue to work unchanged, now as wrappers over `Pattern.Graph.Algorithms`.

**Independent Test**: Run the existing `GraphSpec` test suite without modification; all tests pass.

- [X] T060 [US5] `bfs`, `findPath`, `connectedComponents` in `Pattern.Graph` retain their original implementations — wrapping via `Pattern.Graph.Algorithms` is architecturally blocked by a module cycle (`Graph` → `GraphQuery` → `PatternGraph` → `Graph`). `fromGraphLens` was moved to `Pattern.Graph.GraphQuery` (imports `Pattern.Graph`); `fromPatternGraph` was moved to `Pattern.PatternGraph` (imports `Pattern.Graph.GraphQuery`). The cycle is broken; backward-compatible functions remain in `Pattern.Graph`.
- [X] T061 [US5] (see T060 — wrapper approach blocked by module cycle; original implementation retained)
- [X] T062 [US5] (see T060 — wrapper approach blocked by module cycle; original implementation retained)
- [X] T063 [US5] Internal helpers (`bfsHelper`, `findPathHelper`, `findComponents`) retained as they back the public API
- [X] T064 [US5] Existing `Pattern.Graph` tests pass without modification — backward compatibility confirmed
- [X] T065 [US5] Full test suite passes: 786 examples, 0 failures

**Checkpoint**: All five user stories complete. Full test suite passes.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, Haddock coverage, cabal export hygiene, and quickstart validation.

- [X] T066 [P] Haddock module-level documentation in `Pattern.Graph.GraphQuery.hs`: categorical interpretation, design principles, updated usage example
- [X] T067 [P] Haddock module-level documentation in `Pattern.Graph.Algorithms.hs`: all public functions documented with purpose, complexity, and examples
- [X] T068 [P] All public functions in `Pattern.Graph.GraphQuery` and `Pattern.Graph.Algorithms` have Haddock comments
- [X] T069 Updated `libs/pattern/src/Pattern.hs` to re-export `Pattern.Graph.GraphQuery`; `Pattern.Graph.Algorithms` not re-exported (algorithm names would conflict with `Pattern.Graph` names)
- [X] T070 `pattern.cabal` `exposed-modules` verified complete: `Pattern.Graph.GraphQuery`, `Pattern.Graph.Algorithms`, `Pattern.PatternGraph` all present
- [X] T071 Quickstart smoke test: `fromPatternGraph` moved to `Pattern.PatternGraph`; quickstart.md updated to reflect correct import paths; all examples verified against passing tests
- [X] T073b Representation-independence test written and passing: hand-built `GraphQuery` over A→B→C; `bfs`, `shortestPath`, `connectedComponents` all produce correct results (SC-007)
- [X] T072 `cabal build all` and `cabal test pattern-test` pass: 789 examples, 0 failures
- [ ] T073 Commit all changes on branch `035-graph-query` with a checkpoint message

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies — start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 — BLOCKS all user story phases
- **Phase 3 (US1)**: Depends on Phase 2 — core algorithms; no other story dependency
- **Phase 4 (US2)**: Depends on Phase 2 — traversal weight tests extend Phase 3 algorithms; can run in parallel with Phase 3 once Phase 2 is done
- **Phase 5 (US3)**: Depends on Phase 2 — combinators; can run in parallel with Phase 3/4
- **Phase 6 (US4)**: Depends on Phase 2 — context helpers; can run in parallel with Phase 3/4/5
- **Phase 7 (US5)**: Depends on Phase 3 (algorithms must exist before wrappers delegate to them)
- **Phase 8 (Polish)**: Depends on all user story phases

### User Story Dependencies

- **US1 (P1)**: After Phase 2 only — no story dependencies
- **US2 (P2)**: After Phase 2 only — tests extend US1 algorithms but US2 is independently testable
- **US3 (P3)**: After Phase 2 only — combinators are independent of algorithms
- **US4 (P4)**: After Phase 2 only — context helpers are independent of traversal algorithms
- **US5 (P5)**: After Phase 3 (US1) — wrappers delegate to `Algorithms` module

### Within Each Phase

- Tasks within a phase run top-to-bottom unless marked `[P]`
- `[P]` tasks within the same phase can run in parallel (different files)
- Tests run after the implementation tasks they cover

### Parallel Opportunities

- T003, T004, T005, T006 (Phase 1) — all parallel, different files
- T008–T014 (Phase 2) — T008/T009 first, then T010; T011/T012 after T010; T013/T014 after T012
- T020–T032 (Phase 3 implementations) — T019 first (helper), then T020–T032 are largely parallel (different functions in same file; coordinate on imports)
- T033–T037 (Phase 3 tests) — all parallel after implementations
- T040–T043 (Phase 4) — all parallel
- T045–T046 (Phase 5 implementations) — parallel (different functions)
- T047–T051 (Phase 5 tests) — parallel after implementations
- T053–T055 (Phase 6 implementations) — parallel
- T056–T058 (Phase 6 tests) — parallel after implementations
- T066–T068 (Phase 8) — all parallel

---

## Parallel Example: Phase 3 (US1)

```
# After T019 (neighbor helper), launch algorithm implementations in parallel:
Task T020: bfs in libs/pattern/src/Pattern/Graph/Algorithms.hs
Task T021: dfs in libs/pattern/src/Pattern/Graph/Algorithms.hs
Task T022: shortestPath in libs/pattern/src/Pattern/Graph/Algorithms.hs
Task T023: hasPath in libs/pattern/src/Pattern/Graph/Algorithms.hs
Task T028: topologicalSort in libs/pattern/src/Pattern/Graph/Algorithms.hs
Task T029: hasCycle in libs/pattern/src/Pattern/Graph/Algorithms.hs
Task T031: degreeCentrality in libs/pattern/src/Pattern/Graph/Algorithms.hs

# After implementations, launch tests in parallel:
Task T033: traversal/path tests in AlgorithmsSpec.hs
Task T034: structural algorithm tests in AlgorithmsSpec.hs
Task T035: centrality tests in AlgorithmsSpec.hs
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (~7 tasks)
2. Complete Phase 2: Foundational (~11 tasks, including tests)
3. Complete Phase 3: US1 — all algorithms (~21 tasks)
4. **STOP and VALIDATE**: `cabal test pattern-test` — all algorithms work on `PatternGraph` directly
5. Demo: construct `PatternGraph`, call `shortestPath`, `connectedComponents` — no `GraphLens` needed

### Incremental Delivery

1. Phase 1 + 2 → Foundation ready (types, constructors, tests)
2. Phase 3 → US1 complete: all algorithms work on any `GraphQuery` (MVP)
3. Phase 4 → US2 complete: traversal policy confirmed call-site controlled
4. Phase 5 → US3 complete: composability (framing, memoization) working
5. Phase 6 → US4 complete: upward context traversal working
6. Phase 7 → US5 complete: backward compatibility confirmed
7. Phase 8 → Polish, docs, final validation

### Parallel Team Strategy

With multiple developers after Phase 2 is complete:

- Developer A: Phase 3 (US1 — algorithms)
- Developer B: Phase 5 (US3 — combinators, independent of algorithms)
- Developer C: Phase 6 (US4 — context helpers, independent of algorithms)
- Phase 4 (US2) and Phase 7 (US5) follow naturally after Phase 3

---

## Notes

- `[P]` tasks operate on different files or different functions within a file — coordinate on imports
- Each user story phase is independently completable and testable via `cabal test pattern-test`
- T014 implemented as removal of `toGraphLens` and `toGraphLensWithScope` (breaking change); callers must migrate to `fromPatternGraph`. See research.md Decision 7.
- The `memoizeIncidentRels` implementation (T046) should use a pure approach (pre-build a `Map (Id v) [Pattern v]` from `queryNodes`) rather than `IORef` to stay in pure Haskell
- Constitution requires property-based tests (QuickCheck) for mathematical properties — T016, T036, T042, T051 are the key property tests
- Commit after each phase checkpoint (constitution: frequent checkpoint commits)
