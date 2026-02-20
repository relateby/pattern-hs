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

- [ ] T001 Add `Pattern.Graph.GraphQuery` and `Pattern.Graph.Algorithms` to `exposed-modules` in `libs/pattern/pattern.cabal`
- [ ] T002 Add `Spec.Pattern.Graph.GraphQuerySpec` and `Spec.Pattern.Graph.AlgorithmsSpec` to `other-modules` in the `pattern-test` stanza of `libs/pattern/pattern.cabal`
- [ ] T003 [P] Create empty module stub `libs/pattern/src/Pattern/Graph/GraphQuery.hs` with module declaration and exports list (no implementations yet)
- [ ] T004 [P] Create empty module stub `libs/pattern/src/Pattern/Graph/Algorithms.hs` with module declaration and exports list (no implementations yet)
- [ ] T005 [P] Create empty test file `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs` with module declaration
- [ ] T006 [P] Create empty test file `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs` with module declaration
- [ ] T007 Verify the project builds with stubs: run `cabal build pattern` from repo root

**Checkpoint**: Project builds with new empty modules — ready for foundational work.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and constructors that every user story depends on. MUST be complete before any user story phase begins.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [ ] T008 Implement `TraversalDirection` data type (`Forward | Backward`) with `Eq`, `Show` instances in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`
- [ ] T009 Implement `TraversalWeight v` type alias and the three canonical values (`undirected`, `directed`, `directedReverse`) in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`
- [ ] T010 Implement `GraphQuery v` record type with all nine fields and full Haddock documentation (categorical interpretation, field semantics, invariants) in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`
- [ ] T011 Implement `fromGraphLens :: (GraphValue v, Eq v) => GraphLens v -> GraphQuery v` in `libs/pattern/src/Pattern/Graph/GraphQuery.hs` — derive all fields from existing `Pattern.Graph` functions; `queryNodeById`/`queryRelationshipById` use O(n) scans; `queryContainers` scans relationships and walks
- [ ] T012 Implement `fromPatternGraph :: (GraphValue v, Eq v) => PatternGraph extra v -> GraphQuery v` in `libs/pattern/src/Pattern/Graph/GraphQuery.hs` — read directly from `pgNodes`, `pgRelationships`, `pgWalks`, `pgAnnotations` maps; O(log n) lookups for `queryNodeById`/`queryRelationshipById`
- [ ] T013 Export `fromPatternGraph` from `libs/pattern/src/Pattern/PatternGraph.hs` (add to module export list and import from `Pattern.Graph.GraphQuery`)
- [ ] T014 Add deprecation pragmas to `toGraphLens` and `toGraphLensWithScope` in `libs/pattern/src/Pattern/PatternGraph.hs`: `{-# DEPRECATED toGraphLens "Use fromPatternGraph from Pattern.Graph.GraphQuery instead" #-}`
- [ ] T015 Write unit tests for `GraphQuery` construction in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: verify all nine fields return correct values for a known `PatternGraph` and a known `GraphLens`
- [ ] T016 Write property test in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: `fromGraphLens` and `fromPatternGraph` on equivalent graphs produce the same `queryNodes`, `queryRelationships`, `querySource`, `queryTarget` results
- [ ] T017 Write unit tests for canonical `TraversalWeight` values in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: verify `undirected` returns 1.0 for both directions; `directed` returns 1.0 forward and infinity backward; `directedReverse` is the inverse
- [ ] T018 Verify the project builds and foundational tests pass: `cabal test pattern-test`

**Checkpoint**: Foundation ready — `GraphQuery`, `TraversalWeight`, `fromGraphLens`, `fromPatternGraph` all tested. User story phases can now begin.

---

## Phase 3: User Story 1 — Run Graph Algorithms on Any Representation (Priority: P1) 🎯 MVP

**Goal**: All graph algorithms work against `GraphQuery v` derived from either `GraphLens` or `PatternGraph`, without any intermediate conversion.

**Independent Test**: Construct a `GraphQuery` from a `PatternGraph`, call `shortestPath` and `connectedComponents`, verify correct results — without ever touching `GraphLens`.

- [ ] T019 [US1] Implement internal neighbor-derivation helper in `libs/pattern/src/Pattern/Graph/Algorithms.hs`: given a `GraphQuery v`, a `TraversalWeight v`, and a node, return all reachable neighbor nodes (filters infinite-cost edges)
- [ ] T020 [US1] Implement `bfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T021 [US1] Implement `dfs :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T022 [US1] Implement `shortestPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Maybe [Pattern v]` (Dijkstra) in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T023 [US1] Implement `hasPath :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Bool` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T024 [US1] Implement `allPaths :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> [[Pattern v]]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T025 [US1] Implement `isNeighbor :: Eq (Id v) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Bool` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T026 [US1] Implement `isConnected :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Bool` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T027 [US1] Implement `connectedComponents :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [[Pattern v]]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T028 [US1] Implement `topologicalSort :: Ord (Id v) => GraphQuery v -> Maybe [Pattern v]` (DFS-based; returns `Nothing` on cycle) in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T029 [US1] Implement `hasCycle :: Ord (Id v) => GraphQuery v -> Bool` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T030 [US1] Implement `minimumSpanningTree :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> [Pattern v]` (Kruskal's or Prim's) in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T031 [US1] Implement `degreeCentrality :: Ord (Id v) => GraphQuery v -> Map (Id v) Double` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`
- [ ] T032 [US1] Implement `betweennessCentrality :: Ord (Id v) => GraphQuery v -> TraversalWeight v -> Map (Id v) Double` (Brandes algorithm) in `libs/pattern/src/Pattern/Graph/Algorithms.hs` — **Note**: Brandes is O(n·(n+r)·log n) and calls `queryIncidentRels` in the inner loop; the proposal flags this as the first algorithm that will need bulk adjacency (`Pattern.Graph.Algorithms.Bulk`) for large graphs. Add a `-- TODO: bulk adjacency` comment at the top of the implementation referencing the proposal's open question §1.
- [ ] T033 [US1] Write unit tests for all traversal and path algorithms in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: test `bfs`, `dfs`, `shortestPath`, `hasPath`, `allPaths` on a known graph derived from `PatternGraph` (not `GraphLens`)
- [ ] T034 [US1] Write unit tests for structural algorithms in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: `connectedComponents`, `topologicalSort` (DAG and cyclic), `hasCycle`, `minimumSpanningTree`
- [ ] T035 [US1] Write unit tests for centrality algorithms in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: `degreeCentrality` and `betweennessCentrality` on a small known graph
- [ ] T036 [US1] Write property test: `fromGraphLens` and `fromPatternGraph` on equivalent graphs produce identical `connectedComponents` results in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`
- [ ] T037 [US1] Write edge case tests in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: `shortestPath` returns `Nothing` when no path exists; `topologicalSort` returns `Nothing` on a cycle; `allPaths` on empty graph returns `[]`
- [ ] T038 [US1] Add `{-# INLINE #-}` pragmas to `queryIncidentRels`, `querySource`, `queryTarget`, `queryDegree` field accessor usages in hot-path algorithm loops in `libs/pattern/src/Pattern/Graph/Algorithms.hs`; also consider `{-# UNPACK #-}` on the corresponding fields in the `GraphQuery` record in `libs/pattern/src/Pattern/Graph/GraphQuery.hs` — document any applied `{-# UNPACK #-}` pragmas with a comment explaining they are a Haskell-specific optimization (constitution Principle V: language-specific idioms must be documented)
- [ ] T039 [US1] Run full test suite and verify all US1 tests pass: `cabal test pattern-test`

**Checkpoint**: All graph algorithms work on `GraphQuery` from either source. US1 independently verified.

---

## Phase 4: User Story 2 — Traversal Direction and Weight at the Call Site (Priority: P2)

**Goal**: The same `GraphQuery` produces different traversal results when called with `directed` vs `undirected` `TraversalWeight`, and custom weight functions work correctly.

**Independent Test**: Call `hasPath` on the same `GraphQuery` with `directed` and `undirected` weights on a graph with one-way relationships; verify results differ.

- [ ] T040 [US2] Write directed/undirected differentiation tests in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: graph with A→B relationship; `hasPath directed A B = True`, `hasPath directed B A = False`, `hasPath undirected B A = True`
- [ ] T041 [US2] Write custom `TraversalWeight` test in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: define a weight function reading a numeric property; verify `shortestPath` returns the minimum-weight path, not just the shortest-hop path
- [ ] T042 [US2] Write property test in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: for any graph with at least one directed relationship, `connectedComponents undirected` has ≤ components than `connectedComponents directed`
- [ ] T043 [US2] Write `directedReverse` test: `hasPath directedReverse B A = True` on the same A→B graph
- [ ] T044 [US2] Run US2 tests: `cabal test pattern-test --test-option="--match=/TraversalWeight/"`

**Checkpoint**: Traversal policy is confirmed to be call-site controlled. US2 independently verified.

---

## Phase 5: User Story 3 — Compose Graph Views Without New Types (Priority: P3)

**Goal**: `frameQuery` and `memoizeIncidentRels` produce correct `GraphQuery` values that algorithms treat as complete graphs.

**Independent Test**: Apply `frameQuery` with a predicate, call `queryNodes` on the result, verify only matching nodes are returned; run an algorithm and confirm it operates only within the frame.

- [ ] T045 [US3] Implement `frameQuery :: (Pattern v -> Bool) -> GraphQuery v -> GraphQuery v` in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`: filter `queryNodes` and `queryRelationships` by predicate; filter `queryIncidentRels` to exclude relationships where either endpoint (via `querySource`/`queryTarget`) falls outside the frame — do NOT use a separate `endpoints` helper, use the `GraphQuery` fields directly; pass through `queryDegree`, `queryNodeById`, `queryRelationshipById`, `queryContainers` with appropriate filtering (see research.md Decision 12)
- [ ] T046 [US3] Implement `memoizeIncidentRels :: Ord (Id v) => GraphQuery v -> GraphQuery v` in `libs/pattern/src/Pattern/Graph/GraphQuery.hs`: wrap `queryIncidentRels` with an `IORef`-free pure memoization using `Data.Map` (build cache lazily on first call per node identity, or eagerly from `queryNodes`)
- [ ] T047 [US3] Write unit tests for `frameQuery` in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: mixed-label graph; frame by label; verify `queryNodes` returns only matching nodes; verify `queryIncidentRels` excludes cross-frame relationships
- [ ] T048 [US3] Write unit test: `frameQuery` producing an empty graph — algorithms terminate and return empty results
- [ ] T049 [US3] Write unit test for `memoizeIncidentRels` in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: wrap a `GraphQuery` and verify results are identical to the unwrapped version for all nodes
- [ ] T050 [US3] Write composition test in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: `memoizeIncidentRels . frameQuery predicate $ fromPatternGraph pg` — run `connectedComponents` and verify correct results within the frame
- [ ] T051 [US3] Write property test: `frameQuery` preserves all `GraphQuery` invariants — `querySource r = Just s` implies `s ∈ queryNodes` on the framed result
- [ ] T052 [US3] Run US3 tests: `cabal test pattern-test`

**Checkpoint**: Composability combinators work correctly. US3 independently verified.

---

## Phase 6: User Story 4 — Upward Context Traversal (Priority: P4)

**Goal**: `queryContainers` and the derived context helpers correctly identify all containing structures for any element.

**Independent Test**: Construct a graph with annotations and walks, call `queryContainers` on a node, verify all containing structures are returned.

- [ ] T053 [US4] Implement `queryAnnotationsOf :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`: call `queryContainers`, filter by `GAnnotation` via `classify`
- [ ] T054 [US4] Implement `queryWalksContaining :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`: call `queryContainers`, filter by `GWalk` via `classify`
- [ ] T055 [US4] Implement `queryCoMembers :: GraphQuery v -> Pattern v -> Pattern v -> [Pattern v]` in `libs/pattern/src/Pattern/Graph/Algorithms.hs`: call `queryContainers` on the element, filter to those matching the given container, return co-members
- [ ] T056 [US4] Write unit tests for `queryContainers` in `libs/pattern/tests/Spec/Pattern/Graph/GraphQuerySpec.hs`: node participating in a walk and having an annotation — verify both are returned; node with no containers — verify empty list
- [ ] T057 [US4] Write unit tests for `queryAnnotationsOf` and `queryWalksContaining` in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: verify each returns only the correct category of container
- [ ] T058 [US4] Write unit test for `queryCoMembers` in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: two nodes sharing a walk — verify each is a co-member of the other via that walk
- [ ] T059 [US4] Run US4 tests: `cabal test pattern-test`

**Checkpoint**: Upward context traversal works correctly. US4 independently verified.

---

## Phase 7: User Story 5 — Backward-Compatible GraphLens Algorithms (Priority: P5)

**Goal**: Existing `bfs`, `findPath`, `connectedComponents` on `GraphLens` continue to work unchanged, now as wrappers over `Pattern.Graph.Algorithms`.

**Independent Test**: Run the existing `GraphSpec` test suite without modification; all tests pass.

- [ ] T060 [US5] Replace `bfs` implementation in `libs/pattern/src/Pattern/Graph.hs` with a one-line wrapper: `bfs lens start = Algorithms.bfs (fromGraphLens lens) undirected start`; add imports for `Pattern.Graph.GraphQuery` and `Pattern.Graph.Algorithms`
- [ ] T061 [US5] Replace `findPath` implementation in `libs/pattern/src/Pattern/Graph.hs` with wrapper: `findPath lens s e = Algorithms.shortestPath (fromGraphLens lens) undirected s e`
- [ ] T062 [US5] Replace `connectedComponents` implementation in `libs/pattern/src/Pattern/Graph.hs` with wrapper: `connectedComponents lens = Algorithms.connectedComponents (fromGraphLens lens) undirected`
- [ ] T063 [US5] Remove now-unused internal helpers (`bfsHelper`, `findPathHelper`, `findComponents`) from `libs/pattern/src/Pattern/Graph.hs` if they are no longer referenced
- [ ] T064 [US5] Run existing `GraphSpec` tests without modification to verify backward compatibility: `cabal test pattern-test --test-option="--match=/GraphSpec/"`
- [ ] T065 [US5] Run full test suite to confirm no regressions: `cabal test pattern-test`

**Checkpoint**: All five user stories complete. Full test suite passes.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, Haddock coverage, cabal export hygiene, and quickstart validation.

- [ ] T066 [P] Add Haddock module-level documentation to `libs/pattern/src/Pattern/Graph/GraphQuery.hs`: categorical interpretation of the module, design principles, usage example matching `quickstart.md`
- [ ] T067 [P] Add Haddock module-level documentation to `libs/pattern/src/Pattern/Graph/Algorithms.hs`: categorical interpretation, note on `TraversalWeight` as call-site parameter, complexity summary
- [ ] T068 [P] Verify all public functions in `Pattern.Graph.GraphQuery` and `Pattern.Graph.Algorithms` have Haddock comments with purpose, inputs, outputs, invariants, and at least one example (constitution requirement)
- [ ] T069 Update `libs/pattern/src/Pattern.hs` top-level re-export module (if it exists) to re-export `Pattern.Graph.GraphQuery` and `Pattern.Graph.Algorithms` at appropriate granularity
- [ ] T070 [P] Verify `pattern.cabal` `exposed-modules` list is complete and matches all new source files
- [ ] T071 Run the quickstart examples from `specs/035-graph-query/quickstart.md` as a manual smoke test: construct a `PatternGraph`, derive a `GraphQuery`, run `shortestPath`, `connectedComponents`, `frameQuery`, and `queryAnnotationsOf`
- [ ] T073b Write a representation-independence test in `libs/pattern/tests/Spec/Pattern/Graph/AlgorithmsSpec.hs`: construct a `GraphQuery` from a hand-built record (not `fromGraphLens` or `fromPatternGraph`) over a small fixed graph; verify `bfs`, `shortestPath`, and `connectedComponents` all produce correct results — covers SC-007
- [ ] T072 Run `cabal build all` and `cabal test pattern-test` from repo root — confirm zero warnings and all tests pass
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
- `toGraphLens` deprecation (T014) is non-breaking — existing callers get a compiler warning, not an error
- The `memoizeIncidentRels` implementation (T046) should use a pure approach (pre-build a `Map (Id v) [Pattern v]` from `queryNodes`) rather than `IORef` to stay in pure Haskell
- Constitution requires property-based tests (QuickCheck) for mathematical properties — T016, T036, T042, T051 are the key property tests
- Commit after each phase checkpoint (constitution: frequent checkpoint commits)
