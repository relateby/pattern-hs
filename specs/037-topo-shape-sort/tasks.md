# Tasks: topoShapeSort — Graph Element Ordering Rename and Behavioral Correction

**Input**: Design documents from `specs/037-topo-shape-sort/`
**Prerequisites**: plan.md ✓, spec.md ✓, research.md ✓, data-model.md ✓, contracts/Transform.hs ✓, quickstart.md ✓

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files or independent additions to the same file)
- **[Story]**: User story from spec.md (US1–US4)
- Exact file paths in every description

---

## Phase 1: Setup

**Purpose**: Audit existing references before any changes.

- [ ] T001 Verify `sortByArity` is not exported and has no cross-module imports — search `libs/pattern/src/` for any file other than `Transform.hs` that references `sortByArity`; confirm FR-008 scope is module-local only

**Checkpoint**: Confirmed rename is safe to proceed — no external callers to update.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core algorithm implementation that all user stories depend on. US1 tests cannot be written meaningfully until this is complete.

**⚠️ CRITICAL**: All user story work depends on this phase.

- [ ] T002 Implement `withinBucketTopoSort` helper using Kahn's algorithm in `libs/pattern/src/Pattern/Graph/Transform.hs` — pure function, requires `GraphValue v` constraint; builds `idMap`, `inBucketDeps`, `dependents`, `inDegree` from `data-model.md`; cycle members appended in encountered order with no error (see `specs/037-topo-shape-sort/data-model.md` for algorithm detail)

- [ ] T003 Rename `sortByArity` → `topoShapeSort` in `libs/pattern/src/Pattern/Graph/Transform.hs`: (a) add `GraphValue v` constraint to the function signature, (b) update the implementation to call `withinBucketTopoSort` for the `GAnnotation` and `GOther` buckets (GNode/GRelationship/GWalk buckets remain filter-only), (c) replace both call sites in `paraGraph` (line 237) and `paraGraphWithSeed` (line 307), (d) replace the function's inline comment with the Haddock block from `specs/037-topo-shape-sort/contracts/Transform.hs`

**Checkpoint**: Build passes (`cabal build libs/pattern`). All pre-existing tests in `TransformSpec.hs` pass unchanged. Foundation ready.

---

## Phase 3: User Story 1 — Correct fold results for within-bucket dependencies (Priority: P1) 🎯 MVP

**Goal**: `paraGraph` produces correct `subResults` for annotation-of-annotation and `GOther`-of-`GOther` graphs; cycles degrade gracefully to empty `subResults`.

**Independent Test**: Construct a `GraphView` with annotation B annotating a node and annotation A annotating B; call `paraGraph` with a fold that records `subResults` length; assert A receives exactly 1 result (B's), not 0.

- [ ] T004 [P] [US1] Add `topoShapeSort` annotation-of-annotation ordering test to `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` — given two `GAnnotation` elements where A's `elements` contains B, assert B appears before A in `topoShapeSort` output (call `topoShapeSort` directly or verify via `paraGraph` accumulator order)

- [ ] T005 [P] [US1] Add `topoShapeSort` cycle soft-failure test to `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` — given a cycle (annotation A references B, B references A), assert no exception is raised and output length equals input length

- [ ] T006 [P] [US1] Add `paraGraph` annotation-of-annotation fold correctness test (SC-006) to `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` — build a `GraphView` with node `n`, annotation `b` annotating `n`, annotation `a` annotating `b`; call `paraGraph` with a fold that returns `length subResults`; assert result for `a` is 1 (receives `b`'s result), result for `b` is 1 (receives `n`'s result)

- [ ] T007 [P] [US1] Add `topoShapeSort` `GOther`-of-`GOther` ordering test to `libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs` — given two `GOther` elements where X's `elements` contains Y, assert Y appears before X in `topoShapeSort` output

**Checkpoint**: All four new tests pass. `paraGraph` with annotation-of-annotation produces correct `subResults`. US1 independently verifiable.

---

## Phase 4: User Story 2 — `paraGraph` documentation (Priority: P2)

**Goal**: A developer reading `paraGraph`'s Haddock understands the processing order and `subResults` contract without needing external context.

**Independent Test**: Read the updated Haddock for `paraGraph` in Transform.hs; confirm it names `topoShapeSort`, describes the 5-layer processing order, and explicitly documents the `subResults` best-effort contract (cycle omission).

- [ ] T008 [US2] Replace `paraGraph` Haddock comment in `libs/pattern/src/Pattern/Graph/Transform.hs` with the doc block from `specs/037-topo-shape-sort/contracts/Transform.hs` — must include: Processing Order section (5 layers), `subResults` contract section (best-effort, omission for cycle members), and a usage example

**Checkpoint**: `paraGraph` Haddock accurately describes `topoShapeSort` ordering and `subResults` contract. US2 independently verifiable by reading the source.

---

## Phase 5: User Story 3 — `paraGraphWithSeed` and `paraGraphFixed` documentation (Priority: P3)

**Goal**: A developer reading `paraGraphFixed` or `paraGraphWithSeed` sees consistent references to `topoShapeSort` and understands the per-round ordering.

**Independent Test**: Read the updated Haddock for `paraGraphFixed` and `paraGraphWithSeed` in Transform.hs; confirm both reference `topoShapeSort` and note ordering is recomputed per round from the immutable `GraphView`.

- [ ] T009 [US3] Replace `paraGraphFixed` and `paraGraphWithSeed` Haddock comments in `libs/pattern/src/Pattern/Graph/Transform.hs` with the doc blocks from `specs/037-topo-shape-sort/contracts/Transform.hs` — must cross-reference `paraGraph`'s `subResults` contract and note that the immutable `GraphView` produces identical ordering every round

**Checkpoint**: `paraGraphFixed`/`paraGraphWithSeed` docs consistent with `paraGraph`. US3 independently verifiable.

---

## Phase 6: User Story 4 — Module-level documentation vocabulary (Priority: P4)

**Goal**: The module header uses "shape" as the organizing concept, aligns with `classifyByShape`/`topoShapeSort`, and contains no references to "arity" as a sort criterion.

**Independent Test**: Read the module-level Haddock block at the top of Transform.hs; confirm "shape" appears as the organizing vocabulary, `topoShapeSort` is referenced, and "arity" does not appear as a sort criterion.

- [ ] T010 [US4] Update the module-level Haddock comment in `libs/pattern/src/Pattern/Graph/Transform.hs` — remove any "arity" sort language, update the `== Overview` and `== Example` sections to reflect `topoShapeSort`-based processing, ensure vocabulary aligns with `classifyByShape`

**Checkpoint**: Module header passes vocabulary audit. Zero uses of "arity" as sort criterion in entire module. US4 independently verifiable.

---

## Phase 7: Polish & Cross-Cutting Concerns

- [ ] T011 [P] Review and update `docs/reference/features/para-graph.md` post-implementation — verify code examples in "Annotation-of-Annotation Example" and "Cross-Language Implementation Notes" sections match the final `topoShapeSort` and `withinBucketTopoSort` implementations; update if any details changed during implementation

- [ ] T012 Run full test suite (`cabal test pattern`) and confirm all pre-existing tests still pass alongside the new T004–T007 tests — verify SC-004 (no behavioral regression) and SC-006 (annotation-of-annotation test passes)

- [ ] T013 [P] Verify SC-001: search entire codebase for remaining references to `sortByArity` and confirm zero results — `grep -r sortByArity libs/ specs/ docs/`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately
- **Foundational (Phase 2)**: Depends on Setup audit result — BLOCKS all user story phases
- **US1 (Phase 3)**: Depends on Foundational — behavioural tests require `topoShapeSort` to exist
- **US2 (Phase 4)**: Depends on Foundational — docs reference `topoShapeSort` by name
- **US3 (Phase 5)**: Depends on Foundational — same reason
- **US4 (Phase 6)**: Depends on Foundational — same reason; US2–US4 can run in parallel after Foundational
- **Polish (Phase 7)**: Depends on all desired stories being complete

### User Story Dependencies

- **US1 (P1)**: Start after Phase 2. Independent of US2–US4.
- **US2 (P2)**: Start after Phase 2. Independent of US1, US3, US4.
- **US3 (P3)**: Start after Phase 2. Independent of US1, US2, US4.
- **US4 (P4)**: Start after Phase 2. Independent of US1, US2, US3.

### Within Phase 3 (US1)

T004, T005, T006, T007 are all additions to `TransformSpec.hs`. They are independent `it` blocks in separate `describe` groups and can be written in any order. All four are marked [P].

---

## Parallel Execution Examples

### After Phase 2 completes — US stories in parallel (team scenario)

```
Developer A: Phase 3 (US1 tests — T004, T005, T006, T007)
Developer B: Phase 4 (US2 docs — T008)
Developer C: Phases 5+6 (US3+US4 docs — T009, T010)
```

### Within Phase 3 (US1) — all four test tasks in parallel

```
T004: annotation-of-annotation ordering test
T005: cycle soft-failure test
T006: paraGraph fold correctness test (SC-006)
T007: GOther-of-GOther ordering test
```

### Polish — parallel tasks

```
T011: para-graph.md review
T013: sortByArity reference audit
```

---

## Implementation Strategy

### MVP First (US1 Only)

1. Complete Phase 1: Setup audit
2. Complete Phase 2: Implement `withinBucketTopoSort` + rename to `topoShapeSort`
3. Complete Phase 3: Write and pass all four US1 tests
4. **STOP and VALIDATE**: `paraGraph` correctly folds annotation-of-annotation graphs
5. Ship: behavioral correctness is the highest-value change

### Incremental Delivery

1. Phase 1 + Phase 2 → `topoShapeSort` compiles and passes existing tests
2. Phase 3 → behavioral correctness proven by tests (MVP)
3. Phase 4 → `paraGraph` docs accurate
4. Phase 5 → `paraGraphFixed`/`paraGraphWithSeed` docs consistent
5. Phase 6 → module header clean
6. Phase 7 → doc file verified, full test pass, zero `sortByArity` references

---

## Notes

- `withinBucketTopoSort` (T002) and `topoShapeSort` rename (T003) are in the same file but logically separable — T002 adds a new function, T003 modifies the existing one
- T003 is the only task that removes existing code (`sortByArity` definition and its original call sites)
- T008–T010 are all edits to Haskell Haddock in `Transform.hs` — sequential to avoid file conflicts
- T011 and T013 touch different files and can run in parallel with each other and with T012
- Commit after each Phase 2 task (T002, T003) as checkpoint per constitution Version Control Standards
