# Tasks: RepresentationMap

**Input**: Design documents from `specs/039-representation-map/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Included — spec and plan explicitly require unit tests for each phase and QuickCheck property tests for round-trip correctness (US3).

**Organization**: Tasks are grouped by user story. US2 (Transform) and US4 (Compose) share a phase because `compose` lives in the same new module as `RepresentationMap`. US3 (Round-trip) is separated because it includes the concrete `diagnosticMap` and property-based tests.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies on incomplete tasks)
- **[Story]**: Which user story this task belongs to (US1–US4)

---

## Phase 1: Setup

**Purpose**: Scaffold new module files and wire them into the build before any user story implementation begins.

- [ ] T001 Add `{-# LANGUAGE RankNTypes #-}` to `libs/pattern/src/Pattern/Core.hs` (required for `forall q.` in PatternKind fields)
- [ ] T002 Create `libs/pattern/src/Pattern/RepresentationMap.hs` with module declaration, imports, and empty export list (scaffold only — no types yet)
- [ ] T003 Add `Pattern.RepresentationMap` to `exposed-modules` in `libs/pattern/pattern.cabal`
- [ ] T004 Add `import Pattern.RepresentationMap` and re-export stub to `libs/pattern/src/Pattern.hs`
- [ ] T005 Create `libs/pattern/tests/Spec/Pattern/RepresentationMapSpec.hs` with module declaration and empty HSpec `spec :: Spec` (confirms test runner wires up)

**Checkpoint**: `cabal build` and `cabal test` pass with empty new files in place.

---

## Phase 2: Foundational (Blocking Prerequisite)

**Purpose**: Confirm existing infrastructure is importable from the new module. This phase is minimal because the foundation (ScopeQuery, TrivialScope, ScopeDict, Pattern) already exists.

**⚠️ CRITICAL**: US1–US4 implementation cannot begin until this phase is complete and building cleanly.

- [ ] T006 In `Pattern/RepresentationMap.hs`, add import of `Pattern.Core` (Pattern, ScopeQuery, PatternKind placeholder) — confirms module graph is valid before adding types
- [ ] T007 In `RepresentationMapSpec.hs`, add import of `Pattern.Core` (Pattern, trivialScope, TrivialScope) — confirms test file can reach existing infrastructure

**Checkpoint**: `cabal build` passes cleanly with new files importing from existing modules.

---

## Phase 3: User Story 1 — Recognize Named Shape Kinds (P1) 🎯 MVP

**Goal**: `PatternKind v` exists in `Pattern.Core` with a scope-aware membership predicate, a kind name, and a canonical example. Callers can define shape kinds and check pattern membership.

**Independent Test**: Define two distinct `PatternKind` values with structural predicates. Verify the canonical example satisfies its own predicate (T-US1-001). Verify a non-matching pattern fails (T-US1-002). No other stories or new modules required.

### Tests for User Story 1

> **Write these tests FIRST — they must FAIL before implementation begins**

- [ ] T008 [P] [US1] In `RepresentationMapSpec.hs`: write T-US1-001 — `kindPred k (trivialScope (kindExample k)) (kindExample k)` is `True` for any `PatternKind` (canonical example satisfies its own kind)
- [ ] T009 [P] [US1] In `RepresentationMapSpec.hs`: write T-US1-002 — `kindPred` returns `False` for a pattern that does not match the kind's structural constraint
- [ ] T010 [P] [US1] In `RepresentationMapSpec.hs`: write T-US1-003 — a scope-relative predicate using `allElements` correctly identifies a pattern that is only visible through the scope
- [ ] T011 [P] [US1] In `RepresentationMapSpec.hs`: write T-US1-004 — a structural predicate returns the same result regardless of the scope argument supplied

### Implementation for User Story 1

- [ ] T012 [US1] Add `PatternKind v` data type to `libs/pattern/src/Pattern/Core.hs` with fields `kindName :: String`, `kindPred :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool`, `kindExample :: Pattern v` — requires `RankNTypes` (added in T001)
- [ ] T013 [US1] Add `checkKind :: ScopeQuery q v => PatternKind v -> q v -> Pattern v -> Bool` to `libs/pattern/src/Pattern/Core.hs` — convenience wrapper over `kindPred`
- [ ] T014 [US1] Add `PatternKind(..)` and `checkKind` to the export list of `libs/pattern/src/Pattern/Core.hs`
- [ ] T015 [US1] Add `PatternKind(..)` and `checkKind` to re-exports in `libs/pattern/src/Pattern.hs`
- [ ] T016 [US1] Update `Pattern/RepresentationMap.hs` import of `Pattern.Core` to include `PatternKind`, `kindName` (needed by `compose`)

**Checkpoint**: T008–T011 all pass. `PatternKind` is exported and usable from `Pattern.Core`. `cabal test` passes with no regressions.

**Commit**: `"representation-map: PatternKind in Pattern.Core"`

---

## Phase 4: User Story 2 + User Story 4 — Transform and Compose (P2 + P4)

**Goal (US2)**: `RepresentationMap v` exists in `Pattern.RepresentationMap` with `forward`, `inverse`, and `roundTrip` fields. A map can be constructed over any two shape kinds.

**Goal (US4)**: `compose` combines two compatible maps into a third. Incompatible kinds produce a `Left` error with a descriptive message.

US2 and US4 share this phase because `compose` is defined in the same new module as `RepresentationMap` and has no independent testability from it.

**Independent Test (US2)**: Construct a minimal `RepresentationMap` between two `PatternKind`s with identity-like `forward`/`inverse`. Verify `forward q p` satisfies `kindPred (codomain m)` (T-US2-001).

**Independent Test (US4)**: Compose two maps. Verify the combined `name`, `conventions`, and `forward` behaviour (T-US4-001). Verify `Left` on kind mismatch (T-US4-003).

### Tests for User Story 2

> **Write these tests FIRST — they must FAIL before implementation begins**

- [ ] T017 [P] [US2] In `RepresentationMapSpec.hs`: write T-US2-001 — `kindPred (codomain m) q (forward m q p)` is `True` for a domain-kind pattern `p`
- [ ] T018 [P] [US2] In `RepresentationMapSpec.hs`: write T-US2-002 — `kindPred (domain m) q (inverse m q p)` is `True` for a codomain-kind pattern `p`
- [ ] T019 [P] [US2] In `RepresentationMapSpec.hs`: write T-US2-003 — `conventions m` is accessible and matches declared list

### Tests for User Story 4

- [ ] T020 [P] [US4] In `RepresentationMapSpec.hs`: write T-US4-001 — `compose m1 m2` with compatible kinds returns `Right`; combined `name` contains " >>> "
- [ ] T021 [P] [US4] In `RepresentationMapSpec.hs`: write T-US4-002 — composed `conventions` equals `conventions m1 <> conventions m2`
- [ ] T022 [P] [US4] In `RepresentationMapSpec.hs`: write T-US4-003 — `compose m1 m2` returns `Left` when `kindName (codomain m1) /= kindName (domain m2)`; error message contains both kind names
- [ ] T023 [P] [US4] In `RepresentationMapSpec.hs`: write T-US4-004 — composed `forward` applies `m1.forward` then `m2.forward`; composed `inverse` applies `m2.inverse` then `m1.inverse`

### Implementation for User Story 2

- [ ] T024 [US2] Add `RepresentationMap v` data type to `libs/pattern/src/Pattern/RepresentationMap.hs` with all seven fields (`name`, `domain`, `codomain`, `conventions`, `forward`, `inverse`, `roundTrip`) — add `{-# LANGUAGE RankNTypes #-}` pragma to this module
- [ ] T025 [US2] Add `RepresentationMap(..)` to the export list of `libs/pattern/src/Pattern/RepresentationMap.hs`
- [ ] T026 [US2] Add `RepresentationMap(..)` to re-exports in `libs/pattern/src/Pattern.hs`

### Implementation for User Story 4

- [ ] T027 [US4] Implement `compose :: RepresentationMap v -> RepresentationMap v -> Either String (RepresentationMap v)` in `libs/pattern/src/Pattern/RepresentationMap.hs` — runtime kind-compat check via `kindName`; combined name/conventions/forward/inverse/roundTrip as specified in contract
- [ ] T028 [US4] Add `compose` to the export list of `libs/pattern/src/Pattern/RepresentationMap.hs`
- [ ] T029 [US4] Add `compose` to re-exports in `libs/pattern/src/Pattern.hs`

**Checkpoint**: T017–T023 all pass. `RepresentationMap` and `compose` are exported. `cabal test` passes with no regressions.

**Commit**: `"representation-map: RepresentationMap + compose"`

---

## Phase 5: User Story 3 — Verify Round-Trip Correctness (P3)

**Goal**: A concrete `diagnosticMap` (DiagnosticPattern ↔ DiagnosticGraph) demonstrates that the abstraction works end-to-end. QuickCheck property tests verify the round-trip for all generated inputs. The canonical example passes both kind predicates. This phase lives entirely in the test file — no library code changes.

**Independent Test**: Run `cabal test` and confirm the QuickCheck property `prop_diagnosticRoundTrip` passes over 100+ generated inputs. Confirm T-US3-001 (canonical forward → DiagnosticGraph) and T-US3-002 (canonical inverse → DiagnosticPattern) pass as unit tests.

### Tests for User Story 3 (unit + property — write tests first, then forward, then inverse)

- [ ] T030 [US3] In `RepresentationMapSpec.hs`: define `diagnosticPatternKind :: PatternKind Subject` — predicate checks for `"Location"` label containing a `"Diagnostic"` label; canonical example is a minimal nested pattern
- [ ] T031 [US3] In `RepresentationMapSpec.hs`: define `diagnosticGraphKind :: PatternKind Subject` — predicate checks for flat atomic patterns with `"Location"`/`"Diagnostic"`/`"Remediation"` labels plus `_arity` and `_depth` properties
- [ ] T032 [US3] In `RepresentationMapSpec.hs`: write T-US3-001 — `checkKind diagnosticGraphKind q (forward diagnosticMap q canonicalDiagnosticPattern)` is `True`
- [ ] T033 [US3] In `RepresentationMapSpec.hs`: write T-US3-002 — `checkKind diagnosticPatternKind q (inverse diagnosticMap q canonicalDiagnosticGraph)` is `True`
- [ ] T034 [US3] In `RepresentationMapSpec.hs`: write T-US3-003 — `(inverse diagnosticMap q . forward diagnosticMap q) canonicalDiagnosticPattern == canonicalDiagnosticPattern` (round-trip on canonical example)

### Implementation for User Story 3

- [ ] T035 [US3] In `RepresentationMapSpec.hs`: implement `diagnosticForward :: ScopeQuery q Subject => q Subject -> Pattern Subject -> Pattern Subject` — traverse nested structure with `paraWithScope`; emit flat atomic patterns with `_arity`/`_depth` properties; emit `AT` and `HAS_REMEDIATION` relationship patterns
- [ ] T036 [US3] In `RepresentationMapSpec.hs`: implement `diagnosticInverse :: ScopeQuery q Subject => q Subject -> Pattern Subject -> Pattern Subject` — use `allElements` and `containers` on scope to find Diagnostic/Location/Remediation patterns; reconstruct nesting order from `_depth`; emit nested `Pattern` structure
- [ ] T037 [US3] In `RepresentationMapSpec.hs`: assemble `diagnosticMap :: RepresentationMap Subject` using `diagnosticPatternKind`, `diagnosticGraphKind`, `diagnosticForward`, `diagnosticInverse`, and an inline `roundTrip` check; conventions: `["_arity encodes element count", "_depth encodes nesting depth"]`

### Property tests for User Story 3

- [ ] T038 [US3] In `RepresentationMapSpec.hs`: add `Arbitrary` instance or generator for `DiagnosticPattern`-satisfying `Pattern Subject` values (Location containing Diagnostic containing N Remediations, N ∈ [0..3])
- [ ] T039 [US3] In `RepresentationMapSpec.hs`: write QuickCheck property `prop_diagnosticRoundTrip` — `forAll` generated DiagnosticPattern inputs, `roundTrip diagnosticMap (trivialScope p) p` holds; use `property` from `Test.QuickCheck`
- [ ] T040 [US3] In `RepresentationMapSpec.hs`: write T-US3-004 — `diagnosticMap` conventions list contains `"_arity"` and `"_depth"` substrings
- [ ] T041 [US3] In `RepresentationMapSpec.hs`: write composition smoke test — `compose diagnosticMap identityLikeMap` returns `Right`; composed round-trip holds for canonical example (covers SC-004)

**Checkpoint**: All T030–T041 pass. QuickCheck `prop_diagnosticRoundTrip` reports 100 tests passed. `cabal test` passes with no regressions.

**Commit**: `"representation-map: diagnosticMap prototype and round-trip tests"`

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Regression validation, documentation completeness, and export hygiene.

- [ ] T042 [P] Run full `pattern-hs` test suite (`cabal test`); confirm zero regressions — all pre-existing tests pass without modification
- [ ] T043 [P] Add Haddock documentation to `PatternKind` fields in `libs/pattern/src/Pattern/Core.hs` — include categorical interpretation (subobject classifier) and invariant (`kindPred q (kindExample k) == True`)
- [ ] T044 [P] Add Haddock documentation to `RepresentationMap` fields and `compose` in `libs/pattern/src/Pattern/RepresentationMap.hs` — include categorical interpretation (named isomorphism, morphism composition), invariants, and cross-language correspondence table (Haskell→Rust)
- [ ] T045 Verify `Pattern.hs` re-exports: `PatternKind`, `checkKind`, `RepresentationMap`, `compose` are all reachable from a single `import Pattern` — write a one-line smoke test confirming import in `RepresentationMapSpec.hs`

**Commit**: `"representation-map: documentation and export verification"`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies — start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 — blocks all user story phases
- **Phase 3 (US1 PatternKind)**: Depends on Phase 2 — independently completable
- **Phase 4 (US2+US4 RepresentationMap+compose)**: Depends on Phase 3 (needs `PatternKind` and `kindName`)
- **Phase 5 (US3 Round-trip)**: Depends on Phase 4 (needs `RepresentationMap` and `compose`)
- **Phase 6 (Polish)**: Depends on Phase 5

### User Story Dependencies

- **US1 (P1)**: Unblocked after Phase 2. No dependency on other user stories.
- **US2 (P2)**: Depends on US1 — `RepresentationMap.domain` and `RepresentationMap.codomain` are `PatternKind` values.
- **US4 (P4)**: Depends on US2 — `compose` operates on `RepresentationMap` values.
- **US3 (P3)**: Depends on US2 + US4 — `diagnosticMap` uses `RepresentationMap`; composition smoke test uses `compose`.

### Within Each Phase

- Tests written and confirmed failing BEFORE implementation tasks begin
- Module-level tasks (T001–T007) must complete before type/function tasks
- Export tasks (T014, T028, T029) run after implementation tasks in same phase
- `cabal build` passes at each checkpoint before commit

### Parallel Opportunities

- T008–T011 (US1 tests) can all be written in parallel — they are independent assertions in the same file
- T017–T023 (US2+US4 tests) can all be written in parallel
- T030–T034 (US3 unit tests) can be written in parallel with T035–T036 (forward/inverse impl) once test scaffolding (T030–T031) is in place
- T042–T044 (Polish) can run in parallel

---

## Parallel Example: User Story 1

```bash
# Write all US1 tests in parallel (same file, no ordering between them):
Task: "T008 — T-US1-001: canonical example satisfies own predicate"
Task: "T009 — T-US1-002: non-matching pattern fails predicate"
Task: "T010 — T-US1-003: scope-relative predicate uses allElements"
Task: "T011 — T-US1-004: structural predicate ignores scope argument"

# Then implement in sequence:
Task: "T012 — PatternKind type definition"
Task: "T013 — checkKind function"
Task: "T014, T015 — exports"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001–T005)
2. Complete Phase 2: Foundational (T006–T007)
3. Complete Phase 3: US1 — PatternKind (T008–T016)
4. **STOP and VALIDATE**: `cabal test` passes; `PatternKind` usable from `import Pattern`
5. Shape kinds are now definable and checkable — independently useful for validation

### Incremental Delivery

1. Phase 1+2 → Build scaffolding clean
2. Phase 3 (US1) → `PatternKind` usable → checkpoint commit
3. Phase 4 (US2+US4) → `RepresentationMap` + `compose` usable → checkpoint commit
4. Phase 5 (US3) → `diagnosticMap` + QuickCheck property tests passing → checkpoint commit
5. Phase 6 → Full suite passing, docs complete → merge-ready

---

## Notes

- All `forall q.` fields in `PatternKind` and `RepresentationMap` require `{-# LANGUAGE RankNTypes #-}` — added to `Pattern/Core.hs` in T001 and to `Pattern/RepresentationMap.hs` in T024
- `compose` uses `kindName` string equality for kind-compat check — phantom type safety is deferred
- The `diagnosticMap` lives in the test file for this prototype; it is not exported from the library
- QuickCheck `suchThat` may produce slow generation if domain kind is complex — keep the `Arbitrary` generator (T038) direct rather than filtering
- Each checkpoint commit keeps the branch bisectable
