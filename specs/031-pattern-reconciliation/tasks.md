# Tasks: Pattern Subject Reconciliation

**Input**: Design documents from `/specs/031-pattern-reconciliation/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Property-based and unit tests are included to ensure correctness and maintainability.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Haskell mono-repo**: `libs/pattern/src/`, `libs/pattern/tests/`
- Module structure: `Pattern/Reconcile.hs` for implementation
- Test structure: `Spec/Pattern/ReconcileSpec.hs` and `Spec/Pattern/ReconcileProperties.hs`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and module structure setup

- [ ] T001 Create Pattern/Reconcile.hs module skeleton in libs/pattern/src/Pattern/Reconcile.hs with exports
- [ ] T002 Update Pattern.hs to re-export Pattern.Reconcile types and functions in libs/pattern/src/Pattern.hs
- [ ] T003 Update pattern.cabal to include Pattern.Reconcile in exposed-modules
- [ ] T004 Create test file skeleton for ReconcileSpec.hs in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T005 Create test file skeleton for ReconcileProperties.hs in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T006 Update Test.hs to include ReconcileSpec in test suite in libs/pattern/tests/Test.hs

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data types that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T007 [P] Define ReconciliationPolicy data type (LastWriteWins, FirstWriteWins, Merge, Strict) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T008 [P] Define MergeStrategy and sub-strategy types (LabelMerge, PropertyMerge, ElementMerge) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T009 [P] Define Conflict data type with identity, subjects, and paths in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T010 [P] Define ReconcileError data type with message and conflicts list in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T011 [P] Define ReconcileReport data type with statistics fields in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T012 [P] Define Path type alias as [Int] in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T013 Implement defaultMergeStrategy constant (UnionLabels, ShallowMerge, UnionElements) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T014 Add Eq and Show instances for all policy and strategy types in libs/pattern/src/Pattern/Reconcile.hs

**Checkpoint**: Foundation ready - all types defined and exported

---

## Phase 3: User Story 1 - Normalize Parsed Patterns (Priority: P1) 🎯 MVP

**Goal**: Enable reconciliation of patterns with duplicate identities using LastWriteWins and FirstWriteWins policies

**Independent Test**: Parse a gram text with duplicate IDs, call reconcile with LastWriteWins policy, verify result contains each identity once with expected content

### Property Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T015 [P] [US1] Property test: reconciliation is idempotent (reconcile twice = reconcile once) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T016 [P] [US1] Property test: all unique identities are preserved (set of identities unchanged) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T017 [P] [US1] Property test: deterministic results (same input always gives same output) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T018 [P] [US1] QuickCheck generator for Pattern Subject with duplicate identities in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs

### Unit Tests for User Story 1

- [ ] T019 [P] [US1] Unit test: LastWriteWins with duplicate properties (later wins) in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T020 [P] [US1] Unit test: FirstWriteWins with duplicate properties (earlier wins) in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T021 [P] [US1] Unit test: No duplicates returns pattern unchanged in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T022 [P] [US1] Unit test: Empty pattern returns unchanged in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs

### Implementation for User Story 1

- [ ] T023 [US1] Implement collectByIdentity function to traverse pattern and collect Subject occurrences by identity in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T024 [US1] Implement reconcileOccurrences for LastWriteWins policy (take last occurrence) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T025 [US1] Implement reconcileOccurrences for FirstWriteWins policy (take first occurrence) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T026 [US1] Implement rebuildPattern function to reconstruct pattern with canonical subjects, tracking visited identities in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T027 [US1] Implement reconcile main function orchestrating collect → reconcile → rebuild phases in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T028 [US1] Add Haddock documentation for reconcile function with examples in libs/pattern/src/Pattern/Reconcile.hs

**Checkpoint**: At this point, basic reconciliation (LastWriteWins, FirstWriteWins) should work and be testable independently

---

## Phase 4: User Story 2 - Merge Patterns from Multiple Sources (Priority: P2)

**Goal**: Enable merging patterns from multiple sources with configurable merge strategies

**Independent Test**: Create patterns from different sources with overlapping identities, combine into root pattern, reconcile with specific merge strategy, verify merge follows configured strategy

### Property Tests for User Story 2

- [ ] T029 [P] [US2] Property test: merge strategies correctly combine content (UnionLabels includes all labels) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T030 [P] [US2] Property test: ShallowMerge combines all top-level properties in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T031 [P] [US2] Property test: UnionElements deduplicates by identity in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs

### Unit Tests for User Story 2

- [ ] T032 [P] [US2] Unit test: UnionLabels combines all labels from all occurrences in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T033 [P] [US2] Unit test: IntersectLabels keeps only common labels in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T034 [P] [US2] Unit test: ReplaceLabels uses later labels in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T035 [P] [US2] Unit test: ShallowMerge merges top-level property keys in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T036 [P] [US2] Unit test: DeepMerge recursively merges nested maps in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T037 [P] [US2] Unit test: ReplaceProperties completely replaces properties in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T038 [P] [US2] Unit test: UnionElements deduplicates elements by identity in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T039 [P] [US2] Unit test: AppendElements concatenates all element lists in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T040 [P] [US2] Unit test: ReplaceElements uses later element list in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs

### Implementation for User Story 2

- [ ] T041 [US2] Implement mergeLabels function for UnionLabels strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T042 [US2] Implement mergeLabels function for IntersectLabels strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T043 [US2] Implement mergeLabels function for ReplaceLabels strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T044 [US2] Implement mergeProperties function for ShallowMerge strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T045 [US2] Implement mergeProperties function for DeepMerge strategy (recursive) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T046 [US2] Implement mergeProperties function for ReplaceProperties strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T047 [US2] Implement mergeElements function for UnionElements strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T048 [US2] Implement mergeElements function for AppendElements strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T049 [US2] Implement mergeElements function for ReplaceElements strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T050 [US2] Implement mergeSubjects function combining labels, properties, and elements per strategy in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T051 [US2] Implement reconcileOccurrences for Merge policy using mergeSubjects in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T052 [US2] Add Haddock documentation for merge strategy functions with examples in libs/pattern/src/Pattern/Reconcile.hs

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently - full merge functionality available

---

## Phase 5: User Story 3 - Validate Pattern Coherence (Priority: P3)

**Goal**: Enable strict validation and detailed conflict reporting

**Independent Test**: Create pattern with known conflicts, call reconcile with Strict policy, verify detailed conflict information returned

### Property Tests for User Story 3

- [ ] T053 [P] [US3] Property test: Strict mode is accurate (reports error iff duplicates with different content exist) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs

### Unit Tests for User Story 3

- [ ] T054 [P] [US3] Unit test: Strict mode with conflicts returns ReconcileError with conflict details in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T055 [P] [US3] Unit test: Strict mode with coherent pattern returns success in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T056 [P] [US3] Unit test: findConflicts returns all conflicts without reconciling in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T057 [P] [US3] Unit test: needsReconciliation returns true for duplicates, false otherwise in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs

### Implementation for User Story 3

- [ ] T058 [US3] Implement subjectsDiffer function to compare two Subjects for content equality in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T059 [US3] Implement buildConflict function to create Conflict from duplicate occurrences in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T060 [US3] Implement reconcileOccurrences for Strict policy (detect conflicts, fail if found) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T061 [US3] Implement findConflicts function to extract conflicts without reconciling in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T062 [US3] Implement needsReconciliation function to check for duplicate identities in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T063 [US3] Add Haddock documentation for validation functions with examples in libs/pattern/src/Pattern/Reconcile.hs

**Checkpoint**: All core reconciliation policies now work - validation and diagnostics available

---

## Phase 6: User Story 4 - Complete Partial References (Priority: P3)

**Goal**: Enable automatic reference completion by replacing atomic patterns with full definitions

**Independent Test**: Create pattern with atomic subject and full definition elsewhere, reconcile, verify atomic reference replaced with full definition

### Property Tests for User Story 4

- [ ] T064 [P] [US4] Property test: reference resolution is complete (all atomic patterns with full definitions replaced) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs

### Unit Tests for User Story 4

- [ ] T065 [P] [US4] Unit test: atomic reference replaced with full definition in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T066 [P] [US4] Unit test: all atomic occurrences preserved (no full definition) in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T067 [P] [US4] Unit test: circular references resolved (A→B, B→A both appear once) in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T068 [P] [US4] Unit test: self-referential pattern handled (subject contains itself) in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T069 [P] [US4] Unit test: orphan references preserved as-is in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs

### Implementation for User Story 4

- [ ] T070 [US4] Implement isRefinementOf function to check if one Subject refines another in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T071 [US4] Implement isReference function to detect atomic patterns with full definitions elsewhere in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T072 [US4] Enhance rebuildPattern to detect and complete references using canonical map in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T073 [US4] Add cycle detection to rebuildPattern using visited set in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T074 [US4] Add Haddock documentation for reference functions with examples in libs/pattern/src/Pattern/Reconcile.hs

**Checkpoint**: Reference completion works - all user stories 1-4 independently functional

---

## Phase 7: User Story 5 - Track Reconciliation Actions (Priority: P4)

**Goal**: Enable detailed reporting of reconciliation actions for debugging and monitoring

**Independent Test**: Reconcile pattern with known characteristics (3 duplicates, 2 references), verify report contains accurate counts

### Unit Tests for User Story 5

- [ ] T075 [P] [US5] Unit test: reconcileWithReport returns correct duplicatesFound count in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T076 [P] [US5] Unit test: reconcileWithReport returns correct referencesResolved count in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T077 [P] [US5] Unit test: reconcileWithReport returns correct mergesPerformed count in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs
- [ ] T078 [P] [US5] Unit test: reconcileWithReport returns correct subjectCounts map in libs/pattern/tests/Spec/Pattern/ReconcileSpec.hs

### Implementation for User Story 5

- [ ] T079 [US5] Implement ReconcileReport accumulation in collectByIdentity (count occurrences) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T080 [US5] Implement ReconcileReport accumulation in reconcileOccurrences (count merges) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T081 [US5] Implement ReconcileReport accumulation in rebuildPattern (count references resolved) in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T082 [US5] Implement reconcileWithReport function orchestrating reconcile with report accumulation in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T083 [US5] Add Haddock documentation for reconcileWithReport with examples in libs/pattern/src/Pattern/Reconcile.hs

**Checkpoint**: All user stories complete - full reconciliation feature with reporting available

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Performance, optimization, and comprehensive testing

- [ ] T084 [P] Add comprehensive Haddock module documentation with overview and usage guide in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T085 [P] Add inline documentation for internal helper functions in libs/pattern/src/Pattern/Reconcile.hs
- [ ] T086 [P] Create benchmark tests for 10,000 subject reconciliation (<100ms target) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T087 [P] Create benchmark tests for deep nesting (100+ levels) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T088 [P] Add property test for handling large patterns (memory usage) in libs/pattern/tests/Spec/Pattern/ReconcileProperties.hs
- [ ] T089 Verify all tests pass with cabal test from libs/pattern/
- [ ] T090 Run quickstart.md validation - test all examples work as documented
- [ ] T091 Update CHANGELOG.md with new Pattern.Reconcile module for v0.4.0.0
- [ ] T092 Verify pattern.cabal version bump to 0.4.0.0

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phases 3-7)**: All depend on Foundational phase completion
  - User Story 1 (P1): Can start after Foundational - No dependencies on other stories
  - User Story 2 (P2): Depends on User Story 1 (builds on basic reconciliation)
  - User Story 3 (P3): Can start after Foundational - Independent of US1/US2
  - User Story 4 (P3): Depends on User Story 1 (extends rebuildPattern)
  - User Story 5 (P4): Depends on all previous stories (adds reporting to all operations)
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Foundation only - implements core reconcile with LastWriteWins/FirstWriteWins
- **User Story 2 (P2)**: Depends on US1 - adds Merge policy extending reconcileOccurrences
- **User Story 3 (P3)**: Foundation only - implements Strict policy and validation (independent)
- **User Story 4 (P3)**: Depends on US1 - enhances rebuildPattern with reference completion
- **User Story 5 (P4)**: Depends on US1-4 - adds reporting to all reconciliation operations

### Within Each User Story

- Property/unit tests MUST be written and FAIL before implementation
- Core algorithms before utilities
- Internal helpers before public API
- Documentation after implementation

### Parallel Opportunities

- Phase 1 Setup: All tasks can run in parallel (T001-T006)
- Phase 2 Foundational: T007-T012 (type definitions) can run in parallel
- Within each user story:
  - All property tests marked [P] can run in parallel
  - All unit tests marked [P] can run in parallel
  - Implementation tasks must be sequential (dependencies)
- Phase 8 Polish: T084-T088 can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all property tests for User Story 1 together:
Task: "Property test: idempotence in ReconcileProperties.hs"
Task: "Property test: identity preservation in ReconcileProperties.hs"
Task: "Property test: deterministic results in ReconcileProperties.hs"
Task: "QuickCheck generator for Pattern Subject"

# Launch all unit tests for User Story 1 together:
Task: "Unit test: LastWriteWins with duplicate properties"
Task: "Unit test: FirstWriteWins with duplicate properties"
Task: "Unit test: No duplicates returns unchanged"
Task: "Unit test: Empty pattern returns unchanged"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T006)
2. Complete Phase 2: Foundational (T007-T014) - CRITICAL
3. Complete Phase 3: User Story 1 (T015-T028)
4. **STOP and VALIDATE**: Run tests, verify basic reconciliation works
5. Deploy/demo if ready - MVP complete!

### Incremental Delivery

1. Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → MVP deployed!
3. Add User Story 2 → Test independently → Merge capability added
4. Add User Story 3 → Test independently → Validation added
5. Add User Story 4 → Test independently → Reference completion added
6. Add User Story 5 → Test independently → Reporting added
7. Polish → Performance validated

Each story adds value without breaking previous stories.

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational done:
   - Developer A: User Story 1 (core reconciliation)
   - Developer B: User Story 3 (validation - independent of US1)
3. After US1 complete:
   - Developer A: User Story 2 (merge - extends US1)
   - Developer C: User Story 4 (references - extends US1)
4. After US1-4 complete:
   - Any developer: User Story 5 (reporting)

---

## Notes

- [P] tasks = different files/sections, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Tests written first ensure TDD discipline
- Performance benchmarks in Polish phase verify <100ms for 10k subjects
- All Haddock documentation required for public API
- Version bump to 0.4.0.0 (minor version for new feature)
