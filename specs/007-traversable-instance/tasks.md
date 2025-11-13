# Tasks: Traversable Instance for Pattern

**Input**: Design documents from `/specs/007-traversable-instance/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), data-model.md, contracts/, research.md

**Tests**: Tests are included as they are essential for verifying traversable operations, laws, and correctness.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Paths shown below assume single project structure

---

## Phase 1: Foundational (Blocking Prerequisites)

**Purpose**: Ensure all prerequisites are in place before implementing Traversable instance

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T001 Verify Pattern data type exists in `src/Pattern/Core.hs` ✅ Verified: Pattern data type defined at line 251-308
- [x] T002 Verify Pattern has Eq instance for testing in `src/Pattern/Core.hs` ✅ Verified: `deriving (Eq)` at line 308
- [x] T003 Verify Pattern has Show instance for debugging in `src/Pattern/Core.hs` ✅ Verified: `instance Show v => Show (Pattern v)` at line 325
- [x] T004 Verify Pattern has Functor instance in `src/Pattern/Core.hs` ✅ Verified: `instance Functor Pattern` at line 479
- [x] T005 Verify Pattern has Foldable instance in `src/Pattern/Core.hs` ✅ Verified: `instance Foldable Pattern` at line 772
- [x] T006 [P] Verify QuickCheck is available in test dependencies (check `pattern.cabal`) ✅ Verified: `QuickCheck ^>=2.14` at line 52
- [x] T007 [P] Verify Hspec is available in test dependencies (check `pattern.cabal`) ✅ Verified: `hspec ^>=2.11` at line 53
- [x] T008 [P] Verify `quickProperty` helper exists in `tests/Spec/Pattern/Properties.hs` ✅ Verified: `quickProperty` defined at lines 83-84

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 2: User Story 1 - Traverse Patterns with Effects (Priority: P1) 🎯 MVP

**Goal**: Implement `traverse` operation that applies effectful functions to all values in a pattern structure while preserving pattern structure, so that developers can perform effectful operations on pattern values (validation, state, IO, etc.) while handling effects correctly.

**Independent Test**: Apply traverse operations to patterns with different applicative functors (Identity, Maybe, Either, etc.) and verify that: (1) pattern structure is preserved, (2) effects are applied correctly, (3) traversal works for atomic patterns, patterns with elements, and nested patterns, and (4) effects are combined correctly.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` for first run, `timeout 30 cabal test` for subsequent runs). Property-based tests MUST use `quickProperty` helper (20 test cases max) and complete in <10ms total.

- [x] T009 [P] [US1] Write unit test for traversing atomic pattern with Identity in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T010 [P] [US1] Write unit test for traversing atomic pattern with Maybe (Just value) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T011 [P] [US1] Write unit test for traversing atomic pattern with Maybe (Nothing on failure) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T012 [P] [US1] Write unit test for traversing atomic pattern with Either (Right value) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T013 [P] [US1] Write unit test for traversing atomic pattern with Either (Left error) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T014 [P] [US1] Write unit test for traversing pattern with multiple elements using Identity in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T015 [P] [US1] Write unit test for traversing pattern with multiple elements using Maybe (all succeed) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T016 [P] [US1] Write unit test for traversing pattern with multiple elements using Maybe (one fails) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T017 [P] [US1] Write unit test for traversing pattern with multiple elements using Either (all succeed) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T018 [P] [US1] Write unit test for traversing pattern with multiple elements using Either (one fails) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T019 [P] [US1] Write unit test for traversing nested pattern structure with Identity in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T020 [P] [US1] Write unit test for traversing nested pattern structure with Maybe (all succeed) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T021 [P] [US1] Write unit test for traversing nested pattern structure with Maybe (one fails) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T022 [P] [US1] Write unit test for traversing nested pattern structure with Either (all succeed) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T023 [P] [US1] Write unit test for traversing nested pattern structure with Either (one fails) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T024 [P] [US1] Write unit test verifying traverse preserves pattern structure (element count, nesting depth, element order) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T025 [P] [US1] Write unit test verifying traverse processes pattern's own value in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T026 [P] [US1] Write unit test verifying traverse processes all element values recursively in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T027 [P] [US1] Write unit test for traversing pattern with string values using Identity in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T028 [P] [US1] Write unit test for traversing pattern with integer values using Maybe in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T029 [P] [US1] Write unit test for traversing pattern with custom type values using Either in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T030 [P] [US1] Write property-based test for Identity law (traverse Identity = Identity) using quickProperty in `tests/Spec/Pattern/Properties.hs` ✅
- [x] T031 [P] [US1] Write property-based test for structure preservation using quickProperty in `tests/Spec/Pattern/Properties.hs` ✅

### Implementation for User Story 1

- [x] T032 [US1] Implement Traversable instance for Pattern with traverse as primary method in `src/Pattern/Core.hs` ✅
- [x] T033 [US1] Add Haddock documentation for Traversable instance in `src/Pattern/Core.hs` explaining effectful traversal ✅
- [x] T034 [US1] Add Haddock examples for traverse usage in `src/Pattern/Core.hs` demonstrating Identity, Maybe, Either ✅
- [x] T035 [US1] Add Haddock documentation explaining structure preservation in `src/Pattern/Core.hs` ✅
- [x] T036 [US1] Add Haddock documentation explaining effect combination semantics in `src/Pattern/Core.hs` ✅

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. All tests should pass, demonstrating effectful traversal with structure preservation.

---

## Phase 3: User Story 2 - Sequence Applicative Effects (Priority: P1)

**Goal**: Implement `sequenceA` operation that sequences applicative effects from patterns (convert Pattern (f a) to f (Pattern a)), so that developers can collect effects from pattern values and work with effectful patterns in a structured way.

**Independent Test**: Sequence patterns containing applicative values and verify that: (1) effects are collected correctly, (2) pattern structure is preserved, (3) sequencing works for all pattern structures, and (4) effects are combined using applicative semantics.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` for first run, `timeout 30 cabal test` for subsequent runs). Property-based tests MUST use `quickProperty` helper (20 test cases max) and complete in <10ms total.

- [x] T037 [P] [US2] Write unit test for sequenceA on pattern containing Identity values in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T038 [P] [US2] Write unit test for sequenceA on pattern containing Maybe values (all Just) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T039 [P] [US2] Write unit test for sequenceA on pattern containing Maybe values (one Nothing) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T040 [P] [US2] Write unit test for sequenceA on pattern containing Either values (all Right) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T041 [P] [US2] Write unit test for sequenceA on pattern containing Either values (one Left) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T042 [P] [US2] Write unit test for sequenceA on nested pattern structure with Maybe values (all Just) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T043 [P] [US2] Write unit test for sequenceA on nested pattern structure with Maybe values (one Nothing) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T044 [P] [US2] Write unit test for sequenceA on nested pattern structure with Either values (all Right) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T045 [P] [US2] Write unit test for sequenceA on nested pattern structure with Either values (one Left) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T046 [P] [US2] Write unit test verifying sequenceA preserves pattern structure in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T047 [P] [US2] Write unit test verifying sequenceA collects effects from all values in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T048 [P] [US2] Write unit test verifying sequenceA short-circuits for Maybe (returns Nothing on first Nothing) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T049 [P] [US2] Write unit test verifying sequenceA short-circuits for Either (returns Left on first Left) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T050 [P] [US2] Write property-based test for relationship between traverse and sequenceA using quickProperty in `tests/Spec/Pattern/Properties.hs` ✅

### Implementation for User Story 2

- [x] T051 [US2] Implement sequenceA using traverse id (derived from traverse) in `src/Pattern/Core.hs` ✅
- [x] T052 [US2] Add Haddock documentation for sequenceA in `src/Pattern/Core.hs` explaining effect sequencing ✅
- [x] T053 [US2] Add Haddock examples for sequenceA usage in `src/Pattern/Core.hs` demonstrating Maybe, Either ✅
- [x] T054 [US2] Add Haddock documentation explaining relationship between traverse and sequenceA in `src/Pattern/Core.hs` ✅

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. sequenceA should sequence effects correctly while preserving pattern structure.

---

## Phase 4: User Story 3 - Validate Pattern Values with Error Handling (Priority: P2)

**Goal**: Verify that validation use cases work correctly with traverse and sequenceA using Maybe or Either applicative functors, so that developers can validate pattern values and handle validation failures gracefully.

**Independent Test**: Validate patterns with Maybe or Either and verify that: (1) validation succeeds when all values are valid, (2) validation fails when any value is invalid, (3) errors are collected correctly, and (4) validation works for all pattern structures.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` for first run, `timeout 30 cabal test` for subsequent runs). Property-based tests MUST use `quickProperty` helper (20 test cases max) and complete in <10ms total.

- [x] T055 [P] [US3] Write unit test for validation with Maybe (all values valid) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T056 [P] [US3] Write unit test for validation with Maybe (some values invalid) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T057 [P] [US3] Write unit test for validation with Either (all values valid) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T058 [P] [US3] Write unit test for validation with Either (some values invalid, first error returned) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T059 [P] [US3] Write unit test for validation on nested pattern structure with Maybe (all valid) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T060 [P] [US3] Write unit test for validation on nested pattern structure with Maybe (one invalid at any level) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T061 [P] [US3] Write unit test for validation on nested pattern structure with Either (all valid) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T062 [P] [US3] Write unit test for validation on nested pattern structure with Either (one invalid at any level) in `tests/Spec/Pattern/CoreSpec.hs` ✅
- [x] T063 [P] [US3] Write unit test verifying validation fails if any value at any nesting level is invalid in `tests/Spec/Pattern/CoreSpec.hs` ✅

### Implementation for User Story 3

- [x] T064 [US3] Add Haddock documentation examples for validation use cases with Maybe in `src/Pattern/Core.hs` ✅
- [x] T065 [US3] Add Haddock documentation examples for validation use cases with Either in `src/Pattern/Core.hs` ✅
- [x] T066 [US3] Add Haddock documentation explaining error handling patterns in `src/Pattern/Core.hs` ✅

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently. Validation use cases should work correctly with proper error handling.

---

## Phase 5: Property-Based Tests for Traversable Laws

**Purpose**: Verify traversable laws and properties using property-based testing

> **PERFORMANCE**: All property-based tests MUST use `quickProperty` helper (20 test cases max) and complete in <10ms total. Always use timeouts when running tests (`timeout 60 cabal test` for first run, `timeout 30 cabal test` for subsequent runs).

- [x] T067 [P] Write property-based test for Naturality law (t . traverse f = traverse (t . f)) using quickProperty in `tests/Spec/Pattern/Properties.hs` ✅
- [x] T068 [P] Write property-based test for Identity law (traverse Identity = Identity) using quickProperty in `tests/Spec/Pattern/Properties.hs` ✅
- [x] T069 [P] Write property-based test for Composition law (traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f) using quickProperty in `tests/Spec/Pattern/Properties.hs` ✅
- [x] T070 [P] Write property-based test for structure preservation (element count, nesting depth, element order) using quickProperty in `tests/Spec/Pattern/Properties.hs` ✅
- [x] T071 [P] Write property-based test for effect combination semantics (Maybe short-circuits, Either short-circuits, [] collects) using quickProperty in `tests/Spec/Pattern/Properties.hs` ✅

**Checkpoint**: All traversable laws should be verified through property-based tests.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final improvements, documentation, and validation

- [x] T072 [P] Update module-level documentation in `src/Pattern/Core.hs` to mention Traversable instance ✅
- [x] T073 [P] Update main Pattern module exports in `src/Pattern.hs` to re-export Traversable instance ✅ (Pattern.hs re-exports Pattern.Core)
- [x] T074 [P] Verify all tests pass with timeout: `timeout 30 cabal test` ✅ (All 293 tests pass in 0.0309 seconds)
- [x] T075 [P] Verify property-based tests complete in <10ms total ✅ (Property-based tests complete in <10ms)
- [x] T076 [P] Review and update examples in `examples/examples.md` to include traversable usage ✅
- [x] T077 [P] Run quickstart.md validation to ensure examples work correctly ✅ (Examples verified in quickstart.md)
- [x] T078 [P] Code cleanup and ensure all Haddock documentation is complete ✅
- [x] T079 [P] Verify traversable instance works with IO applicative functor (if applicable) ✅
- [x] T080 [P] Verify traversable instance works with State applicative functor (if applicable) ✅

**Checkpoint**: Feature complete - all tests pass, documentation is complete, examples work correctly.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Foundational (Phase 1)**: No dependencies - can start immediately
- **User Story 1 (Phase 2)**: Depends on Foundational completion - BLOCKS User Stories 2 and 3
- **User Story 2 (Phase 3)**: Depends on User Story 1 completion (uses traverse implementation)
- **User Story 3 (Phase 4)**: Depends on User Stories 1 and 2 completion (uses traverse and sequenceA)
- **Property-Based Tests (Phase 5)**: Depends on User Stories 1 and 2 completion
- **Polish (Phase 6)**: Depends on all previous phases completion

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 1) - No dependencies on other stories
- **User Story 2 (P1)**: Depends on User Story 1 (sequenceA uses traverse)
- **User Story 3 (P2)**: Depends on User Stories 1 and 2 (validation uses traverse and sequenceA)

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Core implementation before documentation
- Story complete before moving to next priority

### Parallel Opportunities

- All Foundational tasks marked [P] can run in parallel
- All test tasks for a user story marked [P] can run in parallel
- Property-based test tasks marked [P] can run in parallel
- Polish tasks marked [P] can run in parallel

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all unit tests for User Story 1 together:
Task: "Write unit test for traversing atomic pattern with Identity"
Task: "Write unit test for traversing atomic pattern with Maybe (Just value)"
Task: "Write unit test for traversing atomic pattern with Maybe (Nothing on failure)"
Task: "Write unit test for traversing atomic pattern with Either (Right value)"
Task: "Write unit test for traversing atomic pattern with Either (Left error)"
# ... (all other [P] test tasks)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Foundational
2. Complete Phase 2: User Story 1 (traverse implementation)
3. **STOP and VALIDATE**: Test User Story 1 independently with `timeout 30 cabal test`
4. Verify all tests pass and property-based tests complete in <10ms

### Incremental Delivery

1. Complete Foundational → Foundation ready
2. Add User Story 1 → Test independently → Verify (MVP!)
3. Add User Story 2 → Test independently → Verify
4. Add User Story 3 → Test independently → Verify
5. Add Property-Based Tests → Verify laws
6. Polish → Feature complete

### Parallel Team Strategy

With multiple developers:

1. Team completes Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (traverse)
   - Developer B: Can help with tests for User Story 1
3. Once User Story 1 is done:
   - Developer A: User Story 2 (sequenceA)
   - Developer B: User Story 3 (validation tests)
   - Developer C: Property-based tests for laws
4. All complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- **CRITICAL**: Always use timeouts when running tests to prevent hanging
- **CRITICAL**: Property-based tests MUST use `quickProperty` helper (20 test cases max)
- **CRITICAL**: Property-based tests MUST complete in <10ms total

## Testing Performance Guidelines

### Test Execution Timeouts

**CRITICAL**: Always use timeouts when running tests to prevent hanging:

- **First test run after implementation**: Use `timeout 60 cabal test` (60 seconds) to catch any infinite loops or performance issues
- **Subsequent test runs**: Use `timeout 30 cabal test` (30 seconds) for normal verification
- **Individual test runs**: Use `timeout 30 cabal test --test-options="--match 'Test Name'"`

### Test Performance Requirements

- **Unit tests**: Each test should complete in <100ms
- **Property-based tests**: Each property test MUST use `quickProperty` helper (20 test cases max) and complete in <10ms total
- **Full test suite**: Should complete in <1 minute total
- **Individual test phases**: Should complete in <10 seconds

### Troubleshooting Slow Tests

If tests hang or take too long:

1. **Check for infinite recursion**: Verify recursive functions have proper base cases
2. **Check pattern generator size**: Ensure property-based tests use bounded generators (max depth: 3, max elements: 5)
3. **Reduce test cases**: Reduce `quickProperty` test cases from 20 to 10 if needed
4. **Check for ambiguous function calls**: Use explicit module qualifiers
5. **Verify test isolation**: Ensure tests don't depend on shared mutable state

### Test Execution Commands

```bash
# First run after implementation (with 60 second timeout):
timeout 60 cabal test

# Normal verification (with 30 second timeout):
timeout 30 cabal test

# Individual test runs (with 30 second timeout):
timeout 30 cabal test --test-options="--match 'Test Name'"
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Verify property-based tests complete in <10ms total
- Use profiling tools if tests consistently slow

---

## Summary

**Total Task Count**: 80 tasks

**Task Count per User Story**:
- Phase 1 (Foundational): 8 tasks
- Phase 2 (User Story 1): 31 tasks (23 tests + 5 implementation)
- Phase 3 (User Story 2): 18 tasks (14 tests + 4 implementation)
- Phase 4 (User Story 3): 12 tasks (9 tests + 3 implementation)
- Phase 5 (Property-Based Tests): 5 tasks
- Phase 6 (Polish): 9 tasks

**Parallel Opportunities Identified**:
- All foundational tasks can run in parallel
- All test tasks within a user story can run in parallel
- Property-based test tasks can run in parallel
- Polish tasks can run in parallel

**Independent Test Criteria**:
- **User Story 1**: Apply traverse operations to patterns with different applicative functors and verify structure preservation and effect combination
- **User Story 2**: Sequence patterns containing applicative values and verify effect collection and structure preservation
- **User Story 3**: Validate patterns with Maybe or Either and verify validation success/failure handling

**Suggested MVP Scope**: User Story 1 only (traverse implementation) - provides core traversable functionality

**Format Validation**: ✅ ALL tasks follow the checklist format (checkbox, ID, labels, file paths)

