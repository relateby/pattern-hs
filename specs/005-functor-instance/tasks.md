# Tasks: Functor Instance for Pattern

**Input**: Design documents from `/specs/005-functor-instance/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are included as they are essential for verifying functor laws and value transformation correctness.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Paths shown below assume single project structure

---

## Phase 1: User Story 1 - Transform Values in Patterns (Priority: P1) 🎯 MVP

**Goal**: Implement Functor instance that transforms values in patterns while preserving pattern structure (element count, nesting depth, element order). This delivers the ability to transform pattern values without manual reconstruction.

**Independent Test**: Apply a transformation function to a pattern and verify that: (1) all values are transformed correctly, (2) the pattern structure (element count, nesting, order) remains unchanged, and (3) the transformation works for atomic patterns, patterns with elements, and nested patterns.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T001 [P] [US1] Write unit test for transforming atomic pattern with string value in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T002 [P] [US1] Write unit test for transforming pattern with multiple elements in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T003 [P] [US1] Write unit test for transforming nested pattern structure in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T004 [P] [US1] Write unit test for transforming pattern with integer values in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T005 [P] [US1] Write unit test for transforming pattern with custom type values in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T006 [P] [US1] Write unit test for structure preservation (element count) in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T007 [P] [US1] Write unit test for structure preservation (element order) in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T008 [P] [US1] Write unit test for type transformation (String to Int) in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 1

- [x] T009 [US1] Implement Functor instance for Pattern in `src/Pattern/Core.hs` with recursive fmap implementation
- [x] T010 [US1] Add Haddock documentation for Functor instance in `src/Pattern/Core.hs` explaining structure preservation
- [x] T011 [US1] Add Haddock examples for fmap usage in `src/Pattern/Core.hs` demonstrating value transformation

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. All tests should pass, demonstrating value transformation while preserving structure.

---

## Phase 2: User Story 2 - Verify Functor Laws (Priority: P1)

**Goal**: Verify that the Functor instance satisfies functor laws (identity and composition) through property-based testing. This delivers mathematical correctness guarantees for pattern transformations.

**Independent Test**: Verify that: (1) applying the identity function to a pattern produces the same pattern, and (2) applying a composition of two functions produces the same result as applying each function sequentially.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T012 [P] [US2] Write property-based test for functor identity law (`fmap id = id`) in `tests/Spec/Pattern/Properties.hs`
- [x] T013 [P] [US2] Write property-based test for functor composition law (`fmap (f . g) = fmap f . fmap g`) in `tests/Spec/Pattern/Properties.hs`
- [x] T014 [P] [US2] Write unit test for identity law with atomic pattern in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T015 [P] [US2] Write unit test for identity law with pattern with elements in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T016 [P] [US2] Write unit test for composition law with two transformation functions in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T017 [P] [US2] Write unit test for composition law with nested patterns in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 2

- [x] T018 [US2] Verify Functor instance implementation satisfies identity law (no code changes if already correct)
- [x] T019 [US2] Verify Functor instance implementation satisfies composition law (no code changes if already correct)
- [x] T020 [US2] Add Haddock documentation for functor laws in `src/Pattern/Core.hs` with formal statements

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Functor laws should be verified through property-based tests.

---

## Phase 3: User Story 3 - Transform Nested Patterns (Priority: P2)

**Goal**: Explicitly test and verify that nested pattern transformation works correctly at all nesting levels. This provides confidence when working with complex pattern structures that may have arbitrary nesting depth.

**Independent Test**: Create patterns with multiple levels of nesting, apply transformations, and verify that values at all levels are transformed while structure is preserved.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T021 [P] [US3] Write unit test for transforming pattern with 3+ levels of nesting in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T022 [P] [US3] Write unit test for transforming pattern with varying nesting depths in different branches in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T023 [P] [US3] Write unit test for transforming pattern with mixed structures at different levels in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T024 [P] [US3] Write unit test verifying all nesting levels are transformed correctly in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 3

- [x] T025 [US3] Verify recursive fmap implementation handles deep nesting correctly (no code changes if already correct)
- [x] T026 [US3] Add Haddock documentation examples for nested pattern transformation in `src/Pattern/Core.hs`

**Checkpoint**: At this point, all user stories should be independently functional. Nested pattern transformation should work correctly at all depths.

---

## Phase 4: Edge Cases & Comprehensive Testing

**Goal**: Ensure the Functor instance handles all edge cases correctly, providing comprehensive coverage for atomic patterns, empty elements, singular patterns, pair patterns, extended patterns, and various value types.

**Independent Test**: Test edge cases independently to verify correct behavior for all pattern structures and value types.

### Tests for Edge Cases

- [x] T027 [P] Write unit test for transforming atomic pattern (no elements) in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T028 [P] Write unit test for transforming pattern with empty elements list in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T029 [P] Write unit test for transforming singular pattern (one element) in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T030 [P] Write unit test for transforming pair pattern (two elements) in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T031 [P] Write unit test for transforming extended pattern (many elements) in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T032 [P] Write unit test for transforming patterns with different value types (strings, integers, custom types) in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T033 [P] Write unit test for type transformation edge cases in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for Edge Cases

- [x] T034 Verify all edge cases are handled correctly by existing Functor instance (no code changes expected)
- [x] T035 Add Haddock documentation examples for edge cases in `src/Pattern/Core.hs`

**Checkpoint**: All edge cases should be tested and verified. The Functor instance should handle all pattern structures correctly.

---

## Phase 5: Polish & Cross-Cutting Concerns

**Purpose**: Final documentation, examples, validation, and cross-cutting improvements

- [x] T036 [P] Update module-level Haddock documentation in `src/Pattern/Core.hs` to mention Functor instance
- [x] T037 [P] Add Functor instance to main Pattern module exports in `src/Pattern.hs` (if needed)
- [x] T038 [P] Verify all examples in `specs/005-functor-instance/quickstart.md` work correctly
- [x] T039 [P] Run all tests and verify 100% pass rate for functor instance tests
- [x] T040 [P] Verify functor laws hold for all test cases (property-based tests should pass)
- [x] T041 Review and update any related documentation that references Pattern typeclass instances
- [x] T042 Code review: verify Functor instance follows Haskell best practices
- [x] T043 Final validation: run full test suite and verify no regressions

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 1 (Phase 1)**: No dependencies - Pattern data type, Eq, and Show instances already exist (Features 1-2)
- **User Story 2 (Phase 2)**: Depends on User Story 1 completion - functor laws verification requires working Functor instance
- **User Story 3 (Phase 3)**: Depends on User Story 1 completion - nested transformation testing requires working Functor instance
- **Edge Cases (Phase 4)**: Depends on User Story 1 completion - edge case testing requires working Functor instance
- **Polish (Phase 5)**: Depends on all previous phases completion

### User Story Dependencies

- **User Story 1 (P1)**: Can start immediately - no dependencies on other stories
- **User Story 2 (P1)**: Depends on User Story 1 - requires Functor instance to verify laws
- **User Story 3 (P2)**: Depends on User Story 1 - requires Functor instance to test nested transformation

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Implementation follows test writing
- Documentation added after implementation
- Story complete before moving to next priority

### Parallel Opportunities

- All test tasks marked [P] within a user story can run in parallel (different test cases, same file)
- User Stories 2 and 3 can be worked on in parallel after User Story 1 completes (different test files)
- Edge case tests can run in parallel (different test cases, same file)
- Polish tasks marked [P] can run in parallel (different files, no dependencies)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Write unit test for transforming atomic pattern with string value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for transforming pattern with multiple elements in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for transforming nested pattern structure in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for transforming pattern with integer values in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for transforming pattern with custom type values in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for structure preservation (element count) in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for structure preservation (element order) in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for type transformation (String to Int) in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: User Story 1 (Transform Values in Patterns)
   - Write all tests (T001-T008)
   - Implement Functor instance (T009)
   - Add documentation (T010-T011)
2. **STOP and VALIDATE**: Test User Story 1 independently
3. Verify all tests pass
4. Deploy/demo if ready

### Incremental Delivery

1. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
2. Add User Story 2 → Test independently → Deploy/Demo (Functor laws verified)
3. Add User Story 3 → Test independently → Deploy/Demo (Nested transformation verified)
4. Add Edge Cases → Test independently → Deploy/Demo (Comprehensive coverage)
5. Polish → Final validation → Deploy/Demo (Production ready)

### Parallel Team Strategy

With multiple developers:

1. Developer A: User Story 1 (all tests + implementation)
2. Once User Story 1 is complete:
   - Developer A: User Story 2 (functor laws)
   - Developer B: User Story 3 (nested patterns)
3. Once User Stories 2-3 complete:
   - Developer A: Edge cases (comprehensive testing)
   - Developer B: Polish (documentation, validation)
4. Stories complete and integrate independently

---

## Notes

- [P] tasks = different test cases or different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- Functor instance implementation is straightforward: `fmap f (Pattern v es) = Pattern (f v) (map (fmap f) es)`
- Property-based tests require QuickCheck Arbitrary instances for Pattern (may need to create helper generators)

---

## Task Summary

- **Total Tasks**: 43
- **User Story 1 Tasks**: 11 (8 tests + 3 implementation)
- **User Story 2 Tasks**: 9 (6 tests + 3 verification)
- **User Story 3 Tasks**: 6 (4 tests + 2 verification)
- **Edge Cases Tasks**: 9 (7 tests + 2 verification)
- **Polish Tasks**: 8
- **Parallel Opportunities**: Many test tasks can run in parallel within each phase
- **Suggested MVP Scope**: User Story 1 only (11 tasks)

