# Tasks: Applicative Instance for Pattern

**Input**: Design documents from `/specs/013-applicative-instance/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are REQUIRED for this feature - property-based tests for Applicative laws and unit tests for edge cases.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Paths: `src/Pattern/Core.hs`, `tests/Spec/Pattern/CoreSpec.hs`, `tests/Spec/Pattern/Properties.hs`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and verification of prerequisites

**Note**: This is a minimal setup phase since the Pattern type and Functor instance already exist. We're adding a new typeclass instance to existing code.

- [x] T001 Verify Pattern data type exists in src/Pattern/Core.hs
- [x] T002 Verify Functor instance exists in src/Pattern/Core.hs
- [x] T003 Verify test infrastructure (Hspec, QuickCheck) is configured in pattern.cabal
- [x] T004 Verify existing test files tests/Spec/Pattern/CoreSpec.hs and tests/Spec/Pattern/Properties.hs exist

---

## Phase 2: User Story 1 - Apply Functions to Pattern Values (Priority: P1) 🎯 MVP

**Goal**: Implement the core Applicative instance (`pure` and `<*>`) that enables applying functions stored in patterns to values stored in patterns using structure-preserving/zip-like semantics.

**Independent Test**: Can be fully tested by creating a pattern containing functions and a pattern containing values, applying the functions to the values using `<*>`, and verifying that: (1) functions are applied correctly according to the applicative semantics, (2) the result pattern structure matches expectations, and (3) the operation works for atomic patterns, patterns with elements, and nested patterns.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total. See Testing Performance Guidelines section below.

- [x] T005 [P] [US1] Add unit test for `pure` with integer value in tests/Spec/Pattern/CoreSpec.hs
- [x] T006 [P] [US1] Add unit test for `pure` with string value in tests/Spec/Pattern/CoreSpec.hs
- [x] T007 [P] [US1] Add unit test for `pure` with function value in tests/Spec/Pattern/CoreSpec.hs
- [x] T008 [P] [US1] Add unit test for `<*>` with atomic patterns (function and value) in tests/Spec/Pattern/CoreSpec.hs
- [x] T009 [P] [US1] Add unit test for `<*>` with patterns having multiple elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T010 [P] [US1] Add unit test for `<*>` with nested patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T011 [P] [US1] Add unit test for `<*>` with `pure` function and pattern value in tests/Spec/Pattern/CoreSpec.hs
- [x] T012 [P] [US1] Add unit test for `<*>` with pattern function and `pure` value in tests/Spec/Pattern/CoreSpec.hs
- [x] T013 [US1] Run tests to verify they fail: `timeout 60 cabal test` in project root

### Implementation for User Story 1

- [x] T014 [US1] Implement `pure :: a -> Pattern a` function in src/Pattern/Core.hs (creates atomic pattern with empty elements list)
- [x] T015 [US1] Implement `<*>` operator with structure-preserving/zip-like semantics in src/Pattern/Core.hs (applies root function to root value, recursively applies element functions to element values)
- [x] T016 [US1] Add Haddock documentation for `pure` function in src/Pattern/Core.hs (explain atomic pattern creation, include examples)
- [x] T017 [US1] Add Haddock documentation for `<*>` operator in src/Pattern/Core.hs (explain structure-preserving semantics, zip-like truncation, include examples)
- [x] T018 [US1] Add module-level documentation for Applicative instance in src/Pattern/Core.hs (categorical interpretation, relationship to Functor)
- [x] T019 [US1] Run tests to verify implementation: `timeout 60 cabal test` in project root
- [x] T020 [US1] Verify all User Story 1 tests pass independently

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. The core Applicative instance (`pure` and `<*>`) should work for atomic patterns, patterns with elements, and nested patterns.

### Git Commit for User Story 1

- [x] T021 [US1] Commit User Story 1: `git add -A && git commit -m "feat: implement Applicative instance for Pattern (User Story 1)

- Implement pure function to wrap values in atomic patterns
- Implement <*> operator with structure-preserving/zip-like semantics
- Add comprehensive Haddock documentation
- Add unit tests for pure and <*> operations
- Support atomic patterns, patterns with elements, and nested patterns"`

---

## Phase 3: User Story 2 - Verify Applicative Laws (Priority: P1)

**Goal**: Verify that the Applicative instance satisfies all Applicative laws (identity, composition, homomorphism, interchange) through property-based testing.

**Independent Test**: Can be fully tested by verifying that: (1) identity law holds (applying `pure id` to a pattern produces the same pattern), (2) composition law holds (applying composed functions produces the same result as applying functions sequentially), (3) homomorphism law holds (applying a pure function to a pure value equals pure application), and (4) interchange law holds (applying a function pattern to a pure value equals applying a pure function to the value pattern).

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation (if laws are not yet satisfied)**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [x] T022 [P] [US2] Add property-based test for identity law (`pure id <*> v = v`) in tests/Spec/Pattern/Properties.hs
- [x] T023 [P] [US2] Add property-based test for composition law (`pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`) in tests/Spec/Pattern/Properties.hs
- [x] T024 [P] [US2] Add property-based test for homomorphism law (`pure f <*> pure x = pure (f x)`) in tests/Spec/Pattern/Properties.hs
- [x] T025 [P] [US2] Add property-based test for interchange law (`u <*> pure y = pure ($ y) <*> u`) in tests/Spec/Pattern/Properties.hs
- [x] T026 [US2] Add Arbitrary instance for Pattern (a -> b) if needed for function pattern generation in tests/Spec/Pattern/Properties.hs
- [x] T027 [US2] Run property-based tests to verify laws: `timeout 60 cabal test` in project root

### Implementation for User Story 2

**Note**: If the implementation from User Story 1 already satisfies the laws, these tests should pass. If not, fix the implementation.

- [x] T028 [US2] Verify identity law holds for all pattern structures (atomic, with elements, nested)
- [x] T029 [US2] Verify composition law holds for all pattern structures
- [x] T030 [US2] Verify homomorphism law holds for all pattern structures
- [x] T031 [US2] Verify interchange law holds for all pattern structures
- [x] T032 [US2] Fix any law violations in src/Pattern/Core.hs if tests fail
- [x] T033 [US2] Run all property-based tests: `timeout 60 cabal test` in project root
- [x] T034 [US2] Verify all User Story 2 tests pass independently

**Checkpoint**: At this point, all Applicative laws should be verified through property-based testing. The implementation should satisfy identity, composition, homomorphism, and interchange laws for all pattern structures.

### Git Commit for User Story 2

- [x] T035 [US2] Commit User Story 2: `git add -A && git commit -m "test: add property-based tests for Applicative laws (User Story 2)

- Add property-based tests for identity law
- Add property-based tests for composition law
- Add property-based tests for homomorphism law
- Add property-based tests for interchange law
- Verify all laws hold for atomic, element, and nested patterns
- Fix any law violations in Applicative instance implementation"`

---

## Phase 4: User Story 3 - Verify Consistency with Functor (Priority: P1)

**Goal**: Verify that Applicative operations are consistent with Functor operations, ensuring that `fmap f x = pure f <*> x` holds for all functions and patterns.

**Independent Test**: Can be fully tested by verifying that for any function f and pattern x, applying `fmap f x` produces the same result as applying `pure f <*> x`.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation (if consistency is not yet satisfied)**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [x] T036 [P] [US3] Add property-based test for Functor consistency (`fmap f x = pure f <*> x`) in tests/Spec/Pattern/Properties.hs
- [x] T037 [P] [US3] Add unit test for Functor consistency with atomic patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T038 [P] [US3] Add unit test for Functor consistency with patterns having elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T039 [P] [US3] Add unit test for Functor consistency with nested patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T040 [P] [US3] Add unit test for Functor consistency with type transformations (e.g., String -> Int) in tests/Spec/Pattern/CoreSpec.hs
- [x] T041 [US3] Run consistency tests to verify: `timeout 60 cabal test` in project root

### Implementation for User Story 3

**Note**: If the implementation from User Story 1 already satisfies consistency, these tests should pass. If not, fix the implementation.

- [x] T042 [US3] Verify consistency holds for all pattern structures (atomic, with elements, nested)
- [x] T043 [US3] Verify consistency holds for all value type transformations
- [x] T044 [US3] Fix any consistency violations in src/Pattern/Core.hs if tests fail
- [x] T045 [US3] Add documentation note about Functor consistency in src/Pattern/Core.hs
- [x] T046 [US3] Run all consistency tests: `timeout 60 cabal test` in project root
- [x] T047 [US3] Verify all User Story 3 tests pass independently

**Checkpoint**: At this point, Applicative operations should be consistent with Functor operations. The relationship `fmap f x = pure f <*> x` should hold for all functions and patterns.

### Git Commit for User Story 3

- [x] T048 [US3] Commit User Story 3: `git add -A && git commit -m "test: add tests for Functor consistency (User Story 3)

- Add property-based tests for fmap f x = pure f <*> x
- Add unit tests for consistency across pattern structures
- Add unit tests for consistency with type transformations
- Verify consistency holds for all pattern types
- Document Functor consistency in Applicative instance"`

---

## Phase 5: User Story 4 - Handle Edge Cases in Applicative Operations (Priority: P2)

**Goal**: Ensure applicative operations handle edge cases correctly (empty patterns, mismatched structures, nested patterns) so that applicative operations work reliably with any pattern structure.

**Independent Test**: Can be fully tested by creating patterns with various edge case configurations (empty elements, mismatched element counts, deep nesting, different value types) and verifying that applicative operations handle them correctly according to the defined semantics.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation (if edge cases are not yet handled)**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [x] T049 [P] [US4] Add unit test for `<*>` with patterns having empty elements lists in tests/Spec/Pattern/CoreSpec.hs
- [x] T050 [P] [US4] Add unit test for `<*>` with mismatched element counts (function pattern has fewer elements) in tests/Spec/Pattern/CoreSpec.hs
- [x] T051 [P] [US4] Add unit test for `<*>` with mismatched element counts (value pattern has fewer elements) in tests/Spec/Pattern/CoreSpec.hs
- [x] T052 [P] [US4] Add unit test for `<*>` with deeply nested patterns (10+ levels) in tests/Spec/Pattern/CoreSpec.hs
- [x] T053 [P] [US4] Add unit test for `<*>` with atomic function pattern and pattern with multiple elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T054 [P] [US4] Add unit test for `<*>` with pattern with multiple function elements and atomic value pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T055 [P] [US4] Add unit test for `pure` with different value types (strings, integers, custom types) in tests/Spec/Pattern/CoreSpec.hs
- [x] T056 [US4] Run edge case tests to verify: `timeout 60 cabal test` in project root

### Implementation for User Story 4

**Note**: The implementation from User Story 1 should already handle these edge cases through zip-like truncation. These tests verify the behavior is correct.

- [x] T057 [US4] Verify zip-like truncation works correctly for mismatched element counts
- [x] T058 [US4] Verify deeply nested patterns work correctly (recursive application at all levels)
- [x] T059 [US4] Verify atomic patterns work correctly with patterns having elements
- [x] T060 [US4] Fix any edge case handling issues in src/Pattern/Core.hs if tests fail
- [x] T061 [US4] Add documentation for edge case behavior (zip-like truncation) in src/Pattern/Core.hs
- [x] T062 [US4] Run all edge case tests: `timeout 60 cabal test` in project root
- [x] T063 [US4] Verify all User Story 4 tests pass independently

**Checkpoint**: At this point, all edge cases should be handled correctly. Applicative operations should work reliably with empty patterns, mismatched structures, deeply nested patterns, and various value types.

### Git Commit for User Story 4

- [x] T064 [US4] Commit User Story 4: `git add -A && git commit -m "test: add edge case tests for Applicative operations (User Story 4)

- Add tests for empty elements lists
- Add tests for mismatched element counts (zip-like truncation)
- Add tests for deeply nested patterns (10+ levels)
- Add tests for atomic patterns with multi-element patterns
- Add tests for various value types
- Verify zip-like truncation behavior is correct
- Document edge case handling in Applicative instance"`

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final improvements, documentation updates, and validation

- [x] T065 [P] Update module exports in src/Pattern.hs to re-export Applicative instance
- [x] T066 [P] Review and update quickstart.md examples if needed in specs/013-applicative-instance/quickstart.md
- [x] T067 [P] Review and update data-model.md if implementation details changed in specs/013-applicative-instance/data-model.md
- [x] T068 [P] Review and update contracts/type-signatures.md if API changed in specs/013-applicative-instance/contracts/type-signatures.md
- [x] T069 Run full test suite: `timeout 60 cabal test` in project root
- [x] T070 Verify all tests pass (unit tests, property-based tests, edge case tests)
- [x] T071 Verify code compiles without warnings: `cabal build` in project root
- [x] T072 Run Haddock documentation generation: `cabal haddock` in project root
- [x] T073 Verify Haddock documentation is complete and accurate

**Checkpoint**: All polish tasks complete, documentation updated, all tests passing, code compiles cleanly.

### Git Commit for Polish

- [x] T074 Commit polish: `git add -A && git commit -m "docs: polish Applicative instance implementation

- Update module exports
- Review and update documentation
- Verify all tests pass
- Verify code compiles without warnings
- Complete Haddock documentation"`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **User Story 1 (Phase 2)**: Depends on Setup completion - MVP implementation
- **User Story 2 (Phase 3)**: Depends on User Story 1 completion - verifies laws
- **User Story 3 (Phase 4)**: Depends on User Story 1 completion - verifies consistency
- **User Story 4 (Phase 5)**: Depends on User Story 1 completion - verifies edge cases
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Setup (Phase 1) - No dependencies on other stories - **MVP**
- **User Story 2 (P1)**: Depends on User Story 1 - Verifies Applicative laws
- **User Story 3 (P1)**: Depends on User Story 1 - Verifies Functor consistency
- **User Story 4 (P2)**: Depends on User Story 1 - Verifies edge case handling

**Note**: User Stories 2, 3, and 4 can be worked on in parallel after User Story 1 is complete, as they are independent verification tasks.

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Implementation follows tests
- Documentation added after implementation
- Story complete before moving to next priority
- Git commit after each user story

### Parallel Opportunities

- All Setup tasks can run in parallel (T001-T004)
- All test tasks within a user story marked [P] can run in parallel
- User Stories 2, 3, and 4 can be worked on in parallel after User Story 1 completes
- Polish tasks marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Add unit test for pure with integer value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for pure with string value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for pure with function value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for <*> with atomic patterns in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for <*> with patterns having multiple elements in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for <*> with nested patterns in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for <*> with pure function and pattern value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for <*> with pattern function and pure value in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: User Story 1 (core Applicative instance)
3. **STOP and VALIDATE**: Test User Story 1 independently
4. Commit User Story 1
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup → Foundation ready
2. Add User Story 1 → Test independently → Commit → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Commit → Deploy/Demo
4. Add User Story 3 → Test independently → Commit → Deploy/Demo
5. Add User Story 4 → Test independently → Commit → Deploy/Demo
6. Polish → Commit → Final release
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup together
2. Once Setup is done:
   - Developer A: User Story 1 (MVP)
3. Once User Story 1 is done:
   - Developer A: User Story 2 (Applicative laws)
   - Developer B: User Story 3 (Functor consistency)
   - Developer C: User Story 4 (Edge cases)
4. Stories complete and integrate independently
5. Team completes Polish together

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each user story (T021, T035, T048, T064)
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- All tests should use timeouts: `timeout 60 cabal test`
- Property-based tests should use bounded generators to ensure fast execution

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

### Troubleshooting Slow Tests

If tests hang or take too long:

1. **Check for infinite recursion**: Verify recursive functions have proper base cases
2. **Check for ambiguous function calls**: Use explicit module qualifiers (e.g., `Prelude.foldl` vs `foldl`)
3. **Check test data size**: Ensure property-based tests use bounded generators
4. **Check for lazy evaluation issues**: Ensure strict evaluation where needed
5. **Verify test isolation**: Ensure tests don't depend on shared mutable state

### Test Execution Commands

```bash
# First run after implementation (with timeout):
timeout 60 cabal test

# Normal verification (with timeout):
timeout 30 cabal test

# Examples by language/framework:
# Haskell/Cabal: timeout 60 cabal test
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Use profiling tools if tests consistently slow

---

## Summary

- **Total Tasks**: 74
- **Tasks per User Story**:
  - User Story 1: 21 tasks (T005-T021)
  - User Story 2: 14 tasks (T022-T035)
  - User Story 3: 13 tasks (T036-T048)
  - User Story 4: 16 tasks (T049-T064)
  - Polish: 10 tasks (T065-T074)
- **Parallel Opportunities**: All test tasks within a story, User Stories 2-4 after User Story 1
- **Independent Test Criteria**: Each user story has clear independent test criteria
- **Suggested MVP Scope**: User Story 1 only (core Applicative instance)
- **Git Commits**: After each user story (T021, T035, T048, T064) and after polish (T074)

