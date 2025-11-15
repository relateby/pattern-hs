# Tasks: Comonad Instance for Pattern

**Input**: Design documents from `/specs/014-comonad-instance/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are REQUIRED for this feature - property-based tests for Comonad laws and unit tests for edge cases.

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

**Note**: This is a minimal setup phase since the Pattern type and related instances already exist. We're adding a new typeclass instance to existing code.

- [X] T001 Verify Pattern data type exists in src/Pattern/Core.hs
- [X] T002 Verify Functor instance exists in src/Pattern/Core.hs
- [X] T003 Verify Foldable instance exists in src/Pattern/Core.hs
- [X] T004 Verify Traversable instance exists in src/Pattern/Core.hs
- [X] T005 Verify basic query functions (size, depth) exist in src/Pattern/Core.hs
- [X] T006 Verify test infrastructure (Hspec, QuickCheck) is configured in pattern.cabal
- [X] T007 Verify existing test files tests/Spec/Pattern/CoreSpec.hs and tests/Spec/Pattern/Properties.hs exist
- [X] T008 Verify comonad package dependency is added to pattern.cabal (or add it if missing)

---

## Phase 2: User Story 1 - Extract Values from Patterns (Priority: P1) 🎯 MVP

**Goal**: Implement the `extract` function that extracts the decoration value from a pattern, providing the value at the focus point (root).

**Independent Test**: Can be fully tested by implementing `extract` that returns the pattern's decoration value, testing on atomic patterns, patterns with elements, and nested patterns. This delivers the ability to extract values from patterns in a comonadic context.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total. See Testing Performance Guidelines section below.

- [X] T009 [P] [US1] Add unit test for `extract` with atomic pattern (integer value) in tests/Spec/Pattern/CoreSpec.hs
- [X] T010 [P] [US1] Add unit test for `extract` with atomic pattern (string value) in tests/Spec/Pattern/CoreSpec.hs
- [X] T011 [P] [US1] Add unit test for `extract` with pattern with elements in tests/Spec/Pattern/CoreSpec.hs
- [X] T012 [P] [US1] Add unit test for `extract` with nested pattern structure in tests/Spec/Pattern/CoreSpec.hs
- [X] T013 [P] [US1] Add unit test for `extract` with different value types (String, Int, custom types) in tests/Spec/Pattern/CoreSpec.hs
- [X] T014 [US1] Run tests to verify they fail: `timeout 60 cabal test` in project root

### Implementation for User Story 1

- [X] T015 [US1] Implement `extract :: Pattern v -> v` function in src/Pattern/Core.hs (extracts decoration value from pattern)
- [X] T016 [US1] Add Haddock documentation for `extract` function in src/Pattern/Core.hs (explain decoration value extraction, include examples)
- [X] T017 [US1] Add module-level documentation for Comonad instance in src/Pattern/Core.hs (categorical interpretation, relationship to zippers)
- [X] T018 [US1] Run tests to verify implementation: `timeout 60 cabal test` in project root
- [X] T019 [US1] Verify all User Story 1 tests pass independently

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. The `extract` function should work for atomic patterns, patterns with elements, and nested patterns.

### Git Commit for User Story 1

- [ ] T020 [US1] Commit User Story 1: `git add -A && git commit -m "feat: implement extract function for Comonad instance (User Story 1)

- Implement extract function to extract decoration value from patterns
- Add comprehensive Haddock documentation
- Add unit tests for extract on atomic patterns, patterns with elements, and nested patterns
- Support all pattern structures and value types"`

---

## Phase 3: User Story 2 - Create Context-Aware Computations (Priority: P1)

**Goal**: Implement the `extend` function that applies a context-aware function to each position in a pattern, creating a new pattern where each position contains the result of applying the function to the pattern structure at that position.

**Independent Test**: Can be fully tested by implementing `extend` that applies a context-aware function to each position in a pattern, creating a new pattern where each position contains the result of applying the function to the pattern structure at that position. This delivers the ability to perform context-aware transformations.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [X] T021 [P] [US2] Add unit test for `extend` with depth computation function on atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [X] T022 [P] [US2] Add unit test for `extend` with depth computation function on pattern with elements in tests/Spec/Pattern/CoreSpec.hs
- [X] T023 [P] [US2] Add unit test for `extend` with size computation function on nested pattern in tests/Spec/Pattern/CoreSpec.hs
- [X] T024 [P] [US2] Add unit test for `extend` with custom context-aware function (e.g., length of values) in tests/Spec/Pattern/CoreSpec.hs
- [X] T025 [P] [US2] Add unit test for `extend` with function that transforms value types (Pattern Int -> String) in tests/Spec/Pattern/CoreSpec.hs
- [X] T026 [P] [US2] Add unit test for `extend` with nested pattern structure (verify recursive application) in tests/Spec/Pattern/CoreSpec.hs
- [X] T027 [US2] Run tests to verify they fail: `timeout 60 cabal test` in project root

### Implementation for User Story 2

- [X] T028 [US2] Implement `extend :: (Pattern v -> w) -> Pattern v -> Pattern w` function in src/Pattern/Core.hs (applies context-aware function recursively using `fmap f . duplicate`)
- [X] T029 [US2] Add Haddock documentation for `extend` function in src/Pattern/Core.hs (explain context-aware computation, include examples)
- [X] T030 [US2] Update module-level documentation for Comonad instance in src/Pattern/Core.hs (add extend operation details)
- [X] T031 [US2] Run tests to verify implementation: `timeout 60 cabal test` in project root
- [X] T032 [US2] Verify all User Story 2 tests pass independently

**Checkpoint**: At this point, User Story 2 should be fully functional and testable independently. The `extend` function should work for atomic patterns, patterns with elements, and nested patterns, applying context-aware functions recursively.

### Git Commit for User Story 2

- [ ] T033 [US2] Commit User Story 2: `git add -A && git commit -m "feat: implement extend function for Comonad instance (User Story 2)

- Implement extend function for context-aware computations
- Add comprehensive Haddock documentation
- Add unit tests for extend with various context-aware functions
- Support recursive application to all pattern structures"`

---

## Phase 4: User Story 3 - Create Patterns of Contexts (Priority: P1)

**Goal**: Implement the `duplicate` function that creates a pattern where each position contains the full pattern structure focused at that position, enabling context-aware computations.

**Independent Test**: Can be fully tested by implementing `duplicate` that creates a pattern where each position contains the pattern structure focused at that position, testing on atomic patterns, patterns with elements, and nested patterns. This delivers the ability to create context structures.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [X] T034 [P] [US3] Add unit test for `duplicate` with atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [X] T035 [P] [US3] Add unit test for `duplicate` with pattern with elements in tests/Spec/Pattern/CoreSpec.hs
- [X] T036 [P] [US3] Add unit test for `duplicate` with nested pattern structure in tests/Spec/Pattern/CoreSpec.hs
- [X] T037 [P] [US3] Add unit test for `extract . duplicate = id` (verifying context structure) in tests/Spec/Pattern/CoreSpec.hs
- [X] T038 [P] [US3] Add unit test for `duplicate` with deeply nested patterns (10+ levels) in tests/Spec/Pattern/CoreSpec.hs
- [X] T039 [US3] Run tests to verify they fail: `timeout 60 cabal test` in project root

### Implementation for User Story 3

- [X] T040 [US3] Implement `duplicate :: Pattern v -> Pattern (Pattern v)` function in src/Pattern/Core.hs (creates pattern of contexts recursively)
- [X] T041 [US3] Add Haddock documentation for `duplicate` function in src/Pattern/Core.hs (explain context creation, include examples)
- [X] T042 [US3] Update module-level documentation for Comonad instance in src/Pattern/Core.hs (add duplicate operation details)
- [X] T043 [US3] Run tests to verify implementation: `timeout 60 cabal test` in project root
- [X] T044 [US3] Verify all User Story 3 tests pass independently

**Checkpoint**: At this point, User Story 3 should be fully functional and testable independently. The `duplicate` function should work for atomic patterns, patterns with elements, and nested patterns, creating contexts recursively at all positions.

### Git Commit for User Story 3

- [ ] T045 [US3] Commit User Story 3: `git add -A && git commit -m "feat: implement duplicate function for Comonad instance (User Story 3)

- Implement duplicate function to create patterns of contexts
- Add comprehensive Haddock documentation
- Add unit tests for duplicate on all pattern structures
- Support recursive context creation at all positions"`

---

## Phase 5: User Story 4 - Verify Comonad Laws (Priority: P1)

**Goal**: Verify that the Comonad instance satisfies all Comonad laws (extract-extend, extend-extract, extend composition) through property-based testing.

**Independent Test**: Can be fully tested by verifying that: (1) extract-extend law holds (`extract . extend f = f`), (2) extend-extract law holds (`extend extract = id`), and (3) extend composition law holds (`extend f . extend g = extend (f . extend g)`). This delivers mathematical correctness guarantees for comonadic pattern operations.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation (if laws are not yet satisfied)**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [X] T046 [P] [US4] Add property-based test for extract-extend law (`extract . extend f = f`) in tests/Spec/Pattern/Properties.hs
- [X] T047 [P] [US4] Add property-based test for extend-extract law (`extend extract = id`) in tests/Spec/Pattern/Properties.hs
- [X] T048 [P] [US4] Add property-based test for extend composition law (`extend f . extend g = extend (f . extend g)`) in tests/Spec/Pattern/Properties.hs
- [X] T049 [P] [US4] Add Arbitrary instance for context-aware functions if needed for property-based testing in tests/Spec/Pattern/Properties.hs
- [X] T050 [US4] Run property-based tests to verify laws: `timeout 60 cabal test` in project root

### Implementation for User Story 4

**Note**: If the implementation from User Stories 1-3 already satisfies the laws, these tests should pass. If not, fix the implementation.

- [X] T051 [US4] Verify extract-extend law holds for all pattern structures (atomic, with elements, nested)
- [X] T052 [US4] Verify extend-extract law holds for all pattern structures
- [X] T053 [US4] Verify extend composition law holds for all pattern structures
- [X] T054 [US4] Fix any law violations in src/Pattern/Core.hs if tests fail
- [X] T055 [US4] Run all property-based tests: `timeout 60 cabal test` in project root
- [X] T056 [US4] Verify all User Story 4 tests pass independently

**Checkpoint**: At this point, all Comonad laws should be verified through property-based testing. The implementation should satisfy extract-extend, extend-extract, and extend composition laws for all pattern structures.

### Git Commit for User Story 4

- [ ] T057 [US4] Commit User Story 4: `git add -A && git commit -m "test: add property-based tests for Comonad laws (User Story 4)

- Add property-based tests for extract-extend law
- Add property-based tests for extend-extract law
- Add property-based tests for extend composition law
- Verify all laws hold for atomic, element, and nested patterns
- Fix any law violations in Comonad instance implementation"`

---

## Phase 6: User Story 5 - Context-Aware Operations (Priority: P2)

**Goal**: Implement optional helper functions (depthAt, sizeAt, indicesAt) that provide convenient access to common context-aware operations, demonstrating the power of the Comonad instance.

**Independent Test**: Can be fully tested by implementing helper functions that use `extend` to compute common context information (depth, size, indices) at each position, testing on various pattern structures. This delivers convenient access to common context-aware computations.

**Note**: Helper functions are optional convenience functions. If time is limited, this phase can be skipped.

### Tests for User Story 5

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [X] T058 [P] [US5] Add unit test for `depthAt` with atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [X] T059 [P] [US5] Add unit test for `depthAt` with nested pattern structure in tests/Spec/Pattern/CoreSpec.hs
- [X] T060 [P] [US5] Add unit test for `sizeAt` with pattern with elements in tests/Spec/Pattern/CoreSpec.hs
- [X] T061 [P] [US5] Add unit test for `sizeAt` with nested pattern structure in tests/Spec/Pattern/CoreSpec.hs
- [X] T062 [P] [US5] Add unit test for `indicesAt` with pattern with elements in tests/Spec/Pattern/CoreSpec.hs
- [X] T063 [P] [US5] Add unit test for `indicesAt` with nested pattern structure in tests/Spec/Pattern/CoreSpec.hs
- [X] T064 [US5] Run tests to verify they fail: `timeout 60 cabal test` in project root

### Implementation for User Story 5

- [X] T065 [US5] Implement `depthAt :: Pattern v -> Pattern Int` helper function in src/Pattern/Core.hs (uses `extend (\p -> depth p)`)
- [X] T066 [US5] Implement `sizeAt :: Pattern v -> Pattern Int` helper function in src/Pattern/Core.hs (uses `extend (\p -> size p)`)
- [X] T067 [US5] Implement `indicesAt :: Pattern v -> Pattern [Int]` helper function in src/Pattern/Core.hs (uses `extend (\p -> indicesFromRoot p pos)`)
- [X] T068 [US5] Add Haddock documentation for helper functions in src/Pattern/Core.hs (explain convenience functions, include examples)
- [X] T069 [US5] Run tests to verify implementation: `timeout 60 cabal test` in project root
- [X] T070 [US5] Verify all User Story 5 tests pass independently

**Checkpoint**: At this point, User Story 5 should be fully functional and testable independently. Helper functions should work for all pattern structures, providing convenient access to common context-aware operations.

### Git Commit for User Story 5

- [X] T071 [US5] Commit User Story 5: `git add -A && git commit -m "feat: add context-aware helper functions (User Story 5)

- Implement depthAt helper function
- Implement sizeAt helper function
- Implement indicesAt helper function (if indicesFromRoot exists)
- Add comprehensive Haddock documentation
- Add unit tests for all helper functions
- Demonstrate power of Comonad instance"`

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final improvements, edge case handling, and comprehensive testing

### Edge Case Tests

- [ ] T072 [P] Add unit test for `extract` on atomic pattern with different value types in tests/Spec/Pattern/CoreSpec.hs
- [ ] T073 [P] Add unit test for `extend` with function accessing parent context on root pattern in tests/Spec/Pattern/CoreSpec.hs
- [ ] T074 [P] Add unit test for `extend` with function accessing sibling context on atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [ ] T075 [P] Add unit test for `duplicate` on deeply nested pattern (100+ levels) in tests/Spec/Pattern/CoreSpec.hs
- [ ] T076 [P] Add unit test for `extend` with functions transforming value types in tests/Spec/Pattern/CoreSpec.hs
- [ ] T077 [P] Add unit test for context-aware operations on patterns with many elements (100+ elements) in tests/Spec/Pattern/CoreSpec.hs

### Integration Tests

- [ ] T078 [P] Add integration test for Comonad operations with existing Pattern operations (Functor, Foldable, Traversable) in tests/Spec/Pattern/CoreSpec.hs
- [ ] T079 [P] Add integration test for Comonad operations with basic query functions (size, depth) in tests/Spec/Pattern/CoreSpec.hs

### Documentation and Cleanup

- [X] T080 Update module-level documentation in src/Pattern/Core.hs to include Comonad instance in typeclass summary
- [ ] T081 Verify all Haddock documentation examples compile and run correctly
- [ ] T082 Run quickstart.md validation to ensure examples work
- [ ] T083 Run full test suite with timeout: `timeout 60 cabal test` to verify all tests pass
- [ ] T084 Code review and cleanup (remove unused imports, fix formatting)

### Final Git Commit

- [ ] T085 Commit polish phase: `git add -A && git commit -m "test: add edge case and integration tests for Comonad instance

- Add edge case tests for all Comonad operations
- Add integration tests with existing Pattern operations
- Update module-level documentation
- Verify all tests pass
- Code cleanup and formatting"`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **User Story 1 (Phase 2)**: Depends on Setup completion - Can start after Phase 1
- **User Story 2 (Phase 3)**: Depends on User Story 1 completion (needs extract for extend implementation via `fmap f . duplicate`)
- **User Story 3 (Phase 4)**: Depends on User Story 1 completion (needs extract for verification) - Can be done in parallel with User Story 2 if duplicate is implemented first
- **User Story 4 (Phase 5)**: Depends on User Stories 1-3 completion (needs all three operations for law verification)
- **User Story 5 (Phase 6)**: Depends on User Story 2 completion (needs extend for helper functions) - Optional
- **Polish (Phase 7)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Setup (Phase 1) - No dependencies on other stories
- **User Story 2 (P1)**: Depends on User Story 1 (extend uses duplicate, which may use extract) - Should be done after US1
- **User Story 3 (P1)**: Depends on User Story 1 (duplicate implementation, extract for verification) - Can be done in parallel with US2 if duplicate is implemented independently
- **User Story 4 (P1)**: Depends on User Stories 1-3 (needs all operations for law verification) - Must be done after US1-3
- **User Story 5 (P2)**: Depends on User Story 2 (needs extend for helper functions) - Optional, can be skipped

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Core implementation before documentation
- Documentation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All test tasks for a user story marked [P] can run in parallel
- User Stories 2 and 3 can potentially run in parallel if duplicate is implemented independently (though extend depends on duplicate)
- All edge case tests in Polish phase marked [P] can run in parallel
- Integration tests in Polish phase marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Add unit test for extract with atomic pattern (integer value) in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for extract with atomic pattern (string value) in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for extract with pattern with elements in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for extract with nested pattern structure in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add unit test for extract with different value types in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Stories 1-4 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: User Story 1 (extract)
3. Complete Phase 3: User Story 2 (extend)
4. Complete Phase 4: User Story 3 (duplicate)
5. Complete Phase 5: User Story 4 (law verification)
6. **STOP and VALIDATE**: Test all core Comonad operations independently
7. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup → Foundation ready
2. Add User Story 1 (extract) → Test independently → Verify MVP capability
3. Add User Story 2 (extend) → Test independently → Verify context-aware computation
4. Add User Story 3 (duplicate) → Test independently → Verify context creation
5. Add User Story 4 (laws) → Test independently → Verify mathematical correctness
6. Add User Story 5 (helpers) → Test independently → Add convenience functions (optional)
7. Polish phase → Final testing and documentation

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup together
2. Once Setup is done:
   - Developer A: User Story 1 (extract)
   - Developer B: Prepare test infrastructure for User Stories 2-4
3. Once User Story 1 is done:
   - Developer A: User Story 2 (extend)
   - Developer B: User Story 3 (duplicate) - if duplicate can be implemented independently
4. Once User Stories 1-3 are done:
   - Developer A: User Story 4 (laws)
   - Developer B: User Story 5 (helpers) - optional
5. Both: Polish phase

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- **Git commit after each user story completion** (see tasks T020, T033, T045, T057, T071)
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- User Story 5 (helper functions) is optional and can be skipped if time is limited

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

# Run specific test suite:
timeout 60 cabal test --test-option=--match="Comonad"
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Use profiling tools if tests consistently slow

