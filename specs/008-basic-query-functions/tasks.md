# Tasks: Basic Query Functions

**Input**: Design documents from `/specs/008-basic-query-functions/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Tests are REQUIRED per FR-011. All query functions must have comprehensive test coverage.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Functions in `src/Pattern/Core.hs`
- Tests in `tests/Spec/Pattern/CoreSpec.hs`

---

## Phase 1: User Story 1 - Query Pattern Sequence Length (Priority: P1) 🎯 MVP

**Goal**: Implement `length` function that returns the number of direct elements in a pattern's sequence. This provides the most basic structural information about a pattern.

**Independent Test**: Can be fully tested by calling `length` on patterns with various element counts (0, 1, 2, many) and verifying it returns the correct count. This delivers immediate value for pattern introspection without requiring other query functions.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total. See Testing Performance Guidelines section below.

- [x] T001 [P] [US1] Write unit test for `length` with atomic pattern (should return 0) in tests/Spec/Pattern/CoreSpec.hs
- [x] T002 [P] [US1] Write unit test for `length` with single element pattern (should return 1) in tests/Spec/Pattern/CoreSpec.hs
- [x] T003 [P] [US1] Write unit test for `length` with multiple elements pattern (should return correct count) in tests/Spec/Pattern/CoreSpec.hs
- [x] T004 [P] [US1] Write unit test for `length` with nested pattern (should only count direct children) in tests/Spec/Pattern/CoreSpec.hs
- [x] T005 [P] [US1] Write property-based test for `length p >= 0` in tests/Spec/Pattern/Properties.hs
- [x] T006 [P] [US1] Write property-based test for `length p == length (elements p)` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 1

- [x] T007 [US1] Implement `length :: Pattern v -> Int` function in src/Pattern/Core.hs
- [x] T008 [US1] Add comprehensive Haddock documentation with examples for `length` function in src/Pattern/Core.hs
- [x] T009 [US1] Export `length` function from Pattern.Core module in src/Pattern/Core.hs
- [x] T010 [US1] Verify all tests pass for `length` function

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. `length` function works correctly for all pattern structures.

---

## Phase 2: User Story 2 - Query Total Pattern Size (Priority: P1)

**Goal**: Implement `size` function that returns the total number of nodes in a pattern structure (including all nested patterns). This provides a complete picture of pattern complexity.

**Independent Test**: Can be fully tested by calling `size` on patterns with various nesting depths and verifying it counts all nodes (root + all descendants). This delivers value for understanding pattern complexity independently.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T011 [P] [US2] Write unit test for `size` with atomic pattern (should return 1) in tests/Spec/Pattern/CoreSpec.hs
- [x] T012 [P] [US2] Write unit test for `size` with pattern having direct elements (should return 1 + element count) in tests/Spec/Pattern/CoreSpec.hs
- [x] T013 [P] [US2] Write unit test for `size` with deeply nested pattern (should count all nodes) in tests/Spec/Pattern/CoreSpec.hs
- [x] T014 [P] [US2] Write unit test for `size` with varying branch depths (should count all nodes across branches) in tests/Spec/Pattern/CoreSpec.hs
- [x] T015 [P] [US2] Write property-based test for `size p >= 1` in tests/Spec/Pattern/Properties.hs
- [x] T016 [P] [US2] Write property-based test for `size p >= length p` in tests/Spec/Pattern/Properties.hs
- [x] T017 [P] [US2] Write property-based test for `size p == 1 + sum (map size (elements p))` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 2

- [x] T018 [US2] Implement `size :: Pattern v -> Int` function in src/Pattern/Core.hs
- [x] T019 [US2] Add comprehensive Haddock documentation with examples for `size` function in src/Pattern/Core.hs
- [x] T020 [US2] Export `size` function from Pattern.Core module in src/Pattern/Core.hs
- [x] T021 [US2] Verify all tests pass for `size` function

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Both `length` and `size` functions work correctly.

---

## Phase 3: User Story 3 - Query Maximum Nesting Depth (Priority: P2)

**Goal**: Implement `depth` function that returns the maximum nesting depth of a pattern structure. This helps understand pattern complexity and validate depth constraints.

**Independent Test**: Can be fully tested by calling `depth` on patterns with various nesting structures and verifying it returns the maximum path depth. This delivers value for depth validation independently.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T022 [P] [US3] Write unit test for `depth` with atomic pattern (should return 0) in tests/Spec/Pattern/CoreSpec.hs
- [x] T023 [P] [US3] Write unit test for `depth` with one level of nesting (should return 1) in tests/Spec/Pattern/CoreSpec.hs
- [x] T024 [P] [US3] Write unit test for `depth` with multiple branches of different depths (should return maximum) in tests/Spec/Pattern/CoreSpec.hs
- [x] T025 [P] [US3] Write unit test for `depth` with deeply nested pattern (should return maximum depth) in tests/Spec/Pattern/CoreSpec.hs
- [x] T026 [P] [US3] Write property-based test for `depth p >= 0` in tests/Spec/Pattern/Properties.hs
- [x] T027 [P] [US3] Write property-based test for `depth p <= size p - 1` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 3

- [x] T028 [US3] Implement `depth :: Pattern v -> Int` function in src/Pattern/Core.hs
- [x] T029 [US3] Add comprehensive Haddock documentation with examples for `depth` function in src/Pattern/Core.hs
- [x] T030 [US3] Export `depth` function from Pattern.Core module in src/Pattern/Core.hs
- [x] T031 [US3] Verify all tests pass for `depth` function

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently. `length`, `size`, and `depth` functions all work correctly.

---

## Phase 4: User Story 4 - Extract All Pattern Values (Priority: P1)

**Goal**: Implement `values` function that returns all values from a pattern structure as a flat list. This enables value aggregation, analysis, and transformation operations.

**Independent Test**: Can be fully tested by calling `values` on patterns with various structures and verifying it returns all values in the correct order. This delivers immediate value for value extraction independently.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T032 [P] [US4] Write unit test for `values` with atomic pattern (should return single value) in tests/Spec/Pattern/CoreSpec.hs
- [x] T033 [P] [US4] Write unit test for `values` with multiple elements (should return all values) in tests/Spec/Pattern/CoreSpec.hs
- [x] T034 [P] [US4] Write unit test for `values` with nested pattern (should return all values from all levels) in tests/Spec/Pattern/CoreSpec.hs
- [x] T035 [P] [US4] Write unit test for `values` with varying nesting depths (should return values in consistent order) in tests/Spec/Pattern/CoreSpec.hs
- [x] T036 [P] [US4] Write property-based test for `length (values p) == size p` in tests/Spec/Pattern/Properties.hs
- [x] T037 [P] [US4] Write property-based test for `head (values p) == value p` in tests/Spec/Pattern/Properties.hs
- [x] T038 [P] [US4] Write property-based test for `values p == toList p` in tests/Spec/Pattern/Properties.hs
- [x] T039 [P] [US4] Write property-based test for `values p == flatten p` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 4

- [x] T040 [US4] Implement `values :: Pattern v -> [v]` function in src/Pattern/Core.hs (equivalent to `toList`)
- [x] T041 [US4] Add comprehensive Haddock documentation with examples for `values` function in src/Pattern/Core.hs
- [x] T042 [US4] Export `values` function from Pattern.Core module in src/Pattern/Core.hs
- [x] T043 [US4] Verify all tests pass for `values` function

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should all work independently. `length`, `size`, `depth`, and `values` functions all work correctly.

---

## Phase 5: User Story 5 - Access Pattern Value (Priority: P1)

**Goal**: Verify that the `value` field accessor works correctly for accessing pattern decoration values. This is already available from the data type definition, but needs verification and documentation.

**Independent Test**: Can be fully tested by accessing the `value` field on patterns with various value types and verifying it returns the correct value. This delivers immediate value for pattern value access.

### Tests for User Story 5

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T044 [P] [US5] Write unit test for `value` with string pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T045 [P] [US5] Write unit test for `value` with integer pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T046 [P] [US5] Write unit test for `value` with custom type pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T047 [P] [US5] Write unit test for `value` with nested patterns (verify each level returns correct value) in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 5

- [x] T048 [US5] Add comprehensive Haddock documentation for `value` field accessor in src/Pattern/Core.hs
- [x] T049 [US5] Verify `value` field accessor is exported from Pattern.Core module in src/Pattern/Core.hs
- [x] T050 [US5] Verify all tests pass for `value` field accessor

**Checkpoint**: At this point, all user stories should be complete. All query functions (`length`, `size`, `depth`, `values`, `value`) work correctly and are documented.

---

## Phase 6: Integration & Cross-Cutting Concerns

**Purpose**: Integration tests and cross-cutting improvements

### Integration Tests

- [x] T051 [P] Write integration test verifying query functions work with patterns created using `pattern` function in tests/Spec/Pattern/CoreSpec.hs
- [x] T052 [P] Write integration test verifying query functions work with patterns created using `patternWith` function in tests/Spec/Pattern/CoreSpec.hs
- [x] T053 [P] Write integration test verifying query functions work with patterns created using `fromList` function in tests/Spec/Pattern/CoreSpec.hs
- [x] T054 [P] Write integration test verifying query functions work with patterns transformed using `fmap` (Functor) in tests/Spec/Pattern/CoreSpec.hs
- [x] T055 [P] Write integration test verifying query functions work with patterns used in Foldable operations in tests/Spec/Pattern/CoreSpec.hs
- [x] T056 [P] Write integration test verifying query functions work with patterns used in Traversable operations in tests/Spec/Pattern/CoreSpec.hs

### Edge Case Tests

- [x] T057 [P] Write edge case test for query functions with very deeply nested patterns (100+ levels) in tests/Spec/Pattern/CoreSpec.hs
- [x] T058 [P] Write edge case test for query functions with patterns having many direct elements (100+) in tests/Spec/Pattern/CoreSpec.hs
- [x] T059 [P] Write edge case test for query functions with patterns containing duplicate values in tests/Spec/Pattern/CoreSpec.hs

### Module Exports

- [x] T060 Verify all query functions are exported from Pattern.Core module in src/Pattern/Core.hs
- [x] T061 Verify all query functions are re-exported from main Pattern module in src/Pattern.hs

### Documentation

- [x] T062 Review and update module-level documentation in src/Pattern/Core.hs to include query functions
- [x] T063 Verify all query functions have comprehensive Haddock documentation with examples

### Performance Validation

- [x] T064 Verify `length` function completes in <1ms for any pattern structure (O(1) operation, verified in documentation)
- [x] T065 Verify `size` function completes in <10ms for patterns with up to 1000 nodes (O(n) operation, verified in tests and documentation)
- [x] T066 Verify `depth` function completes in <5ms for patterns with up to 100 levels of nesting (O(n) operation, verified in tests with 100-level deep patterns)
- [x] T067 Verify `values` function completes in <10ms for patterns with up to 1000 nodes (O(n) operation, verified in tests and documentation)

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 1 (Phase 1)**: No dependencies - can start immediately (MVP)
- **User Story 2 (Phase 2)**: Can start after User Story 1 or in parallel (independent)
- **User Story 3 (Phase 3)**: Can start after User Story 1 or in parallel (independent)
- **User Story 4 (Phase 4)**: Can start after User Story 1 or in parallel (independent)
- **User Story 5 (Phase 5)**: Can start after User Story 1 or in parallel (independent)
- **Integration & Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: No dependencies - can start immediately
- **User Story 2 (P1)**: No dependencies on other stories - can start in parallel with US1
- **User Story 3 (P2)**: No dependencies on other stories - can start in parallel with US1/US2
- **User Story 4 (P1)**: No dependencies on other stories - can start in parallel with US1/US2
- **User Story 5 (P1)**: No dependencies on other stories - can start in parallel with US1/US2

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Implementation before verification
- Documentation added alongside implementation
- Story complete before moving to next priority (or can work in parallel)

### Parallel Opportunities

- All test tasks marked [P] can run in parallel (different test cases, same file)
- User Stories 1, 2, 4, 5 (all P1) can be worked on in parallel by different developers
- User Story 3 (P2) can be worked on in parallel with P1 stories
- Integration tests can be written in parallel
- Edge case tests can be written in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Write unit test for length with atomic pattern in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for length with single element pattern in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for length with multiple elements pattern in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for length with nested pattern in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write property-based test for length p >= 0 in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write property-based test for length p == length (elements p) in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Parallel Example: All P1 User Stories

```bash
# With multiple developers, work on P1 stories in parallel:
Developer A: User Story 1 (length function)
Developer B: User Story 2 (size function)
Developer C: User Story 4 (values function)
Developer D: User Story 5 (value verification)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: User Story 1 (length function)
2. **STOP and VALIDATE**: Test User Story 1 independently
3. Deploy/demo if ready

### Incremental Delivery

1. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
2. Add User Story 2 → Test independently → Deploy/Demo
3. Add User Story 4 → Test independently → Deploy/Demo
4. Add User Story 5 → Test independently → Deploy/Demo
5. Add User Story 3 → Test independently → Deploy/Demo
6. Add Integration & Polish → Final validation
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. All developers start with their assigned P1 user story:
   - Developer A: User Story 1 (length)
   - Developer B: User Story 2 (size)
   - Developer C: User Story 4 (values)
   - Developer D: User Story 5 (value verification)
2. Once P1 stories complete:
   - Developer A: User Story 3 (depth)
   - Developer B: Integration tests
   - Developer C: Edge case tests
   - Developer D: Documentation and polish
3. Stories complete and integrate independently

---

## Notes

- [P] tasks = different test cases or different functions, can work in parallel
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- All query functions are pure (no side effects)
- All query functions work with patterns of any value type `v`
- Performance targets: length <1ms, size <10ms (1000 nodes), depth <5ms (100 levels), values <10ms (1000 nodes)

## Testing Performance Guidelines

### Test Execution Timeouts

**CRITICAL**: Always use timeouts when running tests to prevent hanging:

- **First test run after implementation**: Use `timeout 60 cabal test` (60 seconds) to catch any infinite loops or performance issues
- **Subsequent test runs**: Use `timeout 30 cabal test` (30 seconds) for normal verification
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
timeout 30 cabal test pattern-test
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Use profiling tools if tests consistently slow

