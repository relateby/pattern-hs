# Tasks: Ord Instance for Pattern

**Input**: Design documents from `/specs/009-ord-instance/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Tests are REQUIRED. The `Ord` instance must have comprehensive test coverage including unit tests, property-based tests, and integration tests.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- `Ord` instance in `src/Pattern/Core.hs`
- Tests in `tests/Spec/Pattern/CoreSpec.hs` and `tests/Spec/Pattern/Properties.hs`

---

## Phase 1: User Story 1 - Order Patterns by Structure (Priority: P1) 🎯 MVP

**Goal**: Implement `Ord` instance that orders patterns using lexicographic ordering (value first, then elements recursively). This enables basic pattern comparison and ordering operations.

**Independent Test**: Can be fully tested by comparing patterns with different values and structures, verifying that ordering follows lexicographic rules (value first, then elements recursively). This delivers immediate value for pattern organization and standard library integration.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total. See Testing Performance Guidelines section below.

- [x] T001 [P] [US1] Write unit test for `compare` with atomic patterns having different values in tests/Spec/Pattern/CoreSpec.hs
- [x] T002 [P] [US1] Write unit test for `compare` with atomic patterns having same value in tests/Spec/Pattern/CoreSpec.hs
- [x] T003 [P] [US1] Write unit test for `compare` with patterns having same value but different elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T004 [P] [US1] Write unit test for `compare` with patterns having same value and same number of elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T005 [P] [US1] Write unit test for `compare` with nested patterns (recursive comparison) in tests/Spec/Pattern/CoreSpec.hs
- [x] T006 [P] [US1] Write unit test for comparison operators (`<`, `<=`, `>`, `>=`) with patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T007 [P] [US1] Write unit test for `min` and `max` functions with patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T008 [P] [US1] Write property-based test for transitivity: if `p1 < p2` and `p2 < p3`, then `p1 < p3` in tests/Spec/Pattern/Properties.hs
- [x] T009 [P] [US1] Write property-based test for antisymmetry: if `p1 < p2`, then `p2 > p1` in tests/Spec/Pattern/Properties.hs
- [x] T010 [P] [US1] Write property-based test for reflexivity: `p1 <= p1` always true in tests/Spec/Pattern/Properties.hs
- [x] T011 [P] [US1] Write property-based test for lexicographic ordering: `compare p1 p2 == compare (toTuple p1) (toTuple p2)` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 1

- [x] T012 [US1] Implement `Ord` instance for `Pattern v` with `compare` function in src/Pattern/Core.hs
- [x] T013 [US1] Add comprehensive Haddock documentation with examples for `Ord` instance in src/Pattern/Core.hs
- [x] T014 [US1] Verify `Ord` instance is exported from Pattern.Core module in src/Pattern/Core.hs
- [x] T015 [US1] Verify all tests pass for `Ord` instance

**Checkpoint**: ✅ **COMPLETE** - At this point, User Story 1 should be fully functional and testable independently. `Ord` instance works correctly for all pattern structures with lexicographic ordering.

---

## Phase 2: User Story 2 - Use Patterns in Ordered Collections (Priority: P1)

**Goal**: Verify that patterns can be used as keys in `Data.Map` and elements in `Data.Set`, enabling efficient pattern organization and indexing.

**Independent Test**: Can be fully tested by creating `Data.Set (Pattern v)` and `Data.Map (Pattern v) a` instances and verifying that patterns are correctly ordered and can be used for lookups. This delivers immediate value for pattern organization.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T016 [P] [US2] Write integration test for `Data.Set` with patterns (insertion and ordering) in tests/Spec/Pattern/CoreSpec.hs
- [x] T017 [P] [US2] Write integration test for `Data.Set` membership lookup with patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T018 [P] [US2] Write integration test for `Data.Set` with patterns having duplicate values but different structures in tests/Spec/Pattern/CoreSpec.hs
- [x] T019 [P] [US2] Write integration test for `Data.Map` with patterns as keys (insertion and lookup) in tests/Spec/Pattern/CoreSpec.hs
- [x] T020 [P] [US2] Write integration test for `Data.Map` key matching with patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T021 [P] [US2] Write integration test for sorting patterns using `sort` function in tests/Spec/Pattern/CoreSpec.hs
- [x] T022 [P] [US2] Write integration test for `minimum` and `maximum` functions with pattern lists in tests/Spec/Pattern/CoreSpec.hs
- [x] T023 [P] [US2] Write property-based test for `Data.Set` operations with patterns in tests/Spec/Pattern/Properties.hs
- [x] T024 [P] [US2] Write property-based test for `Data.Map` operations with patterns in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 2

- [x] T025 [US2] Verify `Ord` instance works with `Data.Set` (no additional implementation needed, verify existing instance)
- [x] T026 [US2] Verify `Ord` instance works with `Data.Map` (no additional implementation needed, verify existing instance)
- [x] T027 [US2] Verify all tests pass for standard library integration

**Checkpoint**: ✅ **COMPLETE** - At this point, User Stories 1 AND 2 should both work independently. `Ord` instance enables standard library integration with `Data.Set` and `Data.Map`.

---

## Phase 3: User Story 3 - Verify Ordering Consistency with Equality (Priority: P2)

**Goal**: Verify that the `Ord` instance is consistent with the existing `Eq` instance, ensuring that equal patterns compare as equal and ordering respects structural equality.

**Independent Test**: Can be fully tested by verifying that `p1 == p2` implies `compare p1 p2 == EQ`, and that ordering respects the same structural comparison rules as equality. This delivers value for ensuring correctness and preventing subtle bugs.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T028 [P] [US3] Write unit test for consistency: patterns equal by `Eq` compare as `EQ` in tests/Spec/Pattern/CoreSpec.hs
- [x] T029 [P] [US3] Write unit test for consistency: patterns not equal by `Eq` don't compare as `EQ` in tests/Spec/Pattern/CoreSpec.hs
- [x] T030 [P] [US3] Write unit test for consistency: patterns with same structure but different values compare by value in tests/Spec/Pattern/CoreSpec.hs
- [x] T031 [P] [US3] Write unit test for consistency: patterns with same value but different element structures compare by elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T032 [P] [US3] Write property-based test for consistency: `p1 == p2` implies `compare p1 p2 == EQ` in tests/Spec/Pattern/Properties.hs
- [x] T033 [P] [US3] Write property-based test for consistency: `compare p1 p2 == EQ` implies `p1 == p2` in tests/Spec/Pattern/Properties.hs
- [x] T034 [P] [US3] Write property-based test for consistency: ordering uses same comparison order as `Eq` (value first, then elements) in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 3

- [x] T035 [US3] Verify `Ord` instance consistency with `Eq` instance (no additional implementation needed, verify existing instance)
- [x] T036 [US3] Verify all tests pass for consistency verification

**Checkpoint**: ✅ **COMPLETE** - At this point, all user stories should be complete. `Ord` instance is consistent with `Eq` instance and works correctly for all pattern structures.

---

## Phase 4: Edge Cases & Cross-Cutting Concerns

**Purpose**: Comprehensive edge case testing and final validation

### Edge Case Tests

- [x] T037 [P] Write edge case test for comparing atomic patterns (no elements) in tests/Spec/Pattern/CoreSpec.hs
- [x] T038 [P] Write edge case test for comparing patterns with different numbers of elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T039 [P] Write edge case test for comparing deeply nested patterns (100+ levels) in tests/Spec/Pattern/CoreSpec.hs
- [x] T040 [P] Write edge case test for comparing patterns with same flattened values but different structures in tests/Spec/Pattern/CoreSpec.hs
- [x] T041 [P] Write edge case test for type constraint: `Ord v` requirement in tests/Spec/Pattern/CoreSpec.hs

### Integration & Validation

- [x] T042 Verify all query functions work with `Ord` instance (integration with existing functions) in tests/Spec/Pattern/CoreSpec.hs
- [x] T043 Verify `Ord` instance works with pattern constructors (`pattern`, `patternWith`, `fromList`) in tests/Spec/Pattern/CoreSpec.hs
- [x] T044 Verify `Ord` instance works with type class instances (`Functor`, `Foldable`, `Traversable`) in tests/Spec/Pattern/CoreSpec.hs
- [x] T045 Review and update module-level documentation in src/Pattern/Core.hs to include `Ord` instance
- [x] T046 Verify `Ord` instance is re-exported from main Pattern module in src/Pattern.hs
- [x] T047 Verify all tests pass (run full test suite with timeout)

**Checkpoint**: ✅ **COMPLETE** - All edge cases covered, integration verified, documentation updated, and all tests passing (370 examples, 0 failures).

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 1 (Phase 1)**: No dependencies - can start immediately (Pattern type and `Eq` instance already exist)
- **User Story 2 (Phase 2)**: Depends on User Story 1 completion - requires `Ord` instance to be implemented
- **User Story 3 (Phase 3)**: Depends on User Story 1 completion - requires `Ord` instance to be implemented
- **Edge Cases (Phase 4)**: Depends on all user stories completion - comprehensive validation

### User Story Dependencies

- **User Story 1 (P1)**: Can start immediately - No dependencies on other stories
- **User Story 2 (P1)**: Depends on User Story 1 - Requires `Ord` instance implementation
- **User Story 3 (P2)**: Depends on User Story 1 - Requires `Ord` instance implementation

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Implementation follows tests
- Story complete before moving to next priority

### Parallel Opportunities

- All test tasks marked [P] within a user story can run in parallel
- User Stories 2 and 3 can be worked on in parallel after User Story 1 completes (they both depend on US1 but are independent of each other)
- Edge case tests marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all unit tests for User Story 1 together:
Task: "Write unit test for compare with atomic patterns having different values in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for compare with atomic patterns having same value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for compare with patterns having same value but different elements in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for compare with patterns having same value and same number of elements in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for compare with nested patterns in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for comparison operators in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for min and max functions in tests/Spec/Pattern/CoreSpec.hs"

# Launch all property-based tests for User Story 1 together:
Task: "Write property-based test for transitivity in tests/Spec/Pattern/Properties.hs"
Task: "Write property-based test for antisymmetry in tests/Spec/Pattern/Properties.hs"
Task: "Write property-based test for reflexivity in tests/Spec/Pattern/Properties.hs"
Task: "Write property-based test for lexicographic ordering in tests/Spec/Pattern/Properties.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: User Story 1 (Order Patterns by Structure)
2. **STOP and VALIDATE**: Test `Ord` instance independently
3. Verify basic comparison operations work
4. Deploy/demo if ready

### Incremental Delivery

1. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
2. Add User Story 2 → Test independently → Deploy/Demo
3. Add User Story 3 → Test independently → Deploy/Demo
4. Add Edge Cases → Final validation → Deploy/Demo
5. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Developer A: User Story 1 (implementation)
2. Once User Story 1 is complete:
   - Developer A: User Story 2
   - Developer B: User Story 3 (can start in parallel)
3. Both complete and integrate independently
4. Final validation together

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence

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
timeout 30 cabal test --test-show-details=direct
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Use profiling tools if tests consistently slow

