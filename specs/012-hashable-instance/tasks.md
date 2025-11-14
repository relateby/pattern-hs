# Tasks: Hashable Instance for Pattern

**Input**: Design documents from `/specs/012-hashable-instance/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Tests are REQUIRED. The `Hashable` instance must have comprehensive test coverage including unit tests, property-based tests, and integration tests.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4, US5)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- `Hashable` instance in `src/Pattern/Core.hs`
- Tests in `tests/Spec/Pattern/CoreSpec.hs` and `tests/Spec/Pattern/Properties.hs`

---

## Phase 1: Prerequisites (Already Complete)

**Note**: User Stories 1 and 2 (evaluation and design) have been completed during the research and design phases. The use cases have been identified, hash semantics have been designed, and alignment with the Pattern model has been verified. These are prerequisites for implementation.

**User Story 1 - Evaluate Use Cases for Hash-Based Containers**: ✅ Complete (documented in `research.md`)
- Three concrete use cases identified: high-performance pattern lookups, pattern deduplication in large collections, integration with hash-based libraries
- Comparison with ordered containers completed
- Distinct value beyond `Data.Map`/`Data.Set` confirmed

**User Story 2 - Design Hash Semantics Consistent with Equality**: ✅ Complete (documented in `research.md` and `data-model.md`)
- Hash semantics defined: structure-preserving hashing based on value and elements recursively
- Hash consistency with `Eq` verified: equal patterns produce same hash
- Hash distribution strategy defined with collision handling approach
- Alignment with Pattern model confirmed

---

## Phase 2: User Story 3 - Implement Hashable Instance (Priority: P3) 🎯 MVP

**Goal**: Implement `Hashable` instance for `Pattern` that hashes patterns based on their structure (value and elements recursively), ensuring that equal patterns (according to `Eq`) produce the same hash value. This enables hash-based lookups and deduplication using `HashMap` and `HashSet`.

**Independent Test**: Can be fully tested by implementing the `Hashable` instance, verifying it compiles with the required type constraints, and ensuring it follows the designed semantics. This delivers immediate value for hash-based pattern operations.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total. See Testing Performance Guidelines section below.

- [x] T001 [P] [US3] Write unit test for hashing atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T002 [P] [US3] Write unit test for hashing pattern with elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T003 [P] [US3] Write unit test for hashing nested pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T004 [P] [US3] Write unit test for hash consistency with Eq: equal patterns have same hash in tests/Spec/Pattern/CoreSpec.hs
- [x] T005 [P] [US3] Write unit test for hashing patterns with String values in tests/Spec/Pattern/CoreSpec.hs
- [x] T006 [P] [US3] Write unit test for hashing patterns with Int values in tests/Spec/Pattern/CoreSpec.hs
- [x] T007 [P] [US3] Write unit test for structure-preserving hashing: different structures produce different hashes in tests/Spec/Pattern/CoreSpec.hs
- [x] T008 [P] [US3] Write unit test for recursive hashing: nested structures contribute to hash in tests/Spec/Pattern/CoreSpec.hs
- [x] T009 [P] [US3] Write unit test for type constraint: `Hashable v` requirement in tests/Spec/Pattern/CoreSpec.hs
- [x] T010 [P] [US3] Write unit test for `hashWithSalt` function in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 3

- [x] T011 [US3] Add `hashable` dependency to pattern.cabal (version ^>=1.4)
- [x] T012 [US3] Import `Data.Hashable` module in src/Pattern/Core.hs
- [x] T013 [US3] Implement `Hashable` instance for `Pattern v` with `hashWithSalt` in src/Pattern/Core.hs
- [x] T014 [US3] Add comprehensive Haddock documentation with examples for `Hashable` instance in src/Pattern/Core.hs
- [x] T015 [US3] Verify `Hashable` instance is exported from Pattern.Core module in src/Pattern/Core.hs
- [x] T016 [US3] Verify all tests pass for `Hashable` instance

**Checkpoint**: ✅ At this point, User Story 3 should be fully functional and testable independently. `Hashable` instance works correctly for hashing patterns with structure-preserving semantics.

---

## Phase 3: User Story 4 - Verify Hash Consistency and Distribution (Priority: P3)

**Goal**: Verify that the `Hashable` instance satisfies hash consistency with `Eq` and provides good hash distribution, ensuring robust behavior for production use.

**Independent Test**: Can be fully tested by writing property-based tests for hash consistency with `Eq` and distribution tests for hash collisions. This delivers value for ensuring mathematical correctness and handling all pattern structures.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T017 [P] [US4] Write property-based test for hash consistency with Eq: `p1 == p2` implies `hash p1 == hash p2` in tests/Spec/Pattern/Properties.hs
- [x] T018 [P] [US4] Write property-based test for hash consistency with different value types (String, Int, custom Hashable instances) in tests/Spec/Pattern/Properties.hs
- [x] T019 [P] [US4] Write property-based test for hash consistency with all pattern structures (atomic, with elements, nested, different depths) in tests/Spec/Pattern/Properties.hs
- [x] T020 [P] [US4] Write property-based test for hash distribution: different structures produce different hashes in tests/Spec/Pattern/Properties.hs
- [x] T021 [P] [US4] Write statistical test for hash collision rate (verify < 1% collision rate for random patterns) in tests/Spec/Pattern/Properties.hs
- [x] T022 [P] [US4] Write unit test for hashing atomic patterns (edge case) in tests/Spec/Pattern/CoreSpec.hs
- [x] T023 [P] [US4] Write unit test for hashing patterns with many elements (100+ elements) in tests/Spec/Pattern/CoreSpec.hs
- [x] T024 [P] [US4] Write unit test for hashing deeply nested patterns (10+ levels) in tests/Spec/Pattern/CoreSpec.hs
- [x] T025 [P] [US4] Write unit test for hashing patterns with same flattened values but different structures in tests/Spec/Pattern/CoreSpec.hs
- [x] T026 [P] [US4] Write unit test for hashing patterns with duplicate values in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 4

- [x] T027 [US4] Verify `Hashable` instance satisfies hash consistency with `Eq` (no additional implementation needed, verify existing instance)
- [x] T028 [US4] Verify all tests pass for hash consistency and distribution

**Checkpoint**: ✅ At this point, User Stories 3 AND 4 should both work independently. `Hashable` instance satisfies hash consistency with `Eq` and provides good hash distribution.

---

## Phase 4: User Story 5 - Integration with Hash-Based Containers (Priority: P3)

**Goal**: Verify integration with `HashMap` and `HashSet` for efficient lookups and deduplication, demonstrating real-world usage of the `Hashable` instance.

**Independent Test**: Can be fully tested by creating `HashMap (Pattern v) a` and `HashSet (Pattern v)` instances and verifying that patterns are correctly hashed, can be used for lookups, and handle deduplication correctly. This delivers verification of real-world usage.

### Tests for User Story 5

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T029 [P] [US5] Write integration test for `HashMap` with patterns as keys: create HashMap and perform lookups in tests/Spec/Pattern/CoreSpec.hs
- [x] T030 [P] [US5] Write integration test for `HashMap` with patterns as keys: insert patterns and verify lookups work correctly in tests/Spec/Pattern/CoreSpec.hs
- [x] T031 [P] [US5] Write integration test for `HashMap` with patterns as keys: handle hash collisions correctly (patterns with same hash but different values) in tests/Spec/Pattern/CoreSpec.hs
- [x] T032 [P] [US5] Write integration test for `HashSet` with patterns as elements: create HashSet and test membership in tests/Spec/Pattern/CoreSpec.hs
- [x] T033 [P] [US5] Write integration test for `HashSet` with patterns as elements: insert patterns and verify deduplication works correctly in tests/Spec/Pattern/CoreSpec.hs
- [x] T034 [P] [US5] Write integration test for `HashSet` with patterns as elements: handle hash collisions correctly (patterns with same hash but different values) in tests/Spec/Pattern/CoreSpec.hs
- [x] T035 [P] [US5] Write integration test for `HashMap` performance: O(1) average-case lookups in tests/Spec/Pattern/CoreSpec.hs
- [x] T036 [P] [US5] Write integration test for `HashSet` performance: O(1) average-case membership testing in tests/Spec/Pattern/CoreSpec.hs
- [x] T037 [P] [US5] Write integration test for `HashMap` with nested patterns as keys in tests/Spec/Pattern/CoreSpec.hs
- [x] T038 [P] [US5] Write integration test for `HashSet` with nested patterns as elements in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 5

- [x] T039 [US5] Verify `HashMap` and `HashSet` integration works correctly (no additional implementation needed, verify existing instance)
- [x] T040 [US5] Verify all integration tests pass

**Checkpoint**: ✅ At this point, User Stories 3, 4, AND 5 should all work independently. `Hashable` instance is fully integrated with hash-based containers and ready for production use.

---

## Phase 5: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, documentation updates, and integration with existing Pattern operations

### Documentation & Integration

- [x] T041 [P] Review and update module-level documentation in src/Pattern/Core.hs to include `Hashable` instance
- [x] T042 [P] Verify `Hashable` instance is re-exported from main Pattern module in src/Pattern.hs
- [x] T043 [P] Add examples to Haddock documentation showing hash-based container usage in src/Pattern/Core.hs
- [x] T044 [P] Add examples to Haddock documentation showing when to use hash-based vs ordered containers in src/Pattern/Core.hs

### Integration Tests

- [x] T045 [P] Write integration test for `Hashable` instance with pattern constructors (`pattern`, `patternWith`, `fromList`) in tests/Spec/Pattern/CoreSpec.hs
- [x] T046 [P] Write integration test for `Hashable` instance with type class instances (`Functor`, `Foldable`, `Traversable`, `Eq`, `Ord`) in tests/Spec/Pattern/CoreSpec.hs
- [x] T047 [P] Write integration test for `Hashable` instance with `Semigroup` and `Monoid` instances in tests/Spec/Pattern/CoreSpec.hs

### Final Validation

- [x] T048 Verify all tests pass (run full test suite with timeout)
- [x] T049 Verify `Hashable` instance compiles with all supported GHC versions (9.8.4, 9.10.3)
- [x] T050 Verify `Hashable` instance follows Haskell best practices and conventions

**Checkpoint**: ✅ At this point, all tests pass, documentation is updated, and `Hashable` instance is fully integrated with the Pattern library.

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 3 (Phase 2)**: Depends on User Stories 1 and 2 completion (prerequisites already complete from research/design phase)
- **User Story 4 (Phase 3)**: Depends on User Story 3 completion - requires `Hashable` instance to be implemented
- **User Story 5 (Phase 4)**: Depends on User Story 3 completion - requires `Hashable` instance to be implemented
- **Polish (Phase 5)**: Depends on all user stories completion - comprehensive validation

### User Story Dependencies

```
User Story 1 (Evaluation) ✅ Complete
    ↓
User Story 2 (Design) ✅ Complete
    ↓
User Story 3 (Implementation) → User Story 4 (Verification)
    ↓                          → User Story 5 (Integration)
    ↓
Polish & Cross-Cutting Concerns
```

### Parallel Execution Opportunities

**Within User Story 3 (Phase 2)**:
- All test tasks (T001-T010) can run in parallel - they test different aspects independently
- Implementation tasks (T011-T016) must wait for tests to be written (TDD approach)

**Within User Story 4 (Phase 3)**:
- All test tasks (T017-T026) can run in parallel - they test different aspects independently
- Verification tasks (T027-T028) must wait for tests to be written

**Within User Story 5 (Phase 4)**:
- All test tasks (T029-T038) can run in parallel - they test different aspects independently
- Verification tasks (T039-T040) must wait for tests to be written

**Within Polish (Phase 5)**:
- Documentation tasks (T041-T044) can run in parallel
- Integration test tasks (T045-T047) can run in parallel
- Final validation tasks (T048-T050) must wait for all other tasks

### Story Independence

- **User Story 3**: Can be implemented and tested independently once prerequisites are met
- **User Story 4**: Can be tested independently once User Story 3 is complete
- **User Story 5**: Can be tested independently once User Story 3 is complete
- **Polish**: Requires all user stories to be complete

---

## Implementation Strategy

### MVP Scope (Minimum Viable Product)

**MVP = User Story 3 only**

The MVP delivers a working `Hashable` instance that:
- Hashes patterns based on structure (value and elements recursively)
- Satisfies hash consistency with `Eq` (equal patterns have same hash)
- Compiles with `Hashable v` constraint
- Has basic unit tests

**MVP Value**: Enables basic hash-based operations with patterns, providing immediate value for hash-based lookups and deduplication.

### Incremental Delivery

1. **Phase 2 (MVP)**: Implement `Hashable` instance with basic tests
2. **Phase 3**: Add comprehensive verification (hash consistency, distribution)
3. **Phase 4**: Add integration tests with hash-based containers
4. **Phase 5**: Polish, documentation, and final validation

Each phase builds on the previous, delivering incremental value while maintaining working code.

---

## Testing Performance Guidelines

**CRITICAL**: All tests must complete quickly to maintain development velocity.

### Performance Targets

- **Unit tests**: < 1 second total for all unit tests
- **Property-based tests**: < 10 seconds total for all property-based tests
- **Integration tests**: < 5 seconds total for all integration tests
- **Full test suite**: < 1 minute total

### Performance Optimization Strategies

1. **Limit QuickCheck test size**: Use `quickCheckWith stdArgs {maxSize = 10}` for property-based tests
2. **Use timeouts**: Always run tests with timeout (`timeout 60 cabal test`)
3. **Parallel test execution**: Run independent tests in parallel when possible
4. **Reduce test data size**: Use smaller patterns for edge case tests (e.g., 10 levels instead of 100)
5. **Skip expensive tests in CI**: Mark performance tests as optional or run separately

### Test Organization

- **Fast tests first**: Unit tests should run first and complete quickly
- **Property-based tests**: Should use reasonable size limits to avoid long runtimes
- **Integration tests**: Should test real-world scenarios but with reasonable data sizes
- **Performance tests**: Should be optional or run separately from main test suite

---

## Task Summary

- **Total Tasks**: 50
- **User Story 3 (MVP)**: 16 tasks (T001-T016)
- **User Story 4**: 12 tasks (T017-T028)
- **User Story 5**: 12 tasks (T029-T040)
- **Polish**: 10 tasks (T041-T050)

### Task Breakdown by Type

- **Test Tasks**: 38 tasks (unit tests, property-based tests, integration tests)
- **Implementation Tasks**: 5 tasks (instance implementation, documentation, exports)
- **Verification Tasks**: 4 tasks (test verification, compilation checks)
- **Documentation Tasks**: 3 tasks (Haddock documentation, module exports)

### Parallel Opportunities

- **Phase 2**: 10 test tasks can run in parallel
- **Phase 3**: 10 test tasks can run in parallel
- **Phase 4**: 10 test tasks can run in parallel
- **Phase 5**: 7 tasks can run in parallel (documentation and integration tests)

---

## Notes

- All tasks follow TDD approach: write tests first, then implement
- Tests must fail before implementation to verify they're testing correctly
- All file paths are absolute or relative to repository root
- Task IDs are sequential and indicate execution order
- [P] marker indicates tasks that can run in parallel
- [Story] label indicates which user story the task belongs to
- MVP scope is User Story 3 only, delivering immediate value
- Each phase builds incrementally on previous phases

