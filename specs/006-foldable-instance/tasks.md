# Tasks: Foldable Instance for Pattern

**Input**: Design documents from `/specs/006-foldable-instance/`
**Prerequisites**: plan.md (required), spec.md (required for user stories)

**Tests**: Tests are included as they are essential for verifying foldable operations, laws, and correctness.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Paths shown below assume single project structure

---

## Phase 1: User Story 1 - Aggregate Values from Patterns (Priority: P1) 🎯 MVP

**Goal**: Implement Foldable instance with `foldr` that aggregates values from patterns (sum, product, concatenation, etc.) so that developers can compute statistics, combine values, and perform calculations over pattern structures without manually traversing the pattern tree.

**Independent Test**: Apply fold operations to patterns and verify that: (1) all values are collected correctly, (2) aggregation functions produce correct results, and (3) folding works for atomic patterns, patterns with elements, and nested patterns.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T001 [P] [US1] Write unit test for folding atomic pattern with integer value using foldr in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T002 [P] [US1] Write unit test for folding pattern with multiple integer values using foldr in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T003 [P] [US1] Write unit test for folding pattern with string values using foldr in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T004 [P] [US1] Write unit test for folding nested pattern structure using foldr in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T005 [P] [US1] Write unit test for folding pattern with custom type values using foldr in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T006 [P] [US1] Write unit test verifying foldr processes pattern's own value in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T007 [P] [US1] Write unit test verifying foldr processes all element values recursively in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 1

- [x] T008 [US1] Implement Foldable instance for Pattern with foldr as primary method in `src/Pattern/Core.hs`
- [x] T009 [US1] Add Haddock documentation for Foldable instance in `src/Pattern/Core.hs` explaining value aggregation
- [x] T010 [US1] Add Haddock examples for foldr usage in `src/Pattern/Core.hs` demonstrating value aggregation

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. All tests should pass, demonstrating value aggregation with foldr.

---

## Phase 2: User Story 2 - Extract Values as a Flat List (Priority: P1)

**Goal**: Implement `toList` operation that extracts all values from a pattern as a flat list (standard Foldable behavior), so that developers can work with pattern values in a familiar list-based interface and use standard list-processing functions.

**Independent Test**: Convert patterns to lists and verify that: (1) the pattern's value is included, (2) all element values are included in a flat list, (3) values appear in the correct order, and (4) conversion works for all pattern structures.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T011 [P] [US2] Write unit test for toList on atomic pattern returning single-element list in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T012 [P] [US2] Write unit test for toList on pattern with multiple elements returning flat list with all values in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T013 [P] [US2] Write unit test for toList on nested pattern returning flat list with all values from all levels in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T014 [P] [US2] Write unit test for toList on pattern with integer values returning flat list of integers in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T015 [P] [US2] Write unit test verifying toList includes pattern's own value in `tests/Spec/Pattern/CoreSpec.hs`
- [x] T016 [P] [US2] Write unit test verifying toList preserves element order in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 2

- [x] T017 [US2] Implement toList using standard Foldable behavior (returns flat list) in `src/Pattern/Core.hs`
- [x] T018 [US2] Add Haddock documentation examples for toList usage showing flat list extraction in `src/Pattern/Core.hs`

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. toList should extract all values correctly as a flat list (standard Foldable behavior).

---

## Phase 2b: User Story 2b - Extract Pattern as Tuple (Priority: P2)

**Goal**: Implement `toTuple` operation that extracts a pattern as a tuple preserving structure, so that developers can work with the pattern's value and elements separately while maintaining the structural relationship.

**Independent Test**: Convert patterns to tuples and verify that: (1) the pattern's value is the first element of the tuple, (2) the pattern's elements list is the second element of the tuple, (3) the structure is preserved, and (4) conversion works for all pattern structures.

### Tests for User Story 2b

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T019 [P] [US2b] Write unit test for toTuple on atomic pattern returning tuple with value and empty list in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T020 [P] [US2b] Write unit test for toTuple on pattern with multiple elements returning tuple with value and list of element patterns in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T021 [P] [US2b] Write unit test for toTuple on nested pattern returning tuple where elements list contains nested Pattern structures in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T022 [P] [US2b] Write unit test for toTuple on pattern with integer values returning tuple with integer value and list of Pattern Int in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T023 [P] [US2b] Write unit test verifying toTuple preserves pattern structure in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 2b

- [ ] T024 [US2b] Implement toTuple to preserve structure (returns tuple) in `src/Pattern/Core.hs`
- [ ] T025 [US2b] Add Haddock documentation examples for toTuple usage showing structure preservation in `src/Pattern/Core.hs`

**Checkpoint**: At this point, User Stories 1, 2, 2a, AND 2b should all work independently. toTuple should extract patterns correctly as tuples preserving structure.

---

## Phase 3: User Story 3 - Fold with Right-Associative Operations (Priority: P1)

**Note**: This phase depends on User Story 2a (flatten) completion for testing, as foldr operations should be verified using flatten for flat list extraction.

**Goal**: Verify that foldr correctly processes values in right-to-left order and handles operations that depend on processing order. This ensures reliable right-associative folding over patterns.

**Independent Test**: Apply foldr to patterns and verify that: (1) values are processed in the correct order, (2) right-associative operations produce correct results, and (3) foldr works for all pattern structures.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T029 [P] [US3] Write unit test for foldr processing values in correct order with addition in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T030 [P] [US3] Write unit test for foldr building list in correct order with string values in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T031 [P] [US3] Write unit test for foldr processing nested pattern values in correct order in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T032 [P] [US3] Write unit test verifying foldr right-associativity property in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 3

- [ ] T033 [US3] Verify foldr implementation processes values in correct order (no code changes if already correct)
- [ ] T034 [US3] Add Haddock documentation examples for foldr order semantics in `src/Pattern/Core.hs`

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently. foldr should process values in correct order.

---

## Phase 4: User Story 4 - Fold with Left-Associative Operations (Priority: P1)

**Goal**: Implement `foldl` operation that correctly processes values in left-to-right order and handles operations that require strict left-to-right evaluation. This ensures reliable left-associative folding over patterns.

**Independent Test**: Apply foldl to patterns and verify that: (1) values are processed in left-to-right order, (2) left-associative operations produce correct results, and (3) foldl works for all pattern structures.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T035 [P] [US4] Write unit test for foldl processing values in left-to-right order with addition in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T036 [P] [US4] Write unit test for foldl computing running total correctly with integer values in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T037 [P] [US4] Write unit test for foldl processing nested pattern values in left-to-right order in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T038 [P] [US4] Write unit test verifying foldl left-associativity property in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 4

- [ ] T039 [US4] Verify foldl is correctly derived from foldr (or implement explicitly if needed) in `src/Pattern/Core.hs`
- [ ] T040 [US4] Add Haddock documentation examples for foldl usage in `src/Pattern/Core.hs`

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should all work independently. foldl should process values in left-to-right order.

---

## Phase 5: User Story 5 - Map Values to Monoids and Combine (Priority: P2)

**Goal**: Implement `foldMap` operation that maps values to monoids and combines them efficiently. This enables efficient aggregation using monoid operations without explicitly writing fold functions.

**Independent Test**: Apply foldMap to patterns and verify that: (1) values are mapped to monoids correctly, (2) monoid combination produces correct results, and (3) foldMap works for all pattern structures.

### Tests for User Story 5

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T041 [P] [US5] Write unit test for foldMap with Sum monoid on integer pattern in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T042 [P] [US5] Write unit test for foldMap with list monoid on string pattern in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T043 [P] [US5] Write unit test for foldMap with All monoid on boolean pattern in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T044 [P] [US5] Write unit test for foldMap processing nested pattern values correctly in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 5

- [ ] T045 [US5] Verify foldMap is correctly derived from foldr (or implement explicitly if needed) in `src/Pattern/Core.hs`
- [ ] T046 [US5] Add Haddock documentation examples for foldMap usage with monoids in `src/Pattern/Core.hs`

**Checkpoint**: At this point, all user stories should be independently functional. foldMap should work correctly with monoids.

---

## Phase 6: Property-Based Tests for Foldable Laws

**Goal**: Verify that the Foldable instance satisfies foldable laws and properties through property-based testing. This delivers mathematical correctness guarantees for foldable operations.

**Independent Test**: Verify foldable laws and properties using QuickCheck with `quickProperty` helper (20 test cases max for performance).

### Tests for Foldable Laws

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T047 [P] Write property-based test for toList extracting all values correctly as flat list in `tests/Spec/Pattern/Properties.hs`
- [ ] T048 [P] Write property-based test for flatten extracting all values correctly in `tests/Spec/Pattern/Properties.hs`
- [ ] T049 [P] Write property-based test for foldr processing all values correctly in `tests/Spec/Pattern/Properties.hs`
- [ ] T050 [P] Write property-based test for foldl processing all values correctly in `tests/Spec/Pattern/Properties.hs`
- [ ] T051 [P] Write property-based test for foldMap with Sum monoid producing correct results in `tests/Spec/Pattern/Properties.hs`
- [ ] T052 [P] Write property-based test for order preservation in toList and flatten in `tests/Spec/Pattern/Properties.hs`
- [ ] T053 [P] Write property-based test verifying toList p = flatten p relationship (both extract flat lists) in `tests/Spec/Pattern/Properties.hs`
- [ ] T054 [P] Write property-based test verifying foldr and foldl produce same results for commutative operations in `tests/Spec/Pattern/Properties.hs`

### Implementation for Foldable Laws

- [ ] T055 Verify Foldable instance implementation satisfies all foldable laws (no code changes if already correct)
- [ ] T056 Add Haddock documentation for foldable laws in `src/Pattern/Core.hs` with formal statements

**Checkpoint**: All property-based tests should pass, verifying foldable laws and properties hold for all pattern structures.

---

## Phase 7: Edge Cases & Comprehensive Testing

**Goal**: Ensure the Foldable instance handles all edge cases correctly, providing comprehensive coverage for atomic patterns, empty elements, singular patterns, pair patterns, extended patterns, and various value types.

**Independent Test**: Test edge cases independently to verify correct behavior for all pattern structures and value types.

### Tests for Edge Cases

- [ ] T057 [P] Write unit test for folding atomic pattern (no elements) in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T058 [P] Write unit test for folding pattern with empty elements list in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T059 [P] Write unit test for folding singular pattern (one element) in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T060 [P] Write unit test for folding pattern with many elements in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T061 [P] Write unit test for folding nested patterns with varying depths in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T062 [P] Write unit test for folding patterns with different value types (strings, integers, custom types) in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T063 [P] Write unit test for order preservation in folding operations in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T064 [P] Write unit test for deep nesting (3+ levels) in `tests/Spec/Pattern/CoreSpec.hs`

### Implementation for Edge Cases

- [ ] T065 Verify all edge cases are handled correctly by existing Foldable instance (no code changes expected)
- [ ] T066 Add Haddock documentation examples for edge cases in `src/Pattern/Core.hs`

**Checkpoint**: All edge cases should be tested and verified. The Foldable instance should handle all pattern structures correctly.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final documentation, examples, validation, and cross-cutting improvements

- [ ] T067 [P] Update module-level Haddock documentation in `src/Pattern/Core.hs` to mention Foldable instance and flatten function
- [ ] T068 [P] Add Foldable instance and flatten function to main Pattern module exports in `src/Pattern.hs` (if needed)
- [ ] T069 [P] Verify all examples in `specs/006-foldable-instance/quickstart.md` work correctly (when created)
- [ ] T070 [P] Run all tests and verify 100% pass rate for foldable instance tests
- [ ] T071 [P] Verify foldable laws hold for all test cases (property-based tests should pass)
- [ ] T072 [P] Verify test performance: property-based tests complete in <10ms using quickProperty helper
- [ ] T073 Review and update any related documentation that references Pattern typeclass instances
- [ ] T074 Code review: verify Foldable instance and flatten function follow Haskell best practices
- [ ] T075 Final validation: run full test suite and verify no regressions

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 1 (Phase 1)**: No dependencies - Pattern data type, Eq, Show, and Functor instances already exist (Features 1-4)
- **User Story 2 (Phase 2)**: Depends on User Story 1 completion - toList requires working foldr (standard Foldable behavior, flat list)
- **User Story 2a (Phase 2a)**: Depends on User Story 1 completion - flatten requires working foldr (explicit flattening)
- **User Story 2b (Phase 2b)**: No dependencies - toTuple is a simple structure-preserving extraction
- **User Story 3 (Phase 3)**: Depends on User Stories 1 and 2a completion - foldr order verification requires working foldr and flatten for testing
- **User Story 4 (Phase 4)**: Depends on User Stories 1 and 2a completion - foldl derivation requires working foldr and flatten for testing
- **User Story 5 (Phase 5)**: Depends on User Stories 1 and 2a completion - foldMap derivation requires working foldr and flatten for testing
- **Property-Based Tests (Phase 6)**: Depends on User Stories 1-5 completion - law verification requires all operations
- **Edge Cases (Phase 7)**: Depends on User Stories 1-5 completion - edge case testing requires all operations
- **Polish (Phase 8)**: Depends on all previous phases - final validation requires complete implementation

### Story Completion Order

1. **User Story 1** (MVP) - Must complete first - provides foldr foundation
2. **User Story 2** - Can start after US1 - toList depends on foldr (standard Foldable behavior, flat list)
2b. **User Story 2b** - Can start anytime - toTuple has no dependencies (simple structure-preserving extraction)
3. **User Story 2a** - Can start after US1 - flatten depends on foldr (explicit flattening)
4. **User Story 3** - Can start after US1 and US2a - order verification depends on foldr, testing uses flatten
5. **User Story 4** - Can start after US1 and US2a - foldl depends on foldr, testing uses flatten
6. **User Story 5** - Can start after US1 and US2a - foldMap depends on foldr, testing uses flatten
7. **Property-Based Tests** - Must complete after all user stories
8. **Edge Cases** - Can run in parallel with property-based tests
9. **Polish** - Must complete last

### Parallel Execution Opportunities

**Within Phase 1 (US1)**:
- All test tasks (T001-T007) can run in parallel - they're independent test cases
- Implementation tasks (T008-T010) must run sequentially after tests

**Within Phase 2 (US2)**:
- All test tasks (T011-T016) can run in parallel - they're independent test cases
- Implementation tasks (T017-T018) must run sequentially after tests

**Within Phase 3 (US3)**:
- All test tasks (T019-T022) can run in parallel - they're independent test cases
- Implementation tasks (T023-T024) must run sequentially after tests

**Within Phase 4 (US4)**:
- All test tasks (T025-T028) can run in parallel - they're independent test cases
- Implementation tasks (T029-T030) must run sequentially after tests

**Within Phase 5 (US5)**:
- All test tasks (T031-T034) can run in parallel - they're independent test cases
- Implementation tasks (T035-T036) must run sequentially after tests

**Within Phase 6 (Property-Based Tests)**:
- All test tasks (T037-T042) can run in parallel - they're independent property tests
- Implementation tasks (T043-T044) must run sequentially after tests

**Within Phase 7 (Edge Cases)**:
- All test tasks (T045-T052) can run in parallel - they're independent test cases
- Implementation tasks (T053-T054) must run sequentially after tests

**Within Phase 8 (Polish)**:
- Most tasks (T055-T062) can run in parallel - they're independent validation/documentation tasks
- Final validation (T063) must run last

## Implementation Strategy

### MVP Scope (Minimum Viable Product)

**MVP includes**: User Story 1 only (foldr implementation)
- Provides basic folding capability
- Enables value aggregation
- Foundation for other foldable operations

**MVP delivers**: Developers can fold over patterns to aggregate values using foldr.

### Incremental Delivery

1. **Increment 1 (MVP)**: User Story 1 - foldr implementation
2. **Increment 2**: User Stories 2-4 - toList, foldr order, foldl
3. **Increment 3**: User Story 5 - foldMap
4. **Increment 4**: Property-based tests and edge cases
5. **Increment 5**: Polish and final validation

### Test-Driven Development Approach

**Strategy**: Write tests first (TDD)
1. Write failing tests for each user story
2. Implement minimal code to make tests pass
3. Refactor if needed
4. Verify all tests pass

**Benefits**:
- Ensures tests actually test the implementation
- Provides clear success criteria
- Prevents over-engineering

## Success Criteria Verification

Each phase includes checkpoint verification. Final success criteria from spec:

- ✅ **SC-001**: Unit tests demonstrate folding over atomic, with elements, and nested patterns
- ✅ **SC-002**: Property-based tests verify foldr processes all values with 100% accuracy
- ✅ **SC-003**: Property-based tests verify foldl processes all values with 100% accuracy
- ✅ **SC-004**: Unit and property-based tests verify foldMap works correctly
- ✅ **SC-005**: Unit and property-based tests verify toList extracts all values correctly as a flat list (standard Foldable behavior)
- ✅ **SC-005a**: Unit and property-based tests verify flatten extracts all values correctly as flat lists
- ✅ **SC-005b**: Unit and property-based tests verify toTuple extracts patterns correctly as tuples preserving structure
- ✅ **SC-006**: Tests cover String, Int, and custom type values
- ✅ **SC-007**: Tests include nested patterns (3+ levels deep)
- ✅ **SC-008**: Tests verify order preservation
- ✅ **SC-009**: Property-based tests verify foldable laws and properties

## Notes

- **Test Performance**: All property-based tests must use `quickProperty` helper (20 test cases max) to ensure <10ms execution time
- **Examples Alignment**: Test examples should align with patterns shown in `examples/examples.md`
- **Terminology**: Use consistent terminology (atomic patterns, elements, values) throughout tests and documentation
- **Implementation Pattern**: foldr is the primary method; other methods (foldl, foldMap, toList) can be derived or implemented explicitly for efficiency
- **toList Behavior**: `toList()` follows standard Foldable behavior, extracting all values as a flat list `[a]`
- **flatten Function**: `flatten()` explicitly extracts all values as a flat list, may be equivalent to `toList` or provided for clarity
- **toTuple Function**: `toTuple()` extracts patterns as tuples `(v, [Pattern v])`, preserving structure by keeping elements as Pattern values rather than flattening
- **Relationship**: `toList p = flatten p` (both extract flat lists, standard Foldable behavior)

