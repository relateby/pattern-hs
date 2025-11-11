# Tasks: Construction Functions

**Input**: Design documents from `/specs/004-construction-functions/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅, quickstart.md ✅

**Tests**: Tests are REQUIRED per Constitution (Testing Standards are NON-NEGOTIABLE). All functions must have comprehensive tests.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: User Story 1 - Create Atomic Patterns Conveniently (Priority: P1) 🎯 MVP

**Goal**: Implement `pattern` function that creates atomic patterns (patterns with no elements) from a single value, providing a convenient alternative to verbose record syntax.

**Independent Test**: Can be fully tested by calling `pattern` with various value types and verifying that the resulting pattern has the correct value and an empty elements list. This delivers a more convenient way to create the fundamental building blocks of pattern structures.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T001 [P] [US1] Add test for creating atomic pattern with string value using `pattern` function in tests/Spec/Pattern/CoreSpec.hs
- [x] T002 [P] [US1] Add test for creating atomic pattern with integer value using `pattern` function in tests/Spec/Pattern/CoreSpec.hs
- [x] T003 [P] [US1] Add test for creating atomic pattern with custom type value using `pattern` function in tests/Spec/Pattern/CoreSpec.hs
- [x] T004 [US1] Add test verifying `pattern` function produces patterns functionally identical to record syntax in tests/Spec/Pattern/CoreSpec.hs
- [x] T005 [US1] Add property-based test for functional equivalence: `pattern v == Pattern { value = v, elements = [] }` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 1

- [x] T006 [US1] Implement `pattern :: v -> Pattern v` function in src/Pattern/Core.hs
- [x] T007 [US1] Add Haddock documentation for `pattern` function with examples in src/Pattern/Core.hs
- [x] T008 [US1] Export `pattern` from Pattern.Core module in src/Pattern/Core.hs
- [x] T009 [US1] Re-export `pattern` from main Pattern module in src/Pattern.hs

**Checkpoint**: ✅ At this point, User Story 1 should be fully functional and testable independently - developers can create atomic patterns using `pattern` function.

---

## Phase 2: User Story 2 - Create Patterns with Elements Conveniently (Priority: P1)

**Goal**: Implement `patternWith` function that creates patterns with elements from a value and a list of pattern elements, providing a convenient alternative to verbose record syntax.

**Independent Test**: Can be fully tested by calling `patternWith` with a value and a list of pattern elements, then verifying that the resulting pattern has the correct value and all elements are accessible in the correct order. This delivers a more convenient way to create patterns with any number of elements.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T010 [P] [US2] Add test for creating singular pattern (one element) using `patternWith` in tests/Spec/Pattern/CoreSpec.hs
- [x] T011 [P] [US2] Add test for creating pair pattern (two elements) using `patternWith` in tests/Spec/Pattern/CoreSpec.hs
- [x] T012 [P] [US2] Add test for creating extended pattern (many elements) using `patternWith` in tests/Spec/Pattern/CoreSpec.hs
- [x] T013 [P] [US2] Add test for empty list in `patternWith` (should produce atomic pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T014 [US2] Add test verifying `patternWith` preserves element order in tests/Spec/Pattern/CoreSpec.hs
- [x] T015 [US2] Add test verifying `patternWith` produces patterns functionally identical to record syntax in tests/Spec/Pattern/CoreSpec.hs
- [x] T016 [US2] Add property-based test for functional equivalence: `patternWith v ps == Pattern { value = v, elements = ps }` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 2

- [x] T017 [US2] Implement `patternWith :: v -> [Pattern v] -> Pattern v` function in src/Pattern/Core.hs
- [x] T018 [US2] Add Haddock documentation for `patternWith` function with examples in src/Pattern/Core.hs
- [x] T019 [US2] Export `patternWith` from Pattern.Core module in src/Pattern/Core.hs
- [x] T020 [US2] Re-export `patternWith` from main Pattern module in src/Pattern.hs

**Checkpoint**: ✅ At this point, User Stories 1 AND 2 should both work independently - developers can create atomic patterns and patterns with elements using constructor functions.

---

## Phase 3: User Story 3 - Create Patterns from Lists of Values (Priority: P1)

**Goal**: Implement `fromList` function that creates patterns from lists of raw values by converting each value to an atomic pattern, providing a convenient way to create patterns from external data sources.

**Independent Test**: Can be fully tested by calling `fromList` with a decoration value and a list of values, then verifying that the resulting pattern has the correct decoration and all values are converted to atomic pattern elements in order. This delivers a convenient way to create patterns from raw data.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T021 [P] [US3] Add test for creating pattern from list of strings using `fromList` in tests/Spec/Pattern/CoreSpec.hs
- [x] T022 [P] [US3] Add test for creating pattern from list of integers using `fromList` in tests/Spec/Pattern/CoreSpec.hs
- [x] T023 [P] [US3] Add test for creating pattern from list of custom types using `fromList` in tests/Spec/Pattern/CoreSpec.hs
- [x] T024 [P] [US3] Add test for empty list in `fromList` (should produce atomic pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T025 [US3] Add test verifying `fromList` preserves value order in tests/Spec/Pattern/CoreSpec.hs
- [x] T026 [US3] Add test verifying `fromList` converts each value to atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T027 [US3] Add test verifying `fromList` produces patterns functionally identical to `patternWith decoration (map pattern values)` in tests/Spec/Pattern/CoreSpec.hs
- [x] T028 [US3] Add property-based test for functional equivalence: `fromList decoration values == patternWith decoration (map pattern values)` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 3

- [x] T029 [US3] Implement `fromList :: v -> [v] -> Pattern v` function in src/Pattern/Core.hs
- [x] T030 [US3] Add Haddock documentation for `fromList` function with examples in src/Pattern/Core.hs
- [x] T031 [US3] Export `fromList` from Pattern.Core module in src/Pattern/Core.hs
- [x] T032 [US3] Re-export `fromList` from main Pattern module in src/Pattern.hs

**Checkpoint**: ✅ At this point, User Stories 1, 2, AND 3 should all work independently - developers can create patterns using all three constructor functions.

---

## Phase 4: User Story 4 - Support All Common Pattern Structures (Priority: P2)

**Goal**: Ensure all constructor functions work correctly for all common pattern structures (atomic patterns with 0 elements, singular patterns with 1 element, pairs with 2 elements, and extended patterns with many elements) through comprehensive edge case testing.

**Independent Test**: Can be fully tested by creating patterns with 0, 1, 2, and many elements using all constructors, then verifying each produces the expected structure. This delivers comprehensive coverage of all common pattern creation scenarios.

### Tests for User Story 4

> **NOTE: These tests verify edge cases and comprehensive coverage**

- [x] T033 [P] [US4] Add test for `patternWith` with 0 elements (atomic pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T034 [P] [US4] Add test for `patternWith` with 1 element (singular pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T035 [P] [US4] Add test for `patternWith` with 2 elements (pair pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T036 [P] [US4] Add test for `patternWith` with many elements (extended pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T037 [P] [US4] Add test for `fromList` with 0 values (atomic pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T038 [P] [US4] Add test for `fromList` with 1 value (singular pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T039 [P] [US4] Add test for `fromList` with 2 values (pair pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T040 [P] [US4] Add test for `fromList` with many values (extended pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T041 [US4] Add test for nested patterns with all constructors in tests/Spec/Pattern/CoreSpec.hs
- [x] T042 [US4] Add test for all value types (strings, integers, custom types) with all constructors in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 4

- [x] T043 [US4] Verify all edge cases are handled correctly in existing implementations (no code changes needed if tests pass)

**Checkpoint**: ✅ At this point, all constructor functions should work correctly for all common pattern structures with comprehensive test coverage.

---

## Phase 5: Polish & Cross-Cutting Concerns

**Purpose**: Final documentation, examples, and validation

- [x] T044 [P] Verify all Haddock documentation examples compile and run correctly
- [x] T045 [P] Update examples/examples.md with constructor function examples if needed
- [x] T046 [P] Run quickstart.md validation - verify all examples work correctly
- [x] T047 Verify all tests pass: `cabal test`
- [x] T048 Verify code builds without warnings: `cabal build`
- [x] T049 Generate and review Haddock documentation: `cabal haddock`
- [x] T050 Review all function exports are correct in src/Pattern.hs
- [x] T051 [P] Re-check terminology in codebase: search for tree-like terms (leaf, node, parent, child, children) in values, variables, and documentation; verify they are either acceptable (e.g., string values like `"node"`, implementation detail comments) or replace with appropriate terminology (e.g., use definition-style examples for singular patterns instead of parent/child)

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 1 (Phase 1)**: No dependencies - can start immediately (Pattern data type already exists)
- **User Story 2 (Phase 2)**: Can start after User Story 1 (uses `pattern` in examples) but can be implemented independently
- **User Story 3 (Phase 3)**: Depends on User Story 1 (uses `pattern` in implementation) and User Story 2 (uses `patternWith` in implementation)
- **User Story 4 (Phase 4)**: Depends on all previous user stories (tests all constructors)
- **Polish (Phase 5)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Independent - implements `pattern` function
- **User Story 2 (P1)**: Independent implementation, but examples use `pattern` (can use record syntax in tests)
- **User Story 3 (P1)**: Depends on User Story 1 (`pattern`) and User Story 2 (`patternWith`) for implementation
- **User Story 4 (P2)**: Depends on all previous stories (comprehensive testing)

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Implementation tasks follow test tasks
- Export tasks follow implementation tasks
- Story complete before moving to next priority

### Parallel Opportunities

- All test tasks marked [P] within a user story can run in parallel
- User Stories 1 and 2 can be implemented in parallel (US2 examples can use record syntax instead of `pattern`)
- All polish tasks marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Add test for creating atomic pattern with string value using `pattern` function in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating atomic pattern with integer value using `pattern` function in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating atomic pattern with custom type value using `pattern` function in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Parallel Example: User Story 2

```bash
# Launch all tests for User Story 2 together:
Task: "Add test for creating singular pattern (one element) using `patternWith` in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating pair pattern (two elements) using `patternWith` in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating extended pattern (many elements) using `patternWith` in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for empty list in `patternWith` (should produce atomic pattern) in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Parallel Example: User Story 3

```bash
# Launch all tests for User Story 3 together:
Task: "Add test for creating pattern from list of strings using `fromList` in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating pattern from list of integers using `fromList` in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating pattern from list of custom types using `fromList` in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for empty list in `fromList` (should produce atomic pattern) in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: User Story 1 (pattern function)
2. **STOP and VALIDATE**: Test User Story 1 independently
3. Deploy/demo if ready

### Incremental Delivery

1. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
2. Add User Story 2 → Test independently → Deploy/Demo
3. Add User Story 3 → Test independently → Deploy/Demo (depends on US1 and US2)
4. Add User Story 4 → Test comprehensively → Deploy/Demo
5. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Developer A: User Story 1 (pattern function)
2. Developer B: User Story 2 (patternWith function) - can use record syntax in tests initially
3. Once US1 and US2 complete: Developer C: User Story 3 (fromList function)
4. All developers: User Story 4 (comprehensive testing)

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- User Story 3 implementation depends on US1 and US2, but tests can be written independently
- All functions are simple pure functions - no complex dependencies
- Follow Constitution requirements: comprehensive tests, Haddock documentation, code quality

