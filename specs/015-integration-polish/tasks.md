# Tasks: Integration and Polish

**Input**: Design documents from `/specs/015-integration-polish/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are included for verification of export changes, documentation examples, and test coverage improvements.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Paths shown below assume single project structure

---

## Phase 1: User Story 1 - Clean Public API (Priority: P1) 🎯 MVP

**Goal**: Create a clean, well-organized public API that clearly separates public functionality from internal implementation details. Developers can import the library and access all necessary functions, types, and typeclass instances without exposing internal implementation details.

**Independent Test**: Review module exports by examining `src/Pattern/Core.hs` export list and `src/Pattern.hs` re-exports. Verify that only intended public functions are exported, internal helper functions are not accessible, and the main Pattern module provides convenient access to all core functionality.

### Implementation for User Story 1

- [x] T001 [US1] Review current exports in `src/Pattern/Core.hs` to identify all public API functions, types, and typeclass instances
- [x] T002 [US1] Identify internal helper functions in `src/Pattern/Core.hs` that should NOT be exported (e.g., `searchElements`, `indicesFromRoot`, and other implementation helpers)
- [x] T003 [US1] Create explicit export list for `src/Pattern/Core.hs` including: Pattern type, constructors, construction functions (pattern, patternWith, fromList), query functions (length, size, depth, values, value), predicate functions (anyValue, allValues, filterPatterns, findPattern, findAllPatterns, matches, contains), helper functions (flatten, toTuple), Comonad helpers (depthAt, sizeAt, indicesAt), and all typeclass instances
- [x] T004 [US1] Update `src/Pattern/Core.hs` module declaration to use explicit export list: `module Pattern.Core (exports...) where`
- [x] T005 [US1] Verify that internal helper functions are not included in export list and are not accessible through public API
- [x] T006 [US1] Review `src/Pattern.hs` to ensure it re-exports all public functionality from Pattern.Core
- [x] T007 [US1] Update `src/Pattern.hs` if needed to explicitly re-export public API from Pattern.Core and other modules (Pattern.Views, Pattern.Graph, Pattern.Morphisms)
- [x] T008 [US1] Test that importing `Pattern` provides access to all core functionality without needing to import Pattern.Core directly
- [x] T009 [US1] Verify that internal helper functions cannot be accessed through public API by attempting to use them in a test file
- [x] T010 [US1] Run tests with timeout: `timeout 60 cabal test` to verify all existing tests still pass after export changes
- [ ] T011 [US1] Git commit: "feat: organize Pattern.Core exports for clean public API - US1"

**Checkpoint**: At this point, User Story 1 should be complete. The public API is clean, internal functions are hidden, and the main Pattern module provides convenient access to all functionality.

---

## Phase 2: User Story 2 - Comprehensive Documentation (Priority: P2)

**Goal**: Provide comprehensive Haddock documentation that explains how to use the library, what each function does, and the mathematical properties that govern pattern operations. Developers can understand the library's capabilities, see usage examples, and learn about typeclass laws without reading source code.

**Independent Test**: Generate Haddock documentation using `cabal haddock` and verify that all public functions have documentation, examples compile and run correctly, and mathematical properties are documented. Check that module-level documentation explains concepts and usage patterns.

### Implementation for User Story 2

- [x] T012 [US2] Review existing Haddock documentation in `src/Pattern/Core.hs` to identify gaps in function documentation
- [x] T013 [US2] Enhance module-level documentation in `src/Pattern/Core.hs` with conceptual overview, usage patterns, and examples
- [x] T014 [P] [US2] Add or enhance Haddock documentation for construction functions (pattern, patternWith, fromList) in `src/Pattern/Core.hs` with purpose, parameters, return value, and usage examples
- [x] T015 [P] [US2] Add or enhance Haddock documentation for query functions (length, size, depth, values, value) in `src/Pattern/Core.hs` with purpose, parameters, return value, and usage examples
- [x] T016 [P] [US2] Add or enhance Haddock documentation for predicate functions (anyValue, allValues, filterPatterns, findPattern, findAllPatterns, matches, contains) in `src/Pattern/Core.hs` with purpose, parameters, return value, and usage examples
- [x] T017 [P] [US2] Add or enhance Haddock documentation for helper functions (flatten, toTuple) in `src/Pattern/Core.hs` with purpose, parameters, return value, and usage examples
- [x] T018 [P] [US2] Add or enhance Haddock documentation for Comonad helper functions (depthAt, sizeAt, indicesAt) in `src/Pattern/Core.hs` with purpose, parameters, return value, and usage examples
- [x] T019 [US2] Add or enhance Haddock documentation for Functor instance in `src/Pattern/Core.hs` explaining functor laws (identity: `fmap id = id`, composition: `fmap (f . g) = fmap f . fmap g`) and how they apply to Pattern
- [x] T020 [US2] Add or enhance Haddock documentation for Applicative instance in `src/Pattern/Core.hs` explaining applicative laws (identity, composition, homomorphism, interchange) and how they apply to Pattern
- [x] T021 [US2] Add or enhance Haddock documentation for Comonad instance in `src/Pattern/Core.hs` explaining comonad laws (extract-extend: `extract . extend f = f`, extend-extract: `extend extract = id`, extend composition: `extend f . extend g = extend (f . extend g)`) and how they apply to Pattern
- [x] T022 [US2] Add or enhance Haddock documentation for Semigroup instance in `src/Pattern/Core.hs` explaining associativity law and combination semantics
- [x] T023 [US2] Add or enhance Haddock documentation for Monoid instance in `src/Pattern/Core.hs` explaining identity laws and identity pattern semantics
- [x] T024 [US2] Add or enhance Haddock documentation for other typeclass instances (Ord, Hashable, Foldable, Traversable) in `src/Pattern/Core.hs` with explanations of their properties and laws
- [x] T025 [US2] Ensure all documentation examples use doctest-compatible format (starting with `>>>`) in `src/Pattern/Core.hs`
- [x] T026 [US2] Verify all documentation examples compile and execute correctly by running `cabal haddock` and checking for errors
- [x] T027 [US2] Update `src/Pattern.hs` module-level documentation to explain library organization and re-export structure
- [x] T028 [US2] Generate Haddock documentation: `cabal haddock` and verify all public functions have documentation
- [x] T029 [US2] Review generated Haddock documentation to ensure examples are clear and mathematical properties are explained
- [x] T030 [US2] Run tests with timeout: `timeout 60 cabal test` to verify all tests still pass after documentation updates
- [ ] T031 [US2] Git commit: "docs: add comprehensive Haddock documentation - US2"

**Checkpoint**: At this point, User Story 2 should be complete. All public functions have comprehensive documentation with examples, typeclass laws are documented, and module-level documentation explains concepts and usage patterns.

---

## Phase 3: User Story 3 - Complete Test Coverage (Priority: P3)

**Goal**: Review and improve test coverage to ensure all typeclass laws are verified, edge cases are handled, and the library behaves correctly under all conditions. Developers can trust that the library is correct and reliable.

**Independent Test**: Run the test suite using `cabal test` and verify that all typeclass laws are tested with property-based tests, edge cases are covered, and test coverage metrics meet quality standards. Check that all public API functions have test coverage.

### Implementation for User Story 3

- [ ] T032 [US3] Review existing test coverage in `tests/Spec/Pattern/CoreSpec.hs` to identify gaps in public API function coverage
- [ ] T033 [US3] Review existing property-based tests in `tests/Spec/Pattern/Properties.hs` to identify missing typeclass law tests
- [ ] T034 [US3] Verify Functor law tests (identity: `fmap id = id`, composition: `fmap (f . g) = fmap f . fmap g`) exist in `tests/Spec/Pattern/Properties.hs` and add if missing
- [ ] T035 [US3] Verify Applicative law tests (identity, composition, homomorphism, interchange) exist in `tests/Spec/Pattern/Properties.hs` and add if missing
- [ ] T036 [US3] Verify Comonad law tests (extract-extend, extend-extract, extend composition) exist in `tests/Spec/Pattern/Properties.hs` and add if missing
- [ ] T037 [US3] Verify Semigroup associativity law test exists in `tests/Spec/Pattern/Properties.hs` and add if missing
- [ ] T038 [US3] Verify Monoid identity law tests (`mempty <> p = p`, `p <> mempty = p`) exist in `tests/Spec/Pattern/Properties.hs` and add if missing
- [ ] T039 [US3] Verify Hashable consistency test (equal patterns produce equal hashes) exists in `tests/Spec/Pattern/Properties.hs` and add if missing
- [ ] T040 [P] [US3] Add edge case tests for empty patterns in `tests/Spec/Pattern/CoreSpec.hs` (if not already covered)
- [ ] T041 [P] [US3] Add edge case tests for single element patterns in `tests/Spec/Pattern/CoreSpec.hs` (if not already covered)
- [ ] T042 [P] [US3] Add edge case tests for deeply nested patterns (100+ levels) in `tests/Spec/Pattern/CoreSpec.hs` (if not already covered)
- [ ] T043 [P] [US3] Add edge case tests for large patterns (1000+ nodes) in `tests/Spec/Pattern/CoreSpec.hs` (if not already covered)
- [ ] T044 [US3] Review test coverage metrics using coverage tooling (if available) to identify any uncovered public API functions
- [ ] T045 [US3] Add unit tests for any public API functions that lack test coverage in `tests/Spec/Pattern/CoreSpec.hs`
- [ ] T046 [US3] Ensure all property-based tests use bounded generators to prevent slow test execution in `tests/Spec/Pattern/Properties.hs`
- [ ] T047 [US3] Run full test suite with timeout: `timeout 60 cabal test` to verify all tests pass
- [ ] T048 [US3] Verify test execution time is under 1 minute total
- [ ] T049 [US3] Review test output to ensure all typeclass laws pass and edge cases are handled correctly
- [ ] T050 [US3] Git commit: "test: improve test coverage for typeclass laws and edge cases - US3"

**Checkpoint**: At this point, User Story 3 should be complete. All typeclass laws are verified with property-based tests, edge cases are explicitly tested, and test coverage meets quality standards.

---

## Phase 4: Polish & Cross-Cutting Concerns

**Purpose**: Final polish and validation that affects all user stories

- [ ] T051 [P] Review all export lists to ensure consistency between `src/Pattern/Core.hs` and `src/Pattern.hs`
- [ ] T052 [P] Verify that all documentation examples in `src/Pattern/Core.hs` compile and execute correctly using doctest (if configured)
- [ ] T053 [P] Review quickstart.md in `specs/015-integration-polish/quickstart.md` and validate that it accurately reflects the current API
- [ ] T054 [P] Generate final Haddock documentation: `cabal haddock` and review for completeness
- [ ] T055 Run full test suite with timeout: `timeout 60 cabal test` to verify all tests pass
- [ ] T056 Verify that all public API functions are accessible through main Pattern module without importing Pattern.Core directly
- [ ] T057 Verify that internal helper functions cannot be accessed through public API
- [ ] T058 Review test coverage metrics to ensure all public API functions have coverage
- [ ] T059 Verify that all typeclass laws are tested and pass
- [ ] T060 Verify that all edge cases are explicitly tested
- [ ] T061 Review code for any remaining polish opportunities (formatting, consistency, etc.)
- [ ] T062 Git commit: "chore: finalize integration and polish feature"

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 1 (Phase 1)**: No dependencies - can start immediately. This is the MVP and must be completed first as it affects how all other functionality is exposed.
- **User Story 2 (Phase 2)**: Depends on User Story 1 completion - documentation should match the finalized API exports
- **User Story 3 (Phase 3)**: Depends on User Story 1 and User Story 2 completion - tests should cover the finalized API and match documented behavior
- **Polish (Phase 4)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start immediately - No dependencies on other stories. This is the MVP.
- **User Story 2 (P2)**: Must wait for User Story 1 completion - Documentation should match finalized API
- **User Story 3 (P3)**: Must wait for User Story 1 and User Story 2 completion - Tests should cover finalized API and match documentation

### Within Each User Story

- Review existing code before making changes
- Make changes incrementally (exports first, then documentation, then tests)
- Verify changes don't break existing functionality
- Test after each major change
- Story complete before moving to next priority

### Parallel Opportunities

- Within User Story 2: Documentation tasks for different function categories can run in parallel (T014-T018, T019-T024)
- Within User Story 3: Edge case tests for different scenarios can run in parallel (T040-T043)
- Within Polish phase: Review and validation tasks can run in parallel (T051-T054)
- Different user stories cannot run in parallel due to dependencies (US2 depends on US1, US3 depends on US1 and US2)

---

## Parallel Example: User Story 2

```bash
# Launch all documentation tasks for different function categories together:
Task: "Add or enhance Haddock documentation for construction functions (pattern, patternWith, fromList) in src/Pattern/Core.hs"
Task: "Add or enhance Haddock documentation for query functions (length, size, depth, values, value) in src/Pattern/Core.hs"
Task: "Add or enhance Haddock documentation for predicate functions (anyValue, allValues, filterPatterns, findPattern, findAllPatterns, matches, contains) in src/Pattern/Core.hs"
Task: "Add or enhance Haddock documentation for helper functions (flatten, toTuple) in src/Pattern/Core.hs"
Task: "Add or enhance Haddock documentation for Comonad helper functions (depthAt, sizeAt, indicesAt) in src/Pattern/Core.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: User Story 1 (Clean Public API)
2. **STOP and VALIDATE**: Test that exports are clean and API is accessible
3. Deploy/demo if ready

### Incremental Delivery

1. Add User Story 1 → Test independently → Validate API is clean (MVP!)
2. Add User Story 2 → Test independently → Validate documentation is comprehensive
3. Add User Story 3 → Test independently → Validate test coverage is complete
4. Each story adds value without breaking previous stories

### Sequential Strategy (Required)

Due to dependencies:
- User Story 1 must complete before User Story 2 (documentation needs finalized API)
- User Story 2 must complete before User Story 3 (tests need finalized API and documentation)

**Recommended approach**:
1. Complete User Story 1 (Clean Public API) - MVP
2. Complete User Story 2 (Comprehensive Documentation)
3. Complete User Story 3 (Complete Test Coverage)
4. Complete Polish phase

---

## Notes

- [P] tasks = different files or different sections, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- **Git commit after each user story completion** (see tasks T011, T031, T050, T062)
- Stop at any checkpoint to validate story independently
- This is a polish feature - no new functionality, only organization and documentation improvements
- All changes must preserve existing functionality (no breaking changes)
- Verify tests pass after each major change

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

# Generate Haddock documentation:
cabal haddock
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Use profiling tools if tests consistently slow

