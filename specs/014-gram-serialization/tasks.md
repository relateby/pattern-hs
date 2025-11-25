# Tasks: Gram Serialization Library

**Input**: Design documents from `/specs/014-gram-serialization/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are included as they are explicitly required by the specification (FR-017, SC-003) and constitution (Testing Standards NON-NEGOTIABLE).

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Library project**: `libs/gram/src/`, `libs/gram/tests/` at repository root
- Paths follow the multi-library mono-repo structure

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [x] T001 Create test data directory structure for tree-sitter-gram corpus in libs/gram/test-data/tree-sitter-gram-corpus/
- [x] T002 [P] Verify gram library structure exists (libs/gram/src/Gram.hs, libs/gram/src/Gram/Serialize.hs, libs/gram/src/Gram/Parse.hs)
- [x] T003 [P] Verify test structure exists (libs/gram/tests/Test.hs, libs/gram/tests/Spec/Gram/)
- [x] T004 [P] Review gram.cabal dependencies and ensure pattern and subject libraries are available

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before parsing-related user stories can be implemented

**⚠️ CRITICAL**: Parsing library evaluation (US4) must be completed before US2 and US3 can begin. However, US1 (serialization) can proceed in parallel since it doesn't depend on parsing library choice.

### Phase 2A: Parsing Library Evaluation (US4) - Blocks US2 and US3

- [x] T005 [US4] Research tree-sitter-gram Haskell bindings availability and stability
- [x] T006 [US4] Research Parsec library for gram notation syntax coverage
- [x] T007 [US4] Research Megaparsec library for gram notation syntax coverage
- [x] T008 [US4] Create evaluation criteria document in specs/014-gram-serialization/research.md (syntax coverage, performance, maintainability, integration complexity, stability)
- [x] T009 [US4] Evaluate tree-sitter-gram Haskell bindings against criteria (FFI complexity, binding stability, integration requirements)
- [x] T010 [US4] Evaluate Parsec against criteria (syntax coverage, performance, maintainability)
- [x] T011 [US4] Evaluate Megaparsec against criteria (syntax coverage, performance, maintainability, error messages)
- [x] T012 [US4] Create comparison document with pros and cons for each option in specs/014-gram-serialization/research.md
- [x] T013 [US4] Document parsing library selection decision with clear rationale in specs/014-gram-serialization/research.md
- [x] T014 [US4] Update gram.cabal with selected parsing library dependency

**Checkpoint**: Parsing library selected and documented. US2 and US3 can now begin. US1 can proceed in parallel.

---

## Phase 3: User Story 1 - Serialize Pattern Subject to Gram Notation (Priority: P1) 🎯 MVP

**Goal**: Implement serialization function that converts Pattern Subject data structures to gram notation string format, supporting all value types, nested patterns, and relationship patterns.

**Independent Test**: Create Pattern Subject structures, call toGram function, verify output matches expected gram notation syntax. Can be tested independently without parsing functionality.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [x] T015 [P] [US1] Create unit test for simple subject serialization in libs/gram/tests/Spec/Gram/SerializeSpec.hs
- [x] T016 [P] [US1] Create unit test for subject with standard value types in libs/gram/tests/Spec/Gram/SerializeSpec.hs
- [x] T017 [P] [US1] Create unit test for subject with extended value types in libs/gram/tests/Spec/Gram/SerializeSpec.hs
- [x] T018 [P] [US1] Create unit test for nested pattern serialization in libs/gram/tests/Spec/Gram/SerializeSpec.hs
- [x] T019 [P] [US1] Create unit test for relationship pattern serialization in libs/gram/tests/Spec/Gram/SerializeSpec.hs
- [x] T020 [P] [US1] Create unit test for anonymous subject serialization in libs/gram/tests/Spec/Gram/SerializeSpec.hs
- [x] T021 [P] [US1] Create edge case tests (deep nesting, large property records, special characters) in libs/gram/tests/Spec/Gram/SerializeSpec.hs

### Implementation for User Story 1

- [x] T022 [US1] Implement escapeString helper function in libs/gram/src/Gram/Serialize.hs
- [x] T023 [US1] Implement quoteSymbol helper function in libs/gram/src/Gram/Serialize.hs
- [x] T024 [US1] Implement serializeValue function for standard value types (VInteger, VDecimal, VBoolean, VString, VSymbol) in libs/gram/src/Gram/Serialize.hs
- [x] T025 [US1] Implement serializeValue function for extended value types (VTaggedString, VArray, VMap, VRange, VMeasurement) in libs/gram/src/Gram/Serialize.hs
- [x] T026 [US1] Implement serializePropertyRecord function in libs/gram/src/Gram/Serialize.hs
- [x] T027 [US1] Implement serializeSubject function (identity, labels, properties) in libs/gram/src/Gram/Serialize.hs
- [x] T028 [US1] Implement serializePatternElements function for nested patterns in libs/gram/src/Gram/Serialize.hs
- [x] T029 [US1] Implement toGram function (main serialization entry point) in libs/gram/src/Gram/Serialize.hs
- [x] T030 [US1] Add comprehensive Haddock documentation with examples to libs/gram/src/Gram/Serialize.hs
- [x] T031 [US1] Run tests with timeout: `timeout 60 cabal test gram-test` to verify all User Story 1 tests pass
- [ ] T032 [US1] Git commit: "feat: implement Pattern Subject serialization to gram notation - US1"

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Serialization works for all value types, nested patterns, and relationship patterns.

---

## Phase 4: User Story 2 - Parse Gram Notation to Pattern Subject (Priority: P1)

**Goal**: Implement parsing function that converts gram notation string format to Pattern Subject data structures, with comprehensive error handling for invalid syntax.

**Independent Test**: Provide gram notation strings, call fromGram function, verify output matches expected Pattern Subject structures. Can be tested independently with manually created gram notation strings.

**Dependencies**: Requires Phase 2A (parsing library selection) to be complete.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T033 [P] [US2] Create unit test for simple subject parsing in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T034 [P] [US2] Create unit test for subject with standard value types parsing in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T035 [P] [US2] Create unit test for subject with extended value types parsing in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T036 [P] [US2] Create unit test for nested pattern parsing in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T037 [P] [US2] Create unit test for relationship pattern parsing in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T038 [P] [US2] Create unit test for anonymous subject parsing in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T039 [P] [US2] Create unit test for parse error handling (invalid syntax, malformed structure) in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T040 [P] [US2] Create edge case tests (special characters, Unicode, inconsistent whitespace) in libs/gram/tests/Spec/Gram/ParseSpec.hs

### Implementation for User Story 2

- [x] T041 [US2] Implement ParseError type in libs/gram/src/Gram/Parse.hs
- [x] T042 [US2] Implement unescapeString helper function in libs/gram/src/Gram/Parse.hs
- [x] T043 [US2] Implement parseSymbol parser function in libs/gram/src/Gram/Parse.hs
- [x] T044 [US2] Implement parseValue parser for standard value types in libs/gram/src/Gram/Parse.hs
- [x] T045 [US2] Implement parseValue parser for extended value types in libs/gram/src/Gram/Parse.hs
- [x] T046 [US2] Implement parsePropertyRecord parser function in libs/gram/src/Gram/Parse.hs
- [x] T047 [US2] Implement parseSubject parser function (identity, labels, properties) in libs/gram/src/Gram/Parse.hs
- [x] T048 [US2] Implement parsePatternElements parser function for nested patterns in libs/gram/src/Gram/Parse.hs
- [x] T049 [US2] Implement comment stripping logic (line comments `//`, end-of-line comments) in libs/gram/src/Gram/Parse.hs
- [x] T050 [US2] Implement fromGram function (main parsing entry point) in libs/gram/src/Gram/Parse.hs
- [x] T051 [US2] Add comprehensive Haddock documentation with examples to libs/gram/src/Gram/Parse.hs
- [x] T052 [US2] Run tests with timeout: `timeout 60 cabal test gram-test` to verify all User Story 2 tests pass
- [ ] T053 [US2] Git commit: "feat: implement gram notation parsing to Pattern Subject - US2"

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Parsing works for all value types, nested patterns, and relationship patterns with proper error handling.

---

## Phase 5: User Story 3 - Support All Tree-Sitter-Gram Syntax (Priority: P1)

**Goal**: Integrate tree-sitter-gram test corpus and verify complete syntax support through comprehensive testing.

**Independent Test**: Run test corpus from tree-sitter-gram repository, verify all test cases pass for both serialization and parsing operations, confirm all syntax features are handled correctly.

**Dependencies**: Requires Phase 3 (US1) and Phase 4 (US2) to be complete.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T054 [US3] Examine tree-sitter-gram repository structure to locate test corpus files
- [x] T055 [US3] Copy or integrate tree-sitter-gram test corpus files to libs/gram/test-data/tree-sitter-gram-corpus/
- [x] T056 [P] [US3] Create test corpus loading function in libs/gram/tests/Spec/Gram/CorpusSpec.hs
- [x] T057 [P] [US3] Create test for parsing all corpus files in libs/gram/tests/Spec/Gram/CorpusSpec.hs
- [x] T058 [P] [US3] Create test for round-trip conversion (serialize then parse) for all corpus files in libs/gram/tests/Spec/Gram/CorpusSpec.hs
- [x] T059 [US3] Create test for comment handling (line comments, end-of-line comments) in libs/gram/tests/Spec/Gram/CorpusSpec.hs
- [x] T060 [US3] Create test for all value types round-trip conversion in libs/gram/tests/Spec/Gram/CorpusSpec.hs
- [x] T061 [US3] Create test for complex nested pattern structures round-trip conversion in libs/gram/tests/Spec/Gram/CorpusSpec.hs
- [x] T062 [US3] Create test for edge cases from corpus (empty patterns, deeply nested structures, large patterns) in libs/gram/tests/Spec/Gram/CorpusSpec.hs

### Implementation for User Story 3

- [x] T063 [US3] Implement corpus file loading and test execution logic in libs/gram/tests/Spec/Gram/CorpusSpec.hs
- [ ] T064 [US3] Run corpus tests and analyze failures (Gap Analysis) - *Current Status: Failures identified (Path syntax)*
- [ ] T064a [US3] Implement Path syntax parsing support in libs/gram/src/Gram/Parse.hs (Cypher-like arrows)
- [ ] T064b [US3] Fix node attribute parsing issues identified in corpus tests
- [ ] T064c [US3] Verify all corpus files parse successfully (after fixes)
- [ ] T065 [US3] Verify all corpus files pass round-trip conversion (fix any serialization/parsing mismatches)
- [ ] T066 [US3] Verify comment handling works correctly for all corpus files
- [ ] T067 [US3] Verify all value types are correctly handled in corpus files
- [ ] T068 [US3] Verify edge cases are correctly handled (empty patterns, deep nesting, large property records)
- [ ] T069 [US3] Add comprehensive error reporting for corpus test failures in libs/gram/tests/Spec/Gram/CorpusSpec.hs
- [ ] T070 [US3] Run tests with timeout: `timeout 60 cabal test gram-test` to verify all User Story 3 tests pass
- [ ] T071 [US3] Git commit: "feat: integrate tree-sitter-gram test corpus and verify complete syntax support - US3"

**Checkpoint**: At this point, all user stories should be independently functional. The library handles all syntax features from tree-sitter-gram with comprehensive test coverage.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories, final validation, and documentation

- [ ] T072 [P] Add property-based tests for round-trip conversion (serialize then parse preserves structure) in libs/gram/tests/Spec/Gram/Properties.hs
- [ ] T073 [P] Add performance tests (serialization <1s for 10K+ nodes, parsing <1s for 1MB files) in libs/gram/tests/Spec/Gram/PerformanceSpec.hs
- [ ] T074 [P] Update main Gram module documentation in libs/gram/src/Gram.hs with usage examples
- [ ] T075 [P] Verify all public APIs have comprehensive Haddock documentation
- [ ] T076 [P] Review and update quickstart.md examples to match implementation
- [ ] T077 Code cleanup and refactoring (remove any TODOs, improve code organization)
- [ ] T078 Verify error messages are clear and actionable for all parse error scenarios
- [ ] T079 Run quickstart.md validation (ensure all examples compile and execute correctly)
- [ ] T080 Run full test suite with timeout: `timeout 60 cabal test gram-test` to verify all tests pass
- [ ] T081 Generate Haddock documentation: `cabal haddock lib:gram` and verify completeness
- [ ] T082 Git commit: "docs: finalize gram serialization library feature"

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2A)**: Depends on Setup completion - BLOCKS US2 and US3
- **User Story 1 (Phase 3)**: Can start after Setup (Phase 1) - No dependencies on parsing library
- **User Story 2 (Phase 4)**: Depends on Foundational (Phase 2A) completion - Requires parsing library selection
- **User Story 3 (Phase 5)**: Depends on Phase 3 (US1) and Phase 4 (US2) completion - Requires both serialization and parsing
- **Polish (Phase 6)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Setup (Phase 1) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2A) - Depends on parsing library selection (US4)
- **User Story 3 (P1)**: Can start after US1 and US2 - Depends on both serialization and parsing
- **User Story 4 (P2)**: Can start after Setup (Phase 1) - Blocks US2 and US3, but can proceed in parallel with US1

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Helper functions before main functions
- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- US1 (serialization) can proceed in parallel with US4 (parsing library evaluation)
- All tests for a user story marked [P] can run in parallel
- Helper functions within a story marked [P] can run in parallel
- Different user stories can be worked on in parallel by different team members (subject to dependencies)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: T015 - Create unit test for simple subject serialization
Task: T016 - Create unit test for subject with standard value types
Task: T017 - Create unit test for subject with extended value types
Task: T018 - Create unit test for nested pattern serialization
Task: T019 - Create unit test for relationship pattern serialization
Task: T020 - Create unit test for anonymous subject serialization
Task: T021 - Create edge case tests

# Launch helper functions for User Story 1 together (after tests):
Task: T022 - Implement escapeString helper function
Task: T023 - Implement quoteSymbol helper function
Task: T024 - Implement serializeValue for standard types
Task: T025 - Implement serializeValue for extended types
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2A: Parsing Library Evaluation (US4) - Can proceed in parallel with US1
3. Complete Phase 3: User Story 1 (Serialization)
4. **STOP and VALIDATE**: Test User Story 1 independently
5. Deploy/demo if ready (serialization works, parsing can be added later)

### Incremental Delivery

1. Complete Setup → Foundation ready
2. Add User Story 1 (Serialization) → Test independently → Deploy/Demo (MVP with serialization!)
3. Complete Phase 2A (Parsing Library Evaluation) → Document decision
4. Add User Story 2 (Parsing) → Test independently → Deploy/Demo (Full bidirectional conversion!)
5. Add User Story 3 (Test Corpus) → Test independently → Deploy/Demo (Complete syntax support!)
6. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup together
2. Once Setup is done:
   - Developer A: User Story 1 (Serialization) - Can start immediately
   - Developer B: User Story 4 (Parsing Library Evaluation) - Can start immediately, blocks US2/US3
3. Once US4 is complete:
   - Developer A: Continue US1 or move to US2 (Parsing)
   - Developer B: User Story 2 (Parsing) - Can start after US4
4. Once US1 and US2 are complete:
   - Developer A: User Story 3 (Test Corpus)
   - Developer B: Polish & Documentation

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- **Git commit after each user story completion** (see tasks T032, T053, T071, T082)
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- US1 (serialization) can proceed in parallel with US4 (parsing library evaluation) since serialization doesn't depend on parsing library choice
- US2 and US3 require parsing library selection (US4) to be complete

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

### Test Execution Commands

```bash
# First run after implementation (with timeout):
timeout 60 cabal test gram-test

# Normal verification (with timeout):
timeout 30 cabal test gram-test

# Run specific test suite:
timeout 30 cabal test gram-test --test-option="--match SerializeSpec"
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Use profiling tools if tests consistently slow

