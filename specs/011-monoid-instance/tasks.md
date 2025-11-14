# Tasks: Monoid Instance for Pattern

**Input**: Design documents from `/specs/011-monoid-instance/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are REQUIRED per specification (FR-009) and constitution (Testing Standards NON-NEGOTIABLE).

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Source: `src/Pattern/Core.hs`
- Tests: `tests/Spec/Pattern/CoreSpec.hs` and `tests/Spec/Pattern/Properties.hs`

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and verification of prerequisites

- [x] T001 Verify Pattern Core type exists and is stable in src/Pattern/Core.hs
- [x] T002 Verify Pattern Semigroup instance exists and is stable in src/Pattern/Core.hs
- [x] T003 Verify Pattern construction functions exist (pattern, patternWith, fromList) in src/Pattern/Core.hs
- [x] T004 Verify Pattern Eq instance exists for testing equality in src/Pattern/Core.hs
- [x] T005 [P] Verify test infrastructure (hspec, QuickCheck) is configured in tests/Test.hs

**Checkpoint**: Prerequisites verified - ready to proceed with implementation

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

**Note**: For this feature, the foundational work (evaluation and design) has been completed in research.md and data-model.md. The remaining foundational task is to ensure the Semigroup instance is stable and accessible.

- [x] T006 Verify Semigroup instance is accessible and working correctly in src/Pattern/Core.hs

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Evaluate Use Cases for Identity Pattern (Priority: P1) ✅ COMPLETE

**Goal**: Understand whether an identity pattern (`mempty`) provides clear value for Semigroup operations

**Independent Test**: Use cases documented in research.md, semantic alignment verified, distinct value confirmed

**Status**: ✅ **COMPLETE** - Use case evaluation completed in research.md (Phase 0). Three concrete use cases identified:
1. Pattern accumulation with empty initial state
2. Optional pattern construction
3. Pattern initialization and default values

**Note**: This user story was completed during the research phase. No implementation tasks needed.

---

## Phase 4: User Story 2 - Design Monoid Identity Semantics (Priority: P2) ✅ COMPLETE

**Goal**: Define clear semantics for what `mempty` represents

**Independent Test**: Identity semantics documented, identity laws verified, alignment with decorated sequence model confirmed

**Status**: ✅ **COMPLETE** - Identity semantics designed in research.md and data-model.md (Phase 1). Identity pattern structure defined: `mempty = Pattern { value = mempty, elements = [] }`. Identity laws verified to hold.

**Note**: This user story was completed during the design phase. No implementation tasks needed.

---

## Phase 5: User Story 3 - Implement Monoid Instance (Priority: P3) 🎯 MVP

**Goal**: Implement Monoid instance for Pattern, enabling identity patterns and standard Monoid combinators

**Independent Test**: Monoid instance compiles with `Monoid v` constraint, `mempty` returns correct structure, standard Monoid combinators work correctly

### Tests for User Story 3 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total. See Testing Performance Guidelines section below.

- [x] T007 [P] [US3] Write unit test for identity pattern structure in tests/Spec/Pattern/CoreSpec.hs
- [x] T008 [P] [US3] Write unit test for left identity law (mempty <> p = p) in tests/Spec/Pattern/CoreSpec.hs
- [x] T009 [P] [US3] Write unit test for right identity law (p <> mempty = p) in tests/Spec/Pattern/CoreSpec.hs
- [x] T010 [P] [US3] Write unit test for mconcat with empty list in tests/Spec/Pattern/CoreSpec.hs
- [x] T011 [P] [US3] Write unit test for mconcat with list of patterns in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 3

- [x] T012 [US3] Implement Monoid instance with mempty in src/Pattern/Core.hs
- [x] T013 [US3] Add Haddock documentation for Monoid instance in src/Pattern/Core.hs
- [x] T014 [US3] Add examples demonstrating identity patterns in Haddock documentation in src/Pattern/Core.hs
- [x] T015 [US3] Verify Monoid instance compiles with Monoid v constraint
- [x] T016 [US3] Verify mempty returns pattern with mempty value and empty elements

**Checkpoint**: At this point, User Story 3 should be fully functional. Monoid instance implemented, identity pattern works, standard combinators (mconcat) work correctly.

---

## Phase 6: User Story 4 - Verify Monoid Laws and Consistency (Priority: P3)

**Goal**: Comprehensive test coverage verifying identity laws, consistency with Semigroup, and edge case handling

**Independent Test**: All property-based tests for identity laws pass, all unit tests for consistency pass, all edge case tests pass

### Tests for User Story 4 ⚠️

> **NOTE: These tests verify the implementation from User Story 3**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [x] T017 [P] [US4] Write property-based test for left identity law (mempty <> p = p) in tests/Spec/Pattern/Properties.hs
- [x] T018 [P] [US4] Write property-based test for right identity law (p <> mempty = p) in tests/Spec/Pattern/Properties.hs
- [x] T019 [P] [US4] Write property-based test for identity pattern structure in tests/Spec/Pattern/Properties.hs
- [x] T020 [P] [US4] Write property-based test for consistency with Semigroup in tests/Spec/Pattern/Properties.hs
- [x] T021 [P] [US4] Write unit test for identity with atomic patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T022 [P] [US4] Write unit test for identity with patterns having elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T023 [P] [US4] Write unit test for identity with nested patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T024 [P] [US4] Write unit test for identity with different value types (String, Sum, Product, All, Any) in tests/Spec/Pattern/CoreSpec.hs
- [x] T025 [P] [US4] Write unit test for mconcat with list containing only mempty in tests/Spec/Pattern/CoreSpec.hs
- [x] T026 [P] [US4] Write integration test for standard Monoid combinators in tests/Spec/Pattern/CoreSpec.hs

**Checkpoint**: At this point, User Story 4 should be complete. All tests pass, comprehensive coverage achieved.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and final validation

- [x] T027 [P] Update module-level documentation in src/Pattern/Core.hs to include Monoid instance
- [x] T028 [P] Verify all tests pass with timeout: `timeout 60 cabal test`
- [x] T029 [P] Run quickstart.md validation - verify all examples work correctly
- [x] T030 Code review: Verify Monoid instance follows Haskell best practices
- [x] T031 Verify Haddock documentation builds correctly and examples compile
- [x] T032 Verify type constraint (Monoid v) is properly enforced

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: 
  - User Story 1 (P1): ✅ COMPLETE - Evaluation done in research phase
  - User Story 2 (P2): ✅ COMPLETE - Design done in design phase
  - User Story 3 (P3): Depends on Foundational phase completion - Implementation needed
  - User Story 4 (P3): Depends on User Story 3 completion - Testing needed
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: ✅ COMPLETE - No implementation needed
- **User Story 2 (P2)**: ✅ COMPLETE - No implementation needed
- **User Story 3 (P3)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 4 (P3)**: Depends on User Story 3 completion - Tests verify implementation

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All test tasks for User Story 3 marked [P] can run in parallel
- All test tasks for User Story 4 marked [P] can run in parallel
- Polish tasks marked [P] can run in parallel

---

## Parallel Example: User Story 3

```bash
# Launch all tests for User Story 3 together:
Task: "Write unit test for identity pattern structure in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for left identity law in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for right identity law in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for mconcat with empty list in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for mconcat with list of patterns in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Parallel Example: User Story 4

```bash
# Launch all property-based tests together:
Task: "Write property-based test for left identity law in tests/Spec/Pattern/Properties.hs"
Task: "Write property-based test for right identity law in tests/Spec/Pattern/Properties.hs"
Task: "Write property-based test for identity pattern structure in tests/Spec/Pattern/Properties.hs"
Task: "Write property-based test for consistency with Semigroup in tests/Spec/Pattern/Properties.hs"

# Launch all unit tests together:
Task: "Write unit test for identity with atomic patterns in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for identity with patterns having elements in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for identity with nested patterns in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for identity with different value types in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for mconcat with list containing only mempty in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write integration test for standard Monoid combinators in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 3 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 5: User Story 3 (Implementation)
4. **STOP and VALIDATE**: Test User Story 3 independently
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 3 (Implementation) → Test independently → Deploy/Demo (MVP!)
3. Add User Story 4 (Verification) → Test independently → Deploy/Demo
4. Add Polish → Final validation → Deploy/Demo
5. Each phase adds value without breaking previous work

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 3 implementation
   - Developer B: User Story 4 tests (can start after US3 tests written)
3. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- User Stories 1 and 2 are already complete (evaluation and design phases)
- Focus implementation effort on User Stories 3 and 4

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

# Run specific test file:
timeout 30 cabal test --test-option="--match=Pattern.CoreSpec"
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Use profiling tools if tests consistently slow

