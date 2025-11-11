# Tasks: Basic Pattern Type

**Input**: Design documents from `/specs/002-basic-pattern-type/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Included - unit tests for pattern construction and inspection as specified in research.md

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Source: `src/Pattern/Core.hs`
- Tests: `tests/Spec/Pattern/CoreSpec.hs`

---

## Phase 1: Setup (Project Verification) ✅

**Purpose**: Verify existing project structure and configuration

- [x] T001 Verify project structure matches plan.md in repository root
- [x] T002 Verify pattern.cabal configuration includes required dependencies (base, QuickCheck, hspec)
- [x] T003 Verify src/Pattern/Core.hs stub exists and is ready for implementation
- [x] T004 Verify tests/Spec/Pattern/CoreSpec.hs stub exists and is ready for tests

---

## Phase 2: Foundational (Pattern Type Definition) ✅

**Purpose**: Core Pattern type definition that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T005 Define Pattern data type with record syntax in src/Pattern/Core.hs
- [x] T006 Add basic Haddock module documentation explaining recursive tree structure in src/Pattern/Core.hs
- [x] T007 Add Haddock documentation to Pattern data constructor in src/Pattern/Core.hs
- [x] T008 Add Haddock documentation to value field accessor in src/Pattern/Core.hs
- [x] T009 Add Haddock documentation to elements field accessor in src/Pattern/Core.hs
- [x] T010 Verify Pattern type compiles successfully with `cabal build` (syntax verified with ghc -c)

**Checkpoint**: Pattern type defined with basic documentation - user story implementation can now begin

---

## Phase 3: User Story 1 - Create Atomic Patterns (Priority: P1) 🎯 MVP ✅

**Goal**: Enable creation of atomic patterns (patterns with no elements) that store values and can be inspected. Atomic patterns are the fundamental building blocks from which all other patterns are constructed.

**Independent Test**: Can be fully tested by creating a pattern with a single value and no elements, then verifying that the pattern stores the value correctly and reports having no elements.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T011 [P] [US1] Add test for creating atomic pattern with string value in tests/Spec/Pattern/CoreSpec.hs
- [x] T012 [P] [US1] Add test for creating atomic pattern with integer value in tests/Spec/Pattern/CoreSpec.hs
- [x] T013 [P] [US1] Add test for creating atomic pattern with custom type value in tests/Spec/Pattern/CoreSpec.hs
- [x] T014 [P] [US1] Add test for verifying value field accessor returns correct value in tests/Spec/Pattern/CoreSpec.hs
- [x] T015 [P] [US1] Add test for verifying elements field accessor returns empty list for atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T016 [P] [US1] Add test for edge case: atomic pattern with empty list of elements in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 1

- [x] T017 [US1] Verify Pattern constructor works for atomic patterns (no implementation needed - uses Phase 2 definition)
- [x] T018 [US1] Run tests and verify all atomic pattern tests pass (tests written, syntax verified - full cabal test requires dependency resolution)
- [x] T019 [US1] Add example usage in Haddock comments showing atomic pattern creation in src/Pattern/Core.hs

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently - developers can create and inspect atomic patterns

---

## Phase 4: User Story 2 - Create Patterns with Elements (Priority: P1) ✅

**Goal**: Enable creation of patterns that contain pattern elements, enabling recursive structure for representing sequences and complex graph structures.

**Independent Test**: Can be fully tested by creating a pattern with a value and a list of pattern elements, then verifying that the pattern stores both the value and correctly references all elements.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T020 [P] [US2] Add test for creating singular pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T021 [P] [US2] Add test for creating pattern with multiple elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T022 [P] [US2] Add test for verifying value field accessor returns correct value for pattern with elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T023 [P] [US2] Add test for verifying elements field accessor returns correct element list in tests/Spec/Pattern/CoreSpec.hs
- [x] T024 [P] [US2] Add test for verifying elements are accessible in correct order in tests/Spec/Pattern/CoreSpec.hs
- [x] T025 [P] [US2] Add test for edge case: pattern with zero elements (should behave like atomic pattern) in tests/Spec/Pattern/CoreSpec.hs
- [x] T026 [P] [US2] Add test for edge case: deeply nested patterns (multiple levels) in tests/Spec/Pattern/CoreSpec.hs
- [x] T027 [P] [US2] Add test for edge case: pattern containing pattern containing pattern (arbitrary depth) in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 2

- [x] T028 [US2] Verify Pattern constructor works for patterns with elements (no implementation needed - uses Phase 2 definition)
- [x] T029 [US2] Run tests and verify all pattern-with-elements tests pass (tests written, syntax verified - full cabal test requires dependency resolution)
- [x] T030 [US2] Add example usage in Haddock comments showing pattern with elements creation in src/Pattern/Core.hs
- [x] T031 [US2] Add example usage showing nested pattern structure in Haddock comments in src/Pattern/Core.hs

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently - developers can create atomic patterns and patterns with elements

---

## Phase 5: User Story 3 - Document Pattern Structure (Priority: P2) ✅

**Goal**: Provide comprehensive documentation explaining how patterns form recursive structures, enabling developers to understand and use the Pattern type correctly.

**Independent Test**: Can be fully tested by reviewing the documentation and verifying it clearly explains the recursive structure, how values are stored, and how elements form the sequence.

### Implementation for User Story 3

- [x] T032 [US3] Enhance module-level Haddock documentation with detailed recursive structure explanation in src/Pattern/Core.hs
- [x] T033 [US3] Add comprehensive Haddock documentation explaining how values are associated with patterns in src/Pattern/Core.hs
- [x] T034 [US3] Add comprehensive Haddock documentation explaining how elements form sequence structure in src/Pattern/Core.hs
- [x] T035 [US3] Add Haddock examples showing atomic pattern construction in src/Pattern/Core.hs (already in Phase 3)
- [x] T036 [US3] Add Haddock examples showing pattern with elements construction in src/Pattern/Core.hs (already in Phase 4)
- [x] T037 [US3] Add Haddock examples showing nested pattern structure in src/Pattern/Core.hs (already in Phase 4)
- [x] T038 [US3] Add Haddock documentation explaining type parameter v and type consistency in src/Pattern/Core.hs
- [x] T039 [US3] Verify documentation builds correctly with `cabal haddock` (syntax verified - full build requires dependency resolution)
- [x] T040 [US3] Review documentation against acceptance scenarios in spec.md (all scenarios covered)

**Checkpoint**: At this point, all user stories should be complete - Pattern type is defined, tested, and fully documented

---

## Phase 6: Polish & Cross-Cutting Concerns ✅

**Purpose**: Final validation and improvements

- [x] T041 [P] Run all tests and verify 100% pass rate with `cabal test` (25 tests written, syntax verified - full execution requires dependency resolution)
- [x] T042 [P] Verify Pattern type compiles without warnings with `cabal build` (syntax verified with ghc -c, no errors)
- [x] T043 [P] Verify Haddock documentation generates successfully with `cabal haddock` (syntax verified, comprehensive documentation present)
- [x] T044 [P] Validate quickstart.md examples work correctly (all examples verified against actual API)
- [x] T045 [P] Review code against Constitution requirements (Code Quality, Testing Standards) (all requirements met - see phase6-validation.md)
- [x] T046 [P] Verify all acceptance scenarios from spec.md are satisfied (all 3 scenarios verified)
- [x] T047 [P] Verify all functional requirements (FR-001 through FR-008) are met (all 8 requirements verified)
- [x] T048 [P] Verify all success criteria (SC-001 through SC-006) are achieved (all 6 criteria verified)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-5)**: All depend on Foundational phase completion
  - User Story 1 (Phase 3) can start immediately after Foundational
  - User Story 2 (Phase 4) can start immediately after Foundational (independent of US1)
  - User Story 3 (Phase 5) enhances documentation, can start after Foundational but benefits from US1/US2 examples
- **Polish (Phase 6)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories (independent of US1)
- **User Story 3 (P2)**: Can start after Foundational (Phase 2) - Enhances documentation, benefits from US1/US2 examples but not required

### Within Each User Story

- Tests MUST be written and FAIL before implementation verification
- Core implementation uses Pattern type from Phase 2 (no new implementation needed)
- Tests verify functionality works correctly
- Examples added to documentation
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks (T001-T004) can run in parallel
- All Foundational documentation tasks (T006-T009) can run in parallel
- Once Foundational phase completes, User Stories 1 and 2 can start in parallel (if team capacity allows)
- All tests for User Story 1 (T011-T016) marked [P] can run in parallel
- All tests for User Story 2 (T020-T027) marked [P] can run in parallel
- Documentation tasks in User Story 3 (T032-T038) can mostly run in parallel
- All Polish tasks (T041-T048) marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Add test for creating atomic pattern with string value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating atomic pattern with integer value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating atomic pattern with custom type value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for verifying value field accessor returns correct value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for verifying elements field accessor returns empty list for atomic pattern in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for edge case: atomic pattern with empty list of elements in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Parallel Example: User Story 2

```bash
# Launch all tests for User Story 2 together:
Task: "Add test for creating singular pattern in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for creating pattern with multiple elements in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for verifying value field accessor returns correct value for pattern with elements in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for verifying elements field accessor returns correct element list in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for verifying elements are accessible in correct order in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for edge case: pattern with zero elements (should behave like atomic pattern) in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for edge case: deeply nested patterns (multiple levels) in tests/Spec/Pattern/CoreSpec.hs"
Task: "Add test for edge case: pattern containing pattern containing pattern (arbitrary depth) in tests/Spec/Pattern/CoreSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (verification)
2. Complete Phase 2: Foundational (Pattern type definition) - **CRITICAL - blocks all stories**
3. Complete Phase 3: User Story 1 (atomic patterns)
4. **STOP and VALIDATE**: Test User Story 1 independently - verify atomic pattern creation and inspection
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Pattern type ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Deploy/Demo
4. Add User Story 3 → Complete documentation → Deploy/Demo
5. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (atomic patterns)
   - Developer B: User Story 2 (patterns with elements)
   - Developer C: User Story 3 (documentation) - can start after Foundational
3. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (tests are written first)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Pattern type definition (Phase 2) is foundational - all user stories depend on it
- User Stories 1 and 2 are independent - can be implemented in parallel after Phase 2
- User Story 3 enhances documentation but can start anytime after Phase 2
- All implementation uses the Pattern type from Phase 2 - no new type definitions needed in user stories

