---
description: "Tasks for implementing subject identity and serialization in gram-hs"
---

# Tasks: Subject Identity and Serialization

**Input**: Design documents from `/specs/020-subject-serialization/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/, quickstart.md

**Tests**: Tests are INCLUDED as fundamental tasks given the rigorous nature of the project.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [ ] T001 Create `libs/gram/tests/Spec/Gram/SerializeSpec.hs` skeleton if needed
- [ ] T002 Create `libs/gram/tests/Spec/Gram/ParseSpec.hs` skeleton if needed (likely exists)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T003 Verify `libs/gram/src/Gram/Transform.hs` state monad readiness (conceptually)

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Round-trip Serialization (Priority: P1) 🎯 MVP

**Goal**: Implement robust serialization/parsing cycle for explicit subjects.

**Independent Test**: Serialize `Subject` to string, parse back, assert equality.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> **PERFORMANCE**: Always use timeouts (`timeout 60 cabal test`).

- [ ] T004 [US1] Add property test for round-trip serialization in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
- [ ] T005 [P] [US1] Add unit test for special character escaping in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`

### Implementation for User Story 1

- [ ] T006 [US1] Update `libs/gram/src/Gram/Serialize.hs` to handle all escaping cases (quotes, backslashes)
- [ ] T007 [US1] Ensure `libs/gram/src/Gram/Serialize.hs` produces valid gram syntax for all value types
- [ ] T008 [US1] Run tests with timeout: `timeout 60 cabal test` to verify round-trip
- [ ] T009 [US1] Git commit: "feat: implement round-trip serialization - US1"

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently

---

## Phase 4: User Story 2 - Handling Anonymous Subjects (Priority: P2)

**Goal**: Automatically assign unique identifiers (e.g., `#1`) to anonymous subjects during parsing.

**Independent Test**: Parse gram string with multiple `()` and verify unique, sequential IDs.

### Tests for User Story 2

- [ ] T010 [US2] Add unit test for anonymous node parsing in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [ ] T011 [P] [US2] Add unit test for anonymous path parsing `()-[]->()` in `libs/gram/tests/Spec/Gram/ParseSpec.hs`

### Implementation for User Story 2

- [ ] T012 [US2] Modify `libs/gram/src/Gram/Transform.hs` to use `State Int` monad for ID generation
- [ ] T013 [US2] Implement `transformIdentifier` with `#<N>` format logic in `libs/gram/src/Gram/Transform.hs`
- [ ] T014 [US2] Update `libs/gram/src/Gram/Parse.hs` `fromGram` to run the stateful transformation
- [ ] T015 [US2] Verify generated IDs follow `#<N>` format (illegal as unquoted identifier)
- [ ] T016 [US2] Run tests with timeout: `timeout 60 cabal test` to verify anonymous ID generation
- [ ] T017 [US2] Git commit: "feat: implement anonymous subject ID generation - US2"

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently

---

## Phase 5: User Story 3 - Identity Preservation (Priority: P3)

**Goal**: Ensure specific IDs are preserved exactly during round-trip.

**Independent Test**: Serialize subject with ID "user-123", verify "user-123" in output.

### Tests for User Story 3

- [ ] T018 [US3] Add unit test for explicit ID preservation in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`

### Implementation for User Story 3

- [ ] T019 [US3] Verify `libs/gram/src/Gram/Serialize.hs` correctly handles explicit IDs (should already be covered, but explicit check needed)
- [ ] T020 [US3] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 3 tests pass
- [ ] T021 [US3] Git commit: "feat: verify identity preservation - US3"

**Checkpoint**: All user stories should now be independently functional

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T022 Run quickstart validation from `specs/020-subject-serialization/quickstart.md`
- [ ] T023 Run full test suite with timeout: `timeout 60 cabal test`
- [ ] T024 Git commit: "docs: finalize subject serialization feature"

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2 → P3)
- **Polish (Final Phase)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational (Phase 2) - Modifies Transform logic, but conceptually independent
- **User Story 3 (P3)**: Can start after Foundational (Phase 2) - Independent check

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Models before services
- Services before endpoints
- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational tasks marked [P] can run in parallel (within Phase 2)
- Once Foundational phase completes, all user stories can start in parallel (if team capacity allows)
- All tests for a user story marked [P] can run in parallel
- Models within a story marked [P] can run in parallel
- Different user stories can be worked on in parallel by different team members

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together (if tests requested):
Task: "Add unit test for special character escaping in libs/gram/tests/Spec/Gram/SerializeSpec.hs"

# Launch all models for User Story 1 together:
Task: "Update libs/gram/src/Gram/Serialize.hs to handle all escaping cases"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test User Story 1 independently
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Deploy/Demo
4. Add User Story 3 → Test independently → Deploy/Demo
5. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1
   - Developer B: User Story 2
   - Developer C: User Story 3
3. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- **Git commit after each user story completion** (see tasks T009, T017, T021)
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
```

