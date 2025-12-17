# Tasks: Codefence Multiline Strings

**Input**: Design documents from `/specs/024-codefence-strings/`
**Prerequisites**: plan.md ✓, spec.md ✓, research.md ✓, data-model.md ✓, contracts/ ✓

**Tests**: Included per Constitution testing standards (NON-NEGOTIABLE) and SC-005 success criteria.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Project structure**: `libs/gram/` within mono-repo
- **Source**: `libs/gram/src/Gram/`
- **Tests**: `libs/gram/tests/Spec/Gram/`

---

## Phase 1: Setup

**Purpose**: Verify existing project structure and dependencies

- [x] T001 Verify gram library builds cleanly: `cabal build lib:gram`
- [x] T002 Verify existing tests pass: `timeout 60 cabal test gram-test --test-show-details=always`
- [x] T003 Review existing parseString and parseTaggedString in `libs/gram/src/Gram/Parse.hs`
- [x] T004 Review existing serializeValue in `libs/gram/src/Gram/Serialize.hs`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure needed by multiple user stories

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T005 Add codefenceThreshold constant (120) in `libs/gram/src/Gram/Serialize.hs`
- [x] T006 Implement parseFencedContent helper function in `libs/gram/src/Gram/Parse.hs`

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Plain Codefence String (Priority: P1) 🎯 MVP

**Goal**: Parse plain triple-backtick codefences as VString values

**Independent Test**: Parse ```` ```\nHello\n``` ```` and verify `VString "Hello\n"` is returned

### Tests for User Story 1

> **NOTE: Write tests FIRST, ensure they FAIL before implementation**

- [x] T007 [P] [US1] Add test: parse plain codefence basic in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T008 [P] [US1] Add test: parse plain codefence empty content in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T009 [P] [US1] Add test: parse plain codefence with backticks in content in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T010 [P] [US1] Add test: parse plain codefence error on unclosed fence in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T011 [US1] Run tests with timeout to verify they FAIL: `timeout 60 cabal test gram-test --test-show-details=always`

### Implementation for User Story 1

- [x] T012 [US1] Implement parseFencedString function with Haddock docs in `libs/gram/src/Gram/Parse.hs`
- [x] T013 [US1] Integrate parseFencedString into parseString chain in `libs/gram/src/Gram/Parse.hs`
- [x] T014 [US1] Run tests with timeout: `timeout 60 cabal test gram-test --test-show-details=always` to verify all User Story 1 tests pass
- [x] T015 [US1] Git commit: "feat(gram): add plain codefence string parsing - US1"

**Checkpoint**: Plain codefence parsing is fully functional and testable independently

---

## Phase 4: User Story 2 - Tagged Codefence String (Priority: P1)

**Goal**: Parse tagged triple-backtick codefences as VTaggedString values

**Independent Test**: Parse ```` ```md\n# Title\n``` ```` and verify `VTaggedString "md" "# Title\n"` is returned

### Tests for User Story 2

- [x] T016 [P] [US2] Add test: parse tagged codefence basic in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T017 [P] [US2] Add test: parse tagged codefence empty content in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T018 [P] [US2] Add test: parse tagged codefence various tags (md, json, html) in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T019 [P] [US2] Add test: parse tagged codefence error on missing tag in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T020 [US2] Run tests with timeout to verify they FAIL: `timeout 60 cabal test gram-test --test-show-details=always`

### Implementation for User Story 2

- [x] T021 [US2] Implement parseTaggedFencedString function with Haddock docs in `libs/gram/src/Gram/Parse.hs`
- [x] T022 [US2] Refactor parseTaggedString to try parseTaggedFencedString first in `libs/gram/src/Gram/Parse.hs`
- [x] T023 [US2] Run tests with timeout: `timeout 60 cabal test gram-test --test-show-details=always` to verify all User Story 2 tests pass
- [x] T024 [US2] Git commit: "feat(gram): add tagged codefence string parsing - US2"

**Checkpoint**: Tagged codefence parsing is fully functional and testable independently

---

## Phase 5: User Story 3 - Integration with Property Records (Priority: P2)

**Goal**: Verify codefence strings work seamlessly in property records

**Independent Test**: Parse a node with mixed property types including codefence and verify all values are correct

### Tests for User Story 3

- [x] T025 [P] [US3] Add test: parse node with codefence property in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T026 [P] [US3] Add test: parse node with multiple codefence properties in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T027 [P] [US3] Add test: parse node with mixed value types including codefence in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T028 [US3] Run tests with timeout: `timeout 60 cabal test gram-test --test-show-details=always` to verify integration tests pass

### Implementation for User Story 3

- [x] T029 [US3] Verify parseValue correctly routes to codefence parsers in `libs/gram/src/Gram/Parse.hs`
- [x] T030 [US3] Test parsing examples/markdown.gram succeeds: add test in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [x] T031 [US3] Git commit: "feat(gram): verify codefence integration with property records - US3"

**Checkpoint**: Codefence strings work in all property record contexts

---

## Phase 6: User Story 4 - Automatic Codefence Serialization (Priority: P2)

**Goal**: Serialize strings >120 characters using codefence format automatically

**Independent Test**: Serialize a VString with 121 characters and verify codefence output format

### Tests for User Story 4

- [x] T032 [P] [US4] Add test: serialize short VString (<= 120 chars) uses quotes in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
- [x] T033 [P] [US4] Add test: serialize long VString (> 120 chars) uses codefence in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
- [x] T034 [P] [US4] Add test: serialize VString exactly 120 chars uses quotes in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
- [x] T035 [P] [US4] Add test: serialize short VTaggedString uses inline format in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
- [x] T036 [P] [US4] Add test: serialize long VTaggedString uses codefence format in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
- [x] T037 [US4] Run tests with timeout to verify they FAIL: `timeout 60 cabal test gram-test --test-show-details=always`

### Implementation for User Story 4

- [x] T038 [US4] Modify serializeValue for VString with length check in `libs/gram/src/Gram/Serialize.hs`
- [x] T039 [US4] Modify serializeValue for VTaggedString with length check in `libs/gram/src/Gram/Serialize.hs`
- [x] T040 [US4] Add Haddock documentation for codefence serialization behavior in `libs/gram/src/Gram/Serialize.hs`
- [x] T041 [US4] Run tests with timeout: `timeout 60 cabal test gram-test --test-show-details=always` to verify all serialization tests pass
- [x] T042 [US4] Git commit: "feat(gram): add automatic codefence serialization for long strings - US4"

**Checkpoint**: Serialization correctly uses codefence format based on length threshold

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Round-trip validation, edge cases, and documentation

### Round-Trip Property Tests

- [ ] T043 [P] Add property test: round-trip VString through parse/serialize in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
- [ ] T044 [P] Add property test: round-trip VTaggedString through parse/serialize in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`

### Edge Case Coverage

- [ ] T045 [P] Add test: codefence with exactly two consecutive backticks in content in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
- [ ] T046 [P] Add test: serializer escapes newlines in short strings correctly in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`

### Backward Compatibility Verification

- [ ] T047 Run full existing test suite to verify no regressions: `timeout 60 cabal test all --test-show-details=always`
- [ ] T048 Verify examples/markdown.gram parses and round-trips correctly

### Documentation

- [ ] T049 Update Gram.Parse module header documentation with codefence syntax in `libs/gram/src/Gram/Parse.hs`
- [ ] T050 Update Gram.Serialize module header documentation with threshold behavior in `libs/gram/src/Gram/Serialize.hs`

### Final Validation

- [ ] T051 Run full test suite with timeout: `timeout 60 cabal test all --test-show-details=always`
- [ ] T052 Validate quickstart.md examples work correctly
- [ ] T053 Git commit: "docs(gram): finalize codefence strings feature documentation"

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - verification only
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 and US2 can proceed in parallel (both P1, independent parsing)
  - US3 depends on US1 and US2 (integration testing)
  - US4 is independent (serialization, no parsing dependency)
- **Polish (Phase 7)**: Depends on US1-US4 completion

### User Story Dependencies

- **User Story 1 (P1)**: Depends on Foundational (parseFencedContent) - No dependencies on other stories
- **User Story 2 (P1)**: Depends on Foundational (parseFencedContent) - No dependencies on other stories
- **User Story 3 (P2)**: Depends on US1 and US2 (integration uses both parsers)
- **User Story 4 (P2)**: Depends only on Foundational (codefenceThreshold) - Can run parallel to US1/US2/US3

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Implementation follows test-driven pattern
- Commit after story completion

### Parallel Opportunities

**Phase 2 (Foundational)**:
- T005 and T006 can run in parallel (different files)

**Phase 3-4 (US1 + US2 - both P1)**:
- US1 and US2 can be implemented in parallel (different functions, share parseFencedContent)
- All test tasks within each story (T007-T010, T016-T019) can run in parallel

**Phase 5-6 (US3 + US4 - both P2)**:
- US4 can start as soon as Foundational is complete (parallel to US1/US2)
- US3 must wait for US1/US2 but can run parallel to US4

**Phase 7 (Polish)**:
- T043-T046 can all run in parallel (different test files/areas)

---

## Parallel Example: User Story 1 + User Story 2

```bash
# After Foundational is complete, launch in parallel:

# Developer A: User Story 1 (Plain Codefence)
Task: T007-T010 (tests in parallel)
Task: T012-T015 (implementation)

# Developer B: User Story 2 (Tagged Codefence)  
Task: T016-T019 (tests in parallel)
Task: T021-T024 (implementation)

# Developer C: User Story 4 (Serialization) - Can start immediately after Foundational
Task: T032-T037 (tests in parallel)
Task: T038-T042 (implementation)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (verify existing code)
2. Complete Phase 2: Foundational (parseFencedContent, threshold constant)
3. Complete Phase 3: User Story 1 (plain codefence parsing)
4. **STOP and VALIDATE**: Test US1 independently with examples/markdown.gram (will partially work)
5. Demonstrate plain codefence parsing capability

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Commit (MVP for parsing!)
3. Add User Story 2 → Test independently → Commit (full parsing!)
4. Add User Story 3 → Test independently → Commit (integration verified!)
5. Add User Story 4 → Test independently → Commit (full round-trip!)
6. Each story adds value without breaking previous stories

### Recommended Order (Single Developer)

1. Phase 1: Setup (T001-T004)
2. Phase 2: Foundational (T005-T006)
3. Phase 3: US1 Plain Codefence (T007-T015)
4. Phase 4: US2 Tagged Codefence (T016-T024)
5. Phase 6: US4 Serialization (T032-T042) - before US3 since independent
6. Phase 5: US3 Integration (T025-T031) - integration tests last
7. Phase 7: Polish (T043-T053)

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- Tests use Hspec; property tests use QuickCheck
- **Git commit after each user story completion** (T015, T024, T031, T042, T053)
- Timeout: Always use `timeout 60` for test runs to catch performance issues
- Backward compatibility: Verify existing tests pass after each change

## Testing Performance Guidelines

### Test Execution Timeouts

- **All test runs**: Use `timeout 60 cabal test` to prevent hanging
- **Full test suite**: Should complete in under 1 minute total

### Test Execution Commands

```bash
# Standard test run with timeout:
timeout 60 cabal test gram-test --test-show-details=always

# Full mono-repo test run:
timeout 60 cabal test all --test-show-details=always

# Specific test pattern (if needed):
timeout 60 cabal test gram-test --test-option=--match="/codefence/"
```

