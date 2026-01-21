# Tasks: Separation of Container and Content Parsing

**Input**: Design documents from `specs/030-separate-container-content-parsing/`
**Prerequisites**: plan.md (required), spec.md (required for user stories)

**Organization**: Tasks are grouped by phase and user story to enable incremental implementation and testing.

## Format: `[ID] [P?] [Story?] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and testing skeleton

- [X] T001 Create testing skeleton for new parsing and serialization API in `libs/gram/tests/Spec/Gram/ParseSpec.hs` and `libs/gram/tests/Spec/Gram/SerializeSpec.hs`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Refactoring CST and base parsing logic to support containers

- [X] T002 Update `libs/gram/src/Gram/CST.hs` to explicitly represent the Gram document structure (optional header + list of patterns)
- [X] T003 Refactor the top-level parser in `libs/gram/src/Gram/Parse.hs` to use the updated CST structure and ensure it correctly identifies a leading bare record as a potential header

---

## Phase 3: User Story 1 - Parsing Multiple Patterns as a Stream (Priority: P1)

**Goal**: Parse a Gram document containing multiple top-level patterns and receive them as a list of distinct patterns.

**Independent Test**: Verify `fromGramList "(a) (b)"` returns `[Pattern (Subject "a" ...), Pattern (Subject "b" ...)]`.

### Implementation for User Story 1

- [X] T004 [US1] Implement `fromGramList` in `libs/gram/src/Gram/Parse.hs` which extracts all patterns from a parsed document
- [X] T005 [P] [US1] Add comprehensive unit tests for `fromGramList` in `libs/gram/tests/Spec/Gram/ParseSpec.hs` covering multiple patterns, empty documents, and whitespace/comments

---

## Phase 4: User Story 2 - Explicit Header Metadata Handling (Priority: P1)

**Goal**: Explicitly separate a leading bare record in a Gram document from the subsequent patterns.

**Independent Test**: Verify `fromGramWithHeader "{v:1} (a)"` returns `(Just {v:1}, [(a)])`.

### Implementation for User Story 2

- [X] T006 [US2] Implement `fromGramWithHeader` in `libs/gram/src/Gram/Parse.hs` to separate the leading bare record from the pattern list
- [X] T007 [P] [US2] Add unit tests for `fromGramWithHeader` in `libs/gram/tests/Spec/Gram/ParseSpec.hs` covering documents with and without headers, and bare records that are NOT at the start

---

## Phase 5: User Story 3 - Serializing Multiple Patterns and Headers (Priority: P2)

**Goal**: Serialize a list of patterns, optionally including a header record, into a valid Gram document.

**Independent Test**: Verify `toGramWithHeader {v:1} [(a)]` produces `{v:1}\n(a)`.

### Implementation for User Story 3

- [X] T008 [US3] Implement `toGramList` in `libs/gram/src/Gram/Serialize.hs` for space/newline-delimited pattern serialization
- [X] T009 [US3] Implement `toGramWithHeader` in `libs/gram/src/Gram/Serialize.hs` for serialization with a leading header record
- [X] T010 [P] [US3] Add unit and property-based tests for new serialization functions in `libs/gram/tests/Spec/Gram/SerializeSpec.hs` and `libs/gram/tests/Spec/Gram/RoundtripSpec.hs`

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Documentation updates, backward compatibility, and final integration

- [X] T011 [P] Update `libs/gram/src/Gram.hs` to export all new container-aware parsing and serialization functions
- [X] T012 Update `fromGram` and `toGram` in `libs/gram/src/Gram/Parse.hs` and `libs/gram/src/Gram/Serialize.hs` to maintain backward compatibility using the `Gram.Root` mapping
- [X] T013 [P] Update `docs/reference/PORTING-GUIDE.md` to include container-aware parsing and serialization in Phase 3
- [X] T014 [P] Update `docs/reference/features/gram-serialization.md` to document the new API functions
- [X] T015 [P] Update `docs/reference/SPECIFICATION.md` to include container support in the Gram library feature table
- [X] T016 Run full test suite including round-trip tests and verify SC-001 (performance with 10k patterns)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies
- **Foundational (Phase 2)**: Depends on Phase 1 completion
- **User Stories (Phase 3-4)**: Both depend on Phase 2 completion
- **Serialization (Phase 5)**: Depends on US1/US2 being stable for testing round-trips
- **Polish (Final Phase)**: Depends on all user stories being complete

### User Story Dependencies

- **US1 & US2**: Both P1, can be developed in parallel after Foundational phase if resources permit.
- **US3**: Depends on understanding the parsing behavior of US1/US2.

---

## Implementation Strategy

### MVP First (User Story 1 & 2)

1. Complete Phase 1 & 2.
2. Complete User Story 1 (Stream Parsing) -> Validate.
3. Complete User Story 2 (Header Handling) -> Validate.
4. **STOP and VALIDATE**: Ensure the container/content separation meets cross-language alignment goals.

### Incremental Delivery

1. Add User Story 3 (Serialization) -> Validate.
2. Complete documentation and backward compatibility.
3. Final polish and performance check.
