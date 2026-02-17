# Tasks: Gram annotation-based identifiers and labels

**Input**: Design documents from `/specs/032-gram-annotation-syntax/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Included per Constitution (Testing Standards NON-NEGOTIABLE): corpus tests and unit tests for each annotation form and edge case.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g. US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Library**: `libs/gram/src/Gram/`, `libs/gram/tests/Spec/Gram/`
- **Corpus**: `libs/gram/test-data/tree-sitter-gram/test/corpus/extended_annotations.txt`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Ensure corpus and test runner are available

- [x] T001 Verify tree-sitter-gram corpus and gram test suite run: ensure libs/gram/test-data/tree-sitter-gram exists and `cabal test gram-test` runs from repo root

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: CST and parser support for both annotation kinds and annotation stack; Transform updated. No user story work can begin until this phase is complete.

- [x] T002 [P] Extend Annotation to sum type (PropertyAnnotation | IdentifiedAnnotation) in libs/gram/src/Gram/CST.hs; keep AnnotatedPattern.apAnnotations as [Annotation]
- [x] T003 Add parseIdentifiedAnnotation (try string "@@", then identifier and/or labels; reject empty) and parsePropertyAnnotation (@ key ( value )) in libs/gram/src/Gram/Parse.hs
- [x] T004 Update parseAnnotations and parseAnnotatedPattern to use optional one identified then many property, and produce [Annotation], in libs/gram/src/Gram/Parse.hs
- [x] T005 Update annotationsToProperties and any Annotation consumers in libs/gram/src/Gram/Transform.hs to handle Annotation sum type (property vs identified)

**Checkpoint**: Foundation ready — parser produces both annotation kinds; empty `@@` rejected; user story verification can begin

---

## Phase 3: User Story 1 - Property-style annotations (Priority: P1) — MVP

**Goal**: Property-style `@key(value)` parses and is represented as PropertyAnnotation with correct key/value and body.

**Independent Test**: Parse `@x(1) ()`, `@desc(a) (a)`, `@desc("historic route") (a)-->(b)` and verify annotation key/value and body in CST.

### Tests for User Story 1

- [x] T006 [P] [US1] Add unit tests for property-style annotations (@x(1) (), @desc(a) (a), @desc("historic route") (a)-->(b)) in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T007 [US1] Verify property-style corpus cases from libs/gram/test-data/tree-sitter-gram/test/corpus/extended_annotations.txt pass in corpus runner

**Checkpoint**: User Story 1 is independently testable and passes

---

## Phase 4: User Story 2 - Identifier-only annotations (Priority: P2)

**Goal**: `@@identifier` before a node or relationship pattern parses as IdentifiedAnnotation with identifier and body.

**Independent Test**: Parse `@@p (a)` and `@@r1 (a)-[r]->(b)` and verify IdentifiedAnnotation identifier and body.

### Tests for User Story 2

- [x] T008 [P] [US2] Add unit tests for identifier-only annotations (@@p (a), @@r1 (a)-[r]->(b)) in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T009 [US2] Verify identifier-only corpus cases from extended_annotations.txt pass in libs/gram/tests

**Checkpoint**: User Stories 1 and 2 are independently testable and pass

---

## Phase 5: User Story 3 - Labels-only annotations (Priority: P3)

**Goal**: `@@:Label` and `@@::Label` before a pattern parse as IdentifiedAnnotation with labels and body.

**Independent Test**: Parse `@@:L (a)` and `@@::Label (a)` and verify labels and body in CST.

### Tests for User Story 3

- [x] T010 [P] [US3] Add unit tests for labels-only annotations (@@:L (a), @@::Label (a)) in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T011 [US3] Verify labels-only corpus cases from extended_annotations.txt pass in libs/gram/tests

**Checkpoint**: User Stories 1–3 are independently testable and pass

---

## Phase 6: User Story 4 - Identifier and labels combined (Priority: P4)

**Goal**: `@@identifier:Label(s)` parses as IdentifiedAnnotation with both identifier and labels and body.

**Independent Test**: Parse `@@p:L (a)` and verify both identifier and labels in CST.

### Tests for User Story 4

- [x] T012 [P] [US4] Add unit tests for combined identifier+labels annotation (@@p:L (a)) in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T013 [US4] Verify combined-form corpus case from extended_annotations.txt passes in libs/gram/tests

**Checkpoint**: All four user stories are independently testable and pass

---

## Phase 7: Edge cases

**Purpose**: Empty `@@` and corpus :error case; ensure parse failure and corpus alignment.

- [x] T014 [P] Add unit test for empty @@ (a) parse error in libs/gram/tests/Spec/Gram/ParseSpec.hs
- [x] T015 Verify extended_annotations.txt :error case (empty @@ (a)) produces parse failure in libs/gram/tests corpus run

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Corpus integration, documentation, and quickstart validation.

- [x] T016 [P] Ensure extended_annotations.txt is included in corpus test run in libs/gram/tests (e.g. CorpusSpec.hs or equivalent)
- [x] T017 [P] Update module documentation for new annotation forms (property vs identified) in libs/gram/src/Gram/Parse.hs and libs/gram/src/Gram/CST.hs
- [x] T018 Run quickstart validation: cabal test gram-test, manual checks per specs/032-gram-annotation-syntax/quickstart.md

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — can start immediately
- **Foundational (Phase 2)**: Depends on Setup — BLOCKS all user stories
- **User Stories (Phases 3–6)**: Depend on Foundational completion; US2–US4 can proceed in parallel after US1 or in sequence
- **Edge cases (Phase 7)**: Depends on Foundational; can run in parallel with US1–US4 test tasks
- **Polish (Phase 8)**: Depends on all user story and edge-case phases

### User Story Dependencies

- **User Story 1 (P1)**: Starts after Foundational — no dependency on other stories
- **User Story 2 (P2)**: After Foundational — independently testable
- **User Story 3 (P3)**: After Foundational — independently testable
- **User Story 4 (P4)**: After Foundational — independently testable

### Within Each User Story

- Unit tests (T006, T008, T010, T012) can be written and run once Foundational is done
- Corpus verification tasks (T007, T009, T011, T013) depend on corpus runner including extended_annotations.txt (T016 can be done earlier to unblock)

### Parallel Opportunities

- T002 (CST) is parallelizable with no other Foundational task
- After Foundational: T006, T008, T010, T012 (unit tests for US1–US4) can run in parallel
- T014 (edge unit test), T016 (corpus inclusion), T017 (docs) can run in parallel with each other and with story test tasks

---

## Parallel Example: User Story 1

```bash
# After Foundational (T002–T005) complete:
# Run US1 unit tests
Task T006: "Add unit tests for property-style annotations in libs/gram/tests/Spec/Gram/ParseSpec.hs"
# Then verify corpus
Task T007: "Verify property-style corpus cases from extended_annotations.txt pass"
```

---

## Parallel Example: User Stories 2–4 (after Foundational)

```bash
# Unit tests for US2, US3, US4 can be implemented in parallel (different test cases, same file):
Task T008: "Add unit tests for identifier-only annotations in libs/gram/tests/Spec/Gram/ParseSpec.hs"
Task T010: "Add unit tests for labels-only annotations in libs/gram/tests/Spec/Gram/ParseSpec.hs"
Task T012: "Add unit tests for combined identifier+labels annotation in libs/gram/tests/Spec/Gram/ParseSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (T002–T005)
3. Complete Phase 3: User Story 1 (T006–T007)
4. **STOP and VALIDATE**: Run `cabal test gram-test`, verify property-style and corpus
5. Demo: parse `@x(1) ()`, `@desc("historic route") (a)-->(b)` and show CST

### Incremental Delivery

1. Setup + Foundational → parser supports both annotation kinds and stack
2. Add US1 tests and corpus verification → property-style MVP
3. Add US2 → identifier-only independently testable
4. Add US3 → labels-only independently testable
5. Add US4 → combined form independently testable
6. Edge cases + Polish → full corpus and docs

### Parallel Team Strategy

- One developer: Phase 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 in order
- Multiple developers after Foundational: Dev A (US1), Dev B (US2), Dev C (US3/US4); then edge cases and polish together

---

## Notes

- [P] tasks use different files or independent test cases; same file (e.g. ParseSpec.hs) can receive multiple tasks sequentially or in one edit
- [Story] label maps task to spec user story for traceability
- Each user story is independently completable and testable per spec
- Commit after each task or logical group
- Reference: specs/032-gram-annotation-syntax/contracts/annotations.md and extended_annotations.txt for expected behavior
