# Tasks: Paramorphism for Structure-Aware Folding

**Input**: Design documents from `/specs/028-paramorphism-folding/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), data-model.md (available), contracts/ (available), research.md (available), quickstart.md (available)

**Tests**: Tests are included as they are essential for verifying paramorphism operations, structure access, and correctness.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `libs/pattern/src/`, `libs/pattern/tests/` at repository root
- Paths shown below assume single library project structure

---

## Phase 1: User Story 1 - Fold Patterns with Structure Awareness (Priority: P1) 🎯 MVP

**Goal**: Implement paramorphism function `para` that enables folding over pattern structures while providing access to the full pattern subtree at each position, so that developers can perform structure-aware aggregations that consider both values and structural context.

**Independent Test**: Apply paramorphism operations to patterns and verify that: (1) the folding function receives both the current pattern subtree and recursively computed results from children, (2) structure-aware aggregations produce correct results, and (3) paramorphism works for atomic patterns, patterns with elements, and nested patterns.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T001 [P] [US1] Write unit test for paramorphism on atomic pattern verifying folding function receives pattern subtree in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T002 [P] [US1] Write unit test for paramorphism on pattern with elements verifying folding function receives pattern subtree and child results in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T003 [P] [US1] Write unit test for paramorphism on nested pattern verifying structural access at all levels in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T004 [P] [US1] Write unit test for paramorphism verifying structure-preserving transformation during fold in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T005 [P] [US1] Write unit test for paramorphism verifying context-dependent aggregation adapts to structural properties in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T006 [P] [US1] Write unit test for paramorphism on atomic pattern with integer value in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T007 [P] [US1] Write unit test for paramorphism on pattern with multiple elements containing integer values in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T008 [P] [US1] Write unit test for paramorphism on deeply nested pattern structure in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T009 [P] [US1] Write unit test for paramorphism with string values in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T010 [P] [US1] Write unit test for paramorphism with custom type values in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 1

- [x] T011 [US1] Implement `para` function with signature `para :: (Pattern v -> [r] -> r) -> Pattern v -> r` in `libs/pattern/src/Pattern/Core.hs`
- [x] T012 [US1] Add Haddock documentation for `para` function explaining structure-aware folding in `libs/pattern/src/Pattern/Core.hs`
- [x] T013 [US1] Add Haddock examples for `para` usage demonstrating structure access in `libs/pattern/src/Pattern/Core.hs`
- [x] T014 [US1] Export `para` function from `Pattern.Core` module in `libs/pattern/src/Pattern/Core.hs`

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. All tests should pass, demonstrating structure-aware folding with access to pattern subtrees.

---

## Phase 2: User Story 2 - Perform Structure-Aware Aggregations (Priority: P1)

**Goal**: Verify that paramorphism enables structure-aware aggregations (depth-weighted sums, nesting-level statistics, element-count-based aggregations) that correctly reflect both values and structural properties.

**Independent Test**: Apply paramorphism to compute structure-aware aggregations and verify that results correctly reflect both values and structural properties.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T015 [P] [US2] Write unit test for depth-weighted sum using paramorphism verifying values at different nesting levels contribute differently in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T016 [P] [US2] Write unit test for element-count-aware aggregation using paramorphism verifying aggregation reflects element count in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T017 [P] [US2] Write unit test for nesting-level statistics using paramorphism verifying level-aware statistics in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T018 [P] [US2] Write unit test for context-dependent aggregation using paramorphism verifying aggregation adapts to structural properties in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T019 [P] [US2] Write unit test for depth-weighted sum on pattern with values at different nesting levels in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [x] T020 [P] [US2] Write unit test for element-count aggregation on pattern with varying element counts in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`

### Implementation for User Story 2

- [x] T021 [US2] Verify `para` implementation enables depth-weighted aggregations (no code changes if already correct) in `libs/pattern/src/Pattern/Core.hs`
- [x] T022 [US2] Add Haddock documentation examples for depth-weighted sum using `para` in `libs/pattern/src/Pattern/Core.hs`
- [x] T023 [US2] Add Haddock documentation examples for element-count-aware aggregation using `para` in `libs/pattern/src/Pattern/Core.hs`
- [x] T024 [US2] Add Haddock documentation examples for nesting-level statistics using `para` in `libs/pattern/src/Pattern/Core.hs`

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Paramorphism should enable structure-aware aggregations that correctly reflect both values and structural properties.

---

## Phase 3: Property-Based Tests

**Goal**: Verify paramorphism properties using property-based testing to ensure correctness across many pattern structures.

**Independent Test**: Property-based tests verify paramorphism provides access to pattern structure, structure-aware aggregations produce correct results, and relationship to `Foldable`.

### Property-Based Tests

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T025 [P] Write property-based test verifying `para (\p _ -> depth p) pattern == depth pattern` in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- [x] T026 [P] Write property-based test verifying `para (\p rs -> value p : concat rs) pattern == toList pattern` in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- [x] T027 [P] Write property-based test verifying depth-weighted sum produces correct results in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- [x] T028 [P] Write property-based test verifying element-count aggregation produces correct results in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- [x] T029 [P] Write property-based test verifying paramorphism preserves element order in `libs/pattern/tests/Spec/Pattern/Properties.hs`
- [x] T030 [P] Write property-based test verifying relationship to `Foldable`: `para (\p rs -> value p + sum rs) pattern == foldr (+) 0 pattern` in `libs/pattern/tests/Spec/Pattern/Properties.hs`

**Checkpoint**: At this point, property-based tests should pass, verifying paramorphism correctness across many pattern structures.

---

## Phase 4: User Story 3 - Understand Relationship to Foldable and Comonad (Priority: P2)

**Goal**: Provide comprehensive documentation explaining how paramorphism relates to `Foldable` (value-only folding) and `Comonad` (structure-aware transformations), so that developers can choose the right operation for each use case.

**Independent Test**: Create documentation that explains: (1) how paramorphism extends `Foldable` to provide structure access, (2) how paramorphism relates to `Comonad` (both provide structure access but for different purposes), and (3) when to use each operation.

### Documentation for User Story 3

- [ ] T031 [P] [US3] Add paramorphism section to user guide explaining intuitive relationship to `Foldable` and `Comonad` in `docs/guide/06-advanced-morphisms.md`
- [ ] T032 [P] [US3] Add examples comparing paramorphism with `Foldable` and `Comonad` in user guide in `docs/guide/06-advanced-morphisms.md`
- [ ] T033 [P] [US3] Add "When to Use" guidance comparing paramorphism, `Foldable`, and `Comonad` in user guide in `docs/guide/06-advanced-morphisms.md`
- [ ] T034 [P] [US3] Create reference documentation file for paramorphism with complete API reference in `docs/reference/features/paramorphism.md`
- [ ] T035 [P] [US3] Add type signatures and function documentation to paramorphism reference in `docs/reference/features/paramorphism.md`
- [ ] T036 [P] [US3] Add laws and properties to paramorphism reference in `docs/reference/features/paramorphism.md`
- [ ] T037 [P] [US3] Add implementation details and examples to paramorphism reference in `docs/reference/features/paramorphism.md`
- [ ] T038 [P] [US3] Add paramorphism porting notes explaining how to implement in other languages in `docs/reference/PORTING-GUIDE.md`
- [ ] T039 [P] [US3] Add examples in multiple languages (Rust, TypeScript, Python) to porting guide in `docs/reference/PORTING-GUIDE.md`
- [ ] T040 [P] [US3] Add relationship to `Foldable` and `Comonad` in other languages to porting guide in `docs/reference/PORTING-GUIDE.md`

**Checkpoint**: At this point, User Story 3 should be complete. Documentation should clearly explain the relationship between paramorphism, `Foldable`, and `Comonad` with examples showing when to use each operation.

---

## Phase 5: Edge Cases and Polish

**Goal**: Ensure paramorphism handles all edge cases correctly and add final polish.

### Edge Case Tests

- [ ] T041 [P] Write unit test for paramorphism on atomic pattern (no elements) in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [ ] T042 [P] Write unit test for paramorphism on pattern with empty elements list in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [ ] T043 [P] Write unit test for paramorphism on pattern with single element (singular pattern) in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [ ] T044 [P] Write unit test for paramorphism on pattern with many elements in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [ ] T045 [P] Write unit test for paramorphism on nested patterns with varying depths in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [ ] T046 [P] Write unit test for paramorphism with different value types (strings, integers, custom types) in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [ ] T047 [P] Write unit test for paramorphism verifying element order preservation in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`

### Comparison Tests

- [ ] T048 [P] Write unit test comparing paramorphism with `Foldable` demonstrating value-only vs. structure-aware folding in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- [ ] T049 [P] Write unit test comparing paramorphism with `Comonad` demonstrating structure-aware transformation vs. structure-aware folding in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`

### Final Polish

- [ ] T050 Verify all tests pass and complete quickly (<10ms for property-based tests) in `libs/pattern/tests/Spec/Pattern/`
- [ ] T051 Verify all success criteria from spec.md are met
- [ ] T052 Review and update Haddock documentation for completeness in `libs/pattern/src/Pattern/Core.hs`
- [ ] T053 Verify documentation examples compile and run correctly

**Checkpoint**: At this point, all edge cases should be handled, all tests should pass, and documentation should be complete. Feature is ready for review.

---

## Dependencies

### User Story Completion Order

1. **User Story 1** (P1) - Must complete first: Core paramorphism implementation
2. **User Story 2** (P1) - Depends on User Story 1: Structure-aware aggregations build on core implementation
3. **Property-Based Tests** - Depends on User Stories 1 and 2: Verify correctness of implementation
4. **User Story 3** (P2) - Depends on User Stories 1 and 2: Documentation explains implemented features
5. **Edge Cases and Polish** - Depends on all previous phases: Final verification and polish

### Parallel Execution Opportunities

**Within User Story 1**:
- All test tasks (T001-T010) can run in parallel
- Implementation tasks (T011-T014) must run sequentially after tests

**Within User Story 2**:
- All test tasks (T015-T020) can run in parallel
- Implementation tasks (T021-T024) must run sequentially after tests

**Within Property-Based Tests**:
- All property-based test tasks (T025-T030) can run in parallel

**Within User Story 3**:
- All documentation tasks (T031-T040) can run in parallel

**Within Edge Cases and Polish**:
- All edge case test tasks (T041-T049) can run in parallel
- Final polish tasks (T050-T053) must run sequentially after all tests

## Implementation Strategy

### MVP Scope

**Suggested MVP**: User Story 1 only (Phase 1)
- Implements core `para` function
- Provides structure-aware folding capability
- Enables all documented use cases
- Can be tested independently

**Incremental Delivery**:
1. **MVP**: User Story 1 - Core paramorphism function
2. **Enhancement**: User Story 2 - Structure-aware aggregations
3. **Verification**: Property-based tests
4. **Documentation**: User Story 3 - Comprehensive documentation
5. **Polish**: Edge cases and final verification

### Task Summary

- **Total Tasks**: 53
- **User Story 1 Tasks**: 14 (10 tests + 4 implementation)
- **User Story 2 Tasks**: 10 (6 tests + 4 implementation)
- **Property-Based Tests**: 6
- **User Story 3 Tasks**: 10 (all documentation)
- **Edge Cases and Polish**: 13 (9 edge case tests + 4 polish)

### Independent Test Criteria

- **User Story 1**: Can apply paramorphism to patterns and verify structure access
- **User Story 2**: Can compute structure-aware aggregations using paramorphism
- **User Story 3**: Can read documentation and understand when to use each operation
- **Property-Based Tests**: Verify correctness across many pattern structures
- **Edge Cases**: Verify all edge cases handled correctly

