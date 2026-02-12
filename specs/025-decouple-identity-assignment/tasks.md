---
description: "Tasks for decoupling identity assignment from transformation in pattern-hs"
---

# Tasks: Decouple Identity Assignment from Transformation

**Feature**: Decouple Identity Assignment (`025-decouple-identity-assignment`)  
**Goal**: Make identity assignment optional, preserving anonymity by default for round-trip compatibility  
**Approach**: Option 1 - Separate functions (`transformGram`, `transformGramWithIds`, `assignIdentities`)

**Input**: Design discussion and analysis of round-trip issues  
**Prerequisites**: Understanding of current identity assignment in `Gram.Transform`

**Tests**: Tests are INCLUDED as fundamental tasks given the rigorous nature of the project.

**Organization**: Tasks are grouped by phase to enable systematic implementation and testing.

## Format: `[ID] [P?] [Phase] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Phase]**: Which phase this task belongs to
- Include exact file paths in descriptions

---

## Phase 1: Core Implementation - Transform Module

**Purpose**: Update `Gram.Transform` to support both anonymous-preserving and ID-assigning transforms

### Core Transform Changes

- [x] T001 Update `transformGram` in `libs/gram/src/Gram/Transform.hs` to preserve anonymity
  - Change `transformIdentifier Nothing = generateId` to `transformIdentifier Nothing = return (S.Symbol "")`
  - Change `transformEmptySubject` to use `S.Symbol ""` instead of `generateId`
  - Remove state initialization from `transformGram` (no longer needs counter)
  - Update `transformGram` signature comment to note it preserves anonymity

- [x] T002 Add `transformGramWithIds` function in `libs/gram/src/Gram/Transform.hs`
  - Implement as `transformGramWithIds = assignIdentities . transformGram`
  - Add Haddock documentation explaining it assigns IDs to anonymous subjects
  - Export from module

- [x] T003 Add `assignIdentities` function in `libs/gram/src/Gram/Transform.hs`
  - Type: `assignIdentities :: P.Pattern S.Subject -> P.Pattern S.Subject`
  - Recursively traverse pattern, assigning `#N` IDs to subjects with `Symbol ""`
  - Use `State Int` monad internally with `evalState`
  - Start counter from `findMaxIdInPattern pattern + 1` to avoid collisions
  - Preserve all non-empty identities unchanged
  - Add Haddock documentation

- [x] T004 Add helper `findMaxIdInPattern` in `libs/gram/src/Gram/Transform.hs`
  - Extract maximum numeric suffix from `#N`-style IDs in a Pattern
  - Similar logic to existing `findMaxId` but operates on Pattern instead of CST
  - Used by `assignIdentities` to determine starting counter value

- [x] T005 Update `transformPattern` annotation wrapper ID generation in `libs/gram/src/Gram/Transform.hs`
  - Change `sym <- generateId` to `return (S.Symbol "")` for annotation wrappers
  - Annotation wrappers should also preserve anonymity by default
  - Note: This affects line 95 in current implementation

- [x] T006 Update module exports in `libs/gram/src/Gram/Transform.hs`
  - Export `transformGram` (default, preserves anonymity)
  - Export `transformGramWithIds` (explicit ID assignment)
  - Export `assignIdentities` (post-transform ID assignment)
  - Update module documentation

**Checkpoint**: Core transform functions implemented. Ready for Parse module updates.

---

## Phase 2: Parse Module Updates

**Purpose**: Update `fromGram` to use anonymous-preserving transform by default

- [x] T007 Update `fromGram` in `libs/gram/src/Gram/Parse.hs`
  - Ensure it uses `Transform.transformGram` (which now preserves anonymity)
  - Update function documentation to note it preserves anonymous subjects as `Symbol ""`
  - No signature changes needed

- [x] T008 [P] Add `fromGramWithIds` function in `libs/gram/src/Gram/Parse.hs`
  - Type: `fromGramWithIds :: String -> Either ParseError (Pattern Subject)`
  - Uses `Transform.transformGramWithIds` internally
  - Add Haddock documentation explaining when to use this vs `fromGram`
  - Export from `Gram.Parse` module

**Checkpoint**: Parse module supports both anonymous-preserving and ID-assigning modes.

---

## Phase 3: Test Updates - ParseSpec

**Purpose**: Update existing tests that expect ID assignment to use new functions

### Update Anonymous Subject Tests

- [x] T009 Update test "assigns unique IDs to anonymous nodes" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Change from `fromGram "() ()"` to `fromGramWithIds "() ()"`
  - Keep all assertions about ID format and distinctness
  - Add comment explaining this test uses explicit ID assignment

- [x] T010 Update test "assigns unique IDs to anonymous path elements" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Change from `fromGram "()-[]->()"` to `fromGramWithIds "()-[]->()"`
  - Keep all assertions about ID format and distinctness

- [x] T011 Update test "avoids collision with existing generated-style IDs" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Change from `fromGram "(`#1`) ()"` to `fromGramWithIds "(`#1`) ()"`
  - Keep all assertions about collision avoidance

- [x] T012 Update test "re-round-trips generated IDs safely" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Change from `fromGram "(`#1`) ()"` to `fromGramWithIds "(`#1`) ()"`
  - Keep all assertions

### Add New Anonymous Preservation Tests

- [x] T013 Add test "preserves anonymous nodes as empty Symbol" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Test: `fromGram "() ()"`
  - Assert both nodes have `Symbol ""` as identity
  - Verify they are distinct patterns (structural equality, not identity-based)

- [x] T014 Add test "preserves anonymous path elements as empty Symbol" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Test: `fromGram "()-[]->()"`
  - Assert relationship, left node, and right node all have `Symbol ""`
  - Verify structure is preserved correctly

- [x] T015 Add test "preserves anonymous in nested patterns" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Test: `fromGram "[ () | () ]"`
  - Assert outer pattern and both inner patterns have `Symbol ""`
  - Verify nesting structure is correct

- [x] T016 Add test "preserves anonymous alongside named subjects" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Test: `fromGram "(a) () (b)"`
  - Assert named subjects keep their IDs, anonymous has `Symbol ""`
  - Verify all three are distinct

- [x] T017 Add test "assignIdentities assigns IDs to anonymous only" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Create pattern with mix of named and anonymous subjects
  - Apply `assignIdentities` from `Gram.Transform`
  - Assert anonymous get `#N` IDs, named remain unchanged

- [x] T018 Add test "assignIdentities avoids collisions with existing IDs" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Create pattern with `#1` and anonymous subjects
  - Apply `assignIdentities`
  - Assert anonymous get `#2`, `#3`, etc. (starting after max existing)

**Checkpoint**: ParseSpec tests updated and new anonymous preservation tests added.

---

## Phase 4: Test Updates - SerializeSpec Round-Trip

**Purpose**: Add comprehensive round-trip tests for anonymous preservation

### Round-Trip Tests for Anonymous Subjects

- [x] T019 Add test "round-trip preserves anonymous nodes" in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
  - Parse: `fromGram "()"`
  - Serialize: `toGram parsed`
  - Assert: `serialized == "()"`
  - Re-parse and verify structural equality

- [x] T020 Add test "round-trip preserves anonymous relationships" in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
  - Parse: `fromGram "()-[]->()"`
  - Serialize: `toGram parsed`
  - Re-parse and verify structural equality (not identity-based)

- [x] T021 Add test "round-trip preserves mixed named and anonymous" in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
  - Parse: `fromGram "(a) () (b)"`
  - Serialize and re-parse
  - Verify named subjects keep IDs, anonymous remain anonymous

- [x] T022 Add test "round-trip preserves anonymous in nested structures" in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
  - Parse: `fromGram "[ () | () ]"`
  - Serialize and re-parse
  - Verify nested anonymous subjects preserved

- [x] T023 Update existing round-trip property test in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
  - Review `prop "serializes and parses back to an equivalent pattern"`
  - Ensure it works with anonymous subjects (structural equality, not identity)
  - May need to adjust generator to include anonymous subjects

- [x] T024 Add test "round-trip with assignIdentities preserves structure" in `libs/gram/tests/Spec/Gram/SerializeSpec.hs`
  - Parse: `fromGram "() ()"`
  - Apply: `assignIdentities parsed`
  - Serialize: `toGram assigned`
  - Verify: IDs appear in serialized output (e.g., `(#1) (#2)`)
  - Re-parse and verify IDs are preserved

**Checkpoint**: Round-trip tests verify anonymous preservation works correctly.

---

## Phase 5: Test Updates - CorpusSpec

**Purpose**: Ensure corpus tests continue to work with anonymous preservation

- [x] T025 Review `libs/gram/tests/Spec/Gram/CorpusSpec.hs` round-trip tests
  - Verify corpus tests use structural equality, not identity-based equality
  - Update if needed to handle anonymous subjects correctly
  - Ensure no tests break due to anonymous preservation

- [x] T026 Add test "corpus round-trip with anonymous subjects" in `libs/gram/tests/Spec/Gram/CorpusSpec.hs`
  - Find corpus examples with anonymous subjects
  - Verify they round-trip correctly with new behavior
  - Document any expected differences

**Checkpoint**: Corpus tests verified and updated if needed.

---

## Phase 6: Integration and Validation

**Purpose**: Verify no regressions and validate behavior

### Validation Module Check

- [x] T027 Verify `Gram.Validate` module behavior
  - Review validation logic - should operate on CST (before transform)
  - Confirm validation is unaffected by identity assignment changes
  - Run validation tests to ensure no regressions

### Integration Tests

- [x] T028 Add integration test "end-to-end anonymous preservation workflow" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Parse anonymous pattern
  - Serialize to gram
  - Re-parse
  - Verify structural equality
  - Apply `assignIdentities` if needed
  - Verify IDs assigned correctly

- [x] T029 Add integration test "mixed workflow with explicit ID assignment" in `libs/gram/tests/Spec/Gram/ParseSpec.hs`
  - Use `fromGramWithIds` for patterns needing IDs
  - Use `fromGram` for round-trip preservation
  - Verify both workflows work independently

### Regression Testing

- [x] T030 Run full test suite: `cabal test all`
  - Verify all existing tests pass (except those intentionally updated)
  - Document any unexpected failures
  - Fix any regressions

- [x] T031 Verify no test output changes unexpectedly
  - Compare test output before/after changes
  - Ensure only expected test updates occurred
  - Document any intentional test behavior changes

**Checkpoint**: All tests pass, no unexpected regressions.

---

## Phase 7: Documentation and Cleanup

**Purpose**: Update documentation and finalize implementation

### Code Documentation

- [x] T032 Update Haddock documentation in `libs/gram/src/Gram/Transform.hs`
  - Document `transformGram` preserves anonymity
  - Document `transformGramWithIds` assigns IDs
  - Document `assignIdentities` post-transform usage
  - Add examples showing when to use each function

- [x] T033 Update Haddock documentation in `libs/gram/src/Gram/Parse.hs`
  - Document `fromGram` preserves anonymity
  - Document `fromGramWithIds` assigns IDs
  - Add examples showing round-trip behavior

- [x] T034 Update module-level documentation
  - Explain design decision: anonymity preservation by default
  - Explain when to use ID assignment
  - Link to related functions

### Code Quality

- [x] T035 Review code for consistency
  - Ensure all anonymous subjects use `Symbol ""` consistently
  - Verify no leftover `generateId` calls for anonymous subjects
  - Check for any hardcoded assumptions about ID format

- [x] T036 Run Haddock generation: `cabal haddock lib:gram`
  - Verify documentation builds correctly
  - Check for any documentation warnings

**Checkpoint**: Documentation complete, code quality verified.

---

## Phase 8: Final Verification

**Purpose**: Comprehensive final checks

- [x] T037 Run full test suite with verbose output: `cabal test all --test-show-details=always`
  - Verify all tests pass
  - Review any warnings

- [x] T038 Manual verification of key scenarios
  - Test round-trip: `() -> parse -> serialize -> parse -> ()`
  - Test ID assignment: `() -> parse -> assignIds -> serialize -> (#1)`
  - Test mixed: `(a) () -> parse -> serialize -> (a) ()`

- [x] T039 Verify build succeeds: `cabal build lib:gram`
  - Check for any compilation warnings
  - Verify all modules compile correctly

- [x] T040 Create summary of changes
  - Document what changed
  - Document what stayed the same
  - Document migration path (if any external code needs updates)

**Checkpoint**: Implementation complete, all checks passed.

---

## Notes

### Key Design Decisions

1. **Default Behavior**: `fromGram` preserves anonymity (`Symbol ""`) for round-trip compatibility
2. **Explicit Opt-in**: `fromGramWithIds` assigns IDs when needed
3. **Post-Transform**: `assignIdentities` allows ID assignment after parsing
4. **Backward Compatibility**: Not required, but tests updated to maintain functionality

### Test Strategy

- Update existing tests that expect IDs to use `fromGramWithIds` or `assignIdentities`
- Add comprehensive tests for anonymous preservation
- Add round-trip tests to verify structural equality
- Ensure no tests regress unexpectedly

### Migration Notes

- External code using `fromGram` will now get anonymous subjects as `Symbol ""`
- Code needing IDs should use `fromGramWithIds` or `assignIdentities`
- Validation is unaffected (operates on CST before transform)
