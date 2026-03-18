# Implementation Plan: RepresentationMap

**Branch**: `039-representation-map` | **Date**: 2026-03-17 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/039-representation-map/spec.md`

## Summary

Introduce `PatternKind` (named, scope-aware shape predicates) in `Pattern.Core` and `RepresentationMap` (named invertible isomorphisms between shape kinds) in a new `Pattern.RepresentationMap` module. Add a concrete `diagnosticMap` instance in the test suite with QuickCheck round-trip property tests. All existing APIs are unchanged.

## Technical Context

**Language/Version**: Haskell (GHC 9.12.2)
**Primary Dependencies**: base, containers — no new dependencies required
**Storage**: N/A — pure in-memory types
**Testing**: HSpec + QuickCheck (existing test suite)
**Target Platform**: Library (no platform target)
**Project Type**: Single library project
**Performance Goals**: No hot-path concerns — `PatternKind` and `RepresentationMap` are data values, not traversal primitives
**Constraints**: No new library dependencies; `RankNTypes` extension required
**Scale/Scope**: Two new types, one new function, one new module, one new test file

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-checked post-design.*

| Principle | Status | Notes |
|---|---|---|
| I. Code Quality | ✅ PASS | New types and functions will have explicit documentation with categorical interpretation |
| II. Testing Standards | ✅ PASS | `PatternKind` and `RepresentationMap` will have unit tests; round-trip is a QuickCheck property; compose failure case is unit-tested |
| III. Conceptual Consistency | ✅ PASS | `PatternKind` is a subobject classifier; `RepresentationMap` is a named isomorphism; `compose` is morphism composition — all documented with categorical interpretation |
| IV. Mathematical Clarity | ✅ PASS | `roundTrip` is the formal isomorphism witness; identity law stated in contract; conventions make the inverse explicit |
| V. Multi-Language Reference Alignment | ✅ PASS | Rust correspondence documented in proposal: typeclass→trait, `*Dict`→struct, `forall q`→`Box<dyn ScopeQuery<V>>` |
| Version Control Standards | ✅ PASS | Intermediate commits per step: after PatternKind, after RepresentationMap, after diagnosticMap |

**No violations. No complexity tracking required.**

## Project Structure

### Documentation (this feature)

```text
specs/039-representation-map/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/
│   ├── Pattern.Core.hs              # PatternKind contract
│   └── Pattern.RepresentationMap.hs # RepresentationMap + compose contract
└── tasks.md             # Phase 2 output (/speckit.tasks — NOT created here)
```

### Source Code

```text
libs/pattern/src/
├── Pattern/
│   ├── Core.hs                  # Add: PatternKind, checkKind, RankNTypes extension
│   └── RepresentationMap.hs     # New: RepresentationMap, compose
└── Pattern.hs                   # Add: re-export Pattern.RepresentationMap

libs/pattern/
└── pattern.cabal                # Add: Pattern.RepresentationMap to exposed-modules

libs/pattern/tests/
└── Spec/Pattern/
    └── RepresentationMapSpec.hs # New: unit + property tests, diagnosticMap
```

**Structure Decision**: Single library project. Source additions are additive — no existing files are restructured. `Pattern.RepresentationMap` is a new peer module alongside `Pattern.Graph.*`.

## Implementation Phases

### Phase 1: `PatternKind` in `Pattern.Core`

**Goal**: Named, scope-aware shape predicates.

**Changes**:
1. Add `{-# LANGUAGE RankNTypes #-}` to `Pattern/Core.hs`
2. Add `PatternKind v` data type with `kindName`, `kindPred`, `kindExample`
3. Add `checkKind` convenience function
4. Add `PatternKind`, `checkKind` to `Pattern.Core` exports
5. Add `PatternKind`, `checkKind` to `Pattern.hs` re-exports

**Tests** (in `RepresentationMapSpec.hs`):
- T001: `kindPred` returns true for canonical example (the invariant)
- T002: `kindPred` returns false for a pattern not of that kind
- T003: scope-relative predicate uses `allElements` correctly
- T004: structural predicate ignores scope argument

**Checkpoint commit**: `"representation-map: PatternKind in Pattern.Core"`

---

### Phase 2: `RepresentationMap` + `compose` in `Pattern.RepresentationMap`

**Goal**: Named invertible isomorphisms between shape kinds.

**Changes**:
1. Create `libs/pattern/src/Pattern/RepresentationMap.hs`
2. Implement `RepresentationMap v` data type
3. Implement `compose :: RepresentationMap v -> RepresentationMap v -> Either String (RepresentationMap v)`
4. Add `Pattern.RepresentationMap` to `exposed-modules` in `pattern.cabal`
5. Add re-export to `Pattern.hs`

**Tests**:
- T005: `compose` with compatible kinds produces map from domain-of-first to codomain-of-second
- T006: `compose` combined `name` uses " >>> " separator
- T007: `compose` combined `conventions` is union of both
- T008: `compose` returns `Left` when `kindName (codomain m1) /= kindName (domain m2)` — error message names both kinds
- T009: composed `forward` applies m1 then m2
- T010: composed `inverse` applies m2-inverse then m1-inverse

**Checkpoint commit**: `"representation-map: RepresentationMap + compose"`

---

### Phase 3: `diagnosticMap` prototype and round-trip property tests

**Goal**: Validate the abstraction with a concrete isomorphism; establish the round-trip property-test pattern.

**Shape kinds**:
- `DiagnosticPattern`: `Location` pattern directly containing a `Diagnostic` pattern, which directly contains zero or more `Remediation` patterns
- `DiagnosticGraph`: flat atomic patterns with labels `Location`, `Diagnostic`, `Remediation` connected by `AT`/`HAS_REMEDIATION` relationship patterns; each non-relationship pattern has `_arity` (Int) and `_depth` (Int) properties

**Conventions** (the structural decisions that make inversion possible):
- `"_arity encodes element count of the corresponding nested pattern node"`
- `"_depth encodes nesting depth, enabling reconstruction of order"`

**Forward transform** (DiagnosticPattern → DiagnosticGraph):
- Traverse the nested structure with `paraWithScope` and `TrivialScope`
- Emit flat atomic patterns for each Location/Diagnostic/Remediation node
- Annotate each with `_arity` (count of direct elements) and `_depth` (nesting depth)
- Emit `AT` relationship patterns connecting Location→Diagnostic
- Emit `HAS_REMEDIATION` relationship patterns connecting Diagnostic→Remediation

**Inverse transform** (DiagnosticGraph → DiagnosticPattern):
- Find all `Diagnostic` patterns via `allElements` on scope
- Find their `Location` containers via `containers`
- Find their `Remediation` children via `allElements` filtered by `HAS_REMEDIATION`
- Reconstruct nesting order from `_depth` property
- Emit nested `Pattern` structure

**Tests**:
- T011: `forward` applied to canonical `DiagnosticPattern` example satisfies `DiagnosticGraph` kind predicate
- T012: `inverse` applied to canonical `DiagnosticGraph` example satisfies `DiagnosticPattern` kind predicate
- T013: round-trip on canonical example: `(inverse . forward) canonicalDiagnostic == canonicalDiagnostic`
- T014: QuickCheck property — `forAll` generated `DiagnosticPattern`s, `roundTrip` holds
- T015: QuickCheck property — after `compose diagnosticMap identityMap`, round-trip still holds (composition preserves correctness)
- T016: `diagnosticMap` conventions list is non-empty and contains `"_arity"` and `"_depth"`

**Checkpoint commit**: `"representation-map: diagnosticMap prototype and round-trip tests"`

---

### Phase 4: Regression and full test suite

**Goal**: Confirm zero regressions.

**Actions**:
- Run full `pattern-hs` test suite
- Confirm all pre-existing tests pass without modification
- No call-site changes required anywhere

**Checkpoint commit**: `"representation-map: full test suite passing"` (only if any incidental fixes needed)

---

## Acceptance Criteria

| Criterion | Verified By |
|---|---|
| `PatternKind` + `checkKind` exported from `Pattern.Core` | Module compilation + export check |
| `RepresentationMap` + `compose` exported from `Pattern.RepresentationMap` | Module compilation + export check |
| `kindPred (kindExample k)` is always true | T001 |
| `compose` fails with descriptive error on mismatch | T008 |
| `diagnosticMap` forward produces `DiagnosticGraph`-kind output | T011 |
| `diagnosticMap` round-trip holds for all generated inputs | T014 (QuickCheck) |
| Composition preserves round-trip | T015 |
| All existing tests pass | Phase 4 full run |

## Cross-Language Correspondence (for Rust port)

| Haskell | Rust |
|---|---|
| `PatternKind v` | `struct PatternKind<V>` with `Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> bool>` |
| `RepresentationMap v` | `struct RepresentationMap<V>` with `Box<dyn Fn(...)>` fields |
| `forall q. ScopeQuery q v =>` | `&dyn ScopeQuery<V>` at trait-object boundary; `impl ScopeQuery<V>` for monomorphic paths |
| `compose` → `Either String` | `Result<RepresentationMap<V>, String>` |
| QuickCheck property test | `proptest` or `quickcheck` crate |

Document in porting guide when Rust port is undertaken.
