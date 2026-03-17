# Implementation Plan: Scope Unification for Structure-Aware Operations

**Branch**: `038-scope-unification` | **Date**: 2026-03-17 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/038-scope-unification/spec.md`

## Summary

Introduce a shared scope abstraction for structure-aware folding in `libs/pattern`, centered on `ScopeQuery`, `TrivialScope`, `ScopeDict`, and `paraWithScope` in `Pattern.Core`. Preserve the existing public behavior of `para` and `paraGraph` by keeping them as wrappers, while adapting graph-wide scope through an internal `GraphView`-backed adapter rather than widening `GraphQuery` itself. The implementation remains additive, keeps `topoShapeSort` and current graph ordering semantics intact, and adds documentation plus tests for the new public abstractions.

## Technical Context

**Language/Version**: Haskell (GHC 9.12.2)  
**Primary Dependencies**: `base`, `containers`, existing `pattern` modules (`Pattern.Core`, `Pattern.Graph`, `Pattern.Graph.GraphQuery`, `Pattern.Graph.Transform`)  
**Storage**: N/A - pure in-memory library behavior  
**Testing**: `hspec` and `QuickCheck` via `libs/pattern/tests`; compatibility suites in `Spec.Pattern.CoreSpec` and `Spec.Pattern.Graph.TransformSpec`  
**Target Platform**: Cross-platform library/reference implementation  
**Project Type**: Multi-package Haskell workspace; feature scope is concentrated in `libs/pattern`  
**Performance Goals**: Preserve current asymptotic behavior for `para`; keep `paraGraph` as a single pass over `topoShapeSort` output; subtree lookups/enumeration may remain linear in subtree size  
**Constraints**: Additive-only public change; no existing call-site changes; preserve `paraGraph` best-effort cycle behavior; avoid breaking the public `GraphQuery(..)` record by using an adapter for full-scope enumeration/lookup  
**Scale/Scope**: 4-6 source modules, 2-4 test modules, and 3 reference docs in `docs/reference`

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Design Gate

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Code Quality | ✅ Pass | New public abstractions will be documented explicitly and kept additive rather than replacing existing types. |
| II. Testing Standards | ✅ Pass | Plan includes compatibility tests for `para`/`paraGraph`, unit tests for new scope APIs, and property tests for equivalence and scope boundaries. |
| III. Conceptual Consistency | ✅ Pass | The design makes scope boundary an explicit concept shared by tree and graph folds. |
| IV. Mathematical Clarity | ✅ Pass | Plan includes formal contracts for the scope interface, fold invariants, and scope-boundary semantics. |
| V. Multi-Language Reference Alignment | ✅ Pass | Typeclass + first-class dictionary/value-form correspondence will be documented in the porting guide. |

### Post-Design Gate

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Code Quality | ✅ Pass | `research.md`, `data-model.md`, contracts, and quickstart keep the new API surface small and clearly separated by module responsibility. |
| II. Testing Standards | ✅ Pass | Design artifacts define unit, integration, and property-level coverage for new and preserved behavior. |
| III. Conceptual Consistency | ✅ Pass | The graph adapter decision preserves the unified scope model without changing the meaning of `GraphQuery`. |
| IV. Mathematical Clarity | ✅ Pass | Contracts and quickstart describe bottom-up folding, fixed-scope semantics, and observational equivalence of dictionary forms. |
| V. Multi-Language Reference Alignment | ✅ Pass | The design explicitly documents how the Haskell typeclass maps to trait/protocol/interface-style implementations in other languages. |

*No gate failures. No Complexity Tracking entry required.*

## Project Structure

### Documentation (this feature)

```text
specs/038-scope-unification/
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── contracts/
│   ├── Pattern.Core.hs
│   └── Pattern.Graph.Transform.hs
└── tasks.md
```

### Source Code (repository root)

```text
libs/pattern/src/
├── Pattern/Core.hs                  # New scope abstractions + paraWithScope + para wrapper update
├── Pattern/Graph.hs                 # Touch only if graph-side public docs or exports need adjustment
├── Pattern/Graph/Transform.hs       # Internal graph adapter + paraGraph/paraGraphFixed wrapper updates
└── Pattern.hs                       # Top-level re-exports for new public generic scope APIs if needed

libs/pattern/tests/
├── Spec/Pattern/CoreSpec.hs         # New ScopeQuery / TrivialScope / ScopeDict / paraWithScope tests
├── Spec/Pattern/Graph/TransformSpec.hs
└── Test.hs

docs/reference/
├── PORTING-GUIDE.md                 # Cross-language mapping for scope abstractions
└── features/
   ├── paramorphism.md               # Unified fold model for para / paraWithScope
   └── para-graph.md                 # Graph-side scope-aware fold explanation
```

**Structure Decision**: Keep the implementation inside the existing `libs/pattern` library. Put the generic scope contract in `Pattern.Core`, keep graph-specific scheduling in `Pattern.Graph.Transform`, and only add re-exports where needed to preserve discoverability without moving existing responsibilities across packages.

## Phase 0: Research

See [research.md](./research.md).

Key Phase 0 outcomes to carry into implementation:
- Use a typeclass plus first-class dictionary/value form for generic scope behavior.
- Preserve `GraphQuery(..)` unchanged and bridge graph scope through an internal `GraphView`-backed adapter.
- Treat `paraWithScope` as the canonical scope-aware tree fold, while `paraGraph` remains a graph-specific wrapper preserving `Map` output and `topoShapeSort` scheduling.

## Phase 1: Design

See [data-model.md](./data-model.md), [quickstart.md](./quickstart.md), and the contracts in [contracts/Pattern.Core.hs](./contracts/Pattern.Core.hs) and [contracts/Pattern.Graph.Transform.hs](./contracts/Pattern.Graph.Transform.hs).

### Design Notes

1. `Pattern.Core` owns the generic contract:
   - `ScopeQuery`
   - `TrivialScope`
   - `ScopeDict`
   - `toScopeDict`
   - `paraWithScope`

2. Graph-wide scope is adapted, not redefined:
   - An internal `GraphView`-backed adapter supplies generic scope answers for all classified elements in the view.
   - Existing `GraphQuery` remains the public graph-topology interface passed to current `paraGraph` callers.

3. Testing is split by responsibility:
   - Compatibility: existing `para` and `paraGraph` suites pass unchanged.
   - New API verification: unit tests for scope operations, explicit subtree empty-result behavior, and dictionary equivalence.
   - Behavioral guarantees: QuickCheck property tests for `paraWithScope (trivialScope p)` matching `para`, plus scope reification and boundary laws where applicable.

4. Documentation is part of the implementation scope:
   - `paramorphism.md` explains `paraWithScope` and the relationship to `para`
   - `para-graph.md` explains graph scheduling plus shared scope semantics
   - `PORTING-GUIDE.md` explains typeclass/dictionary correspondence for ports
   - Source-level docs include categorical meaning, invariants, and usage examples for every new public type and function

5. Validation includes non-functional guardrails:
   - review the final implementation to confirm `para` preserves its existing asymptotic behavior
   - review the graph wrapper to confirm `paraGraph` remains a single pass over `topoShapeSort`
   - audit final changes against the spec boundary so broader cleanup work is not pulled into this feature

## Complexity Tracking

No constitution violations or exceptional complexity are planned at this stage.
