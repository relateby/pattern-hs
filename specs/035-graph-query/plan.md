# Implementation Plan: GraphQuery — Portable, Composable Graph Query Interface

**Branch**: `035-graph-query` | **Date**: 2026-02-20 | **Spec**: [spec.md](./spec.md)  
**Input**: Feature specification from `/specs/035-graph-query/spec.md`

## Summary

Introduce `GraphQuery v` as a record-of-functions that abstracts graph traversal and lookup over any graph representation. Move existing `GraphLens`-bound algorithms (`bfs`, `findPath`, `connectedComponents`) to a new `Pattern.Graph.Algorithms` module expressed against `GraphQuery`. Add `fromGraphLens` and `fromPatternGraph` constructors, `TraversalWeight` for call-site traversal policy, composability combinators (`frameQuery`, `memoizeIncidentRels`), context query helpers (`queryAnnotationsOf`, `queryWalksContaining`, `queryCoMembers`), and backward-compatible wrappers on `GraphLens`. Retire `PatternGraph.toGraphLens` once superseded.

## Technical Context

**Language/Version**: Haskell (GHC 9.10.3)  
**Primary Dependencies**: `containers ^>=0.7` (Map, Set), `base >=4.17.0.0`, `subject` (GraphValue/Symbol), `pattern` (Pattern.Core, Pattern.Graph, Pattern.PatternGraph, Pattern.Graph.GraphClassifier)  
**Storage**: N/A — pure in-memory data structures  
**Testing**: hspec ^>=2.11, QuickCheck ^>=2.14 (property-based tests)  
**Target Platform**: Library (GHC, cross-platform)  
**Project Type**: Single library (`libs/pattern`)  
**Performance Goals**: Correct behavior is the primary goal. `queryIncidentRels`, `querySource`, `queryTarget`, and `queryDegree` are hot-path functions; `{-# INLINE #-}` pragmas should be applied. Bulk adjacency for iterative algorithms is a near-term follow-on, not part of this feature.  
**Constraints**: Backward compatibility with existing `GraphLens` algorithm callers MUST be preserved. No new external dependencies.  
**Scale/Scope**: In-memory graphs of the scale handled by `PatternGraph` and `GraphLens` today. Large-graph performance (Louvain, PageRank) is explicitly deferred.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Code Quality | PASS | All new types and functions must have Haddock documentation with categorical interpretation. `GraphQuery` fields must document purpose, inputs, outputs, and invariants. |
| II. Testing Standards | PASS | Every public function requires hspec unit tests (happy path + edge cases). `TraversalWeight` canonical values and `frameQuery` require property-based QuickCheck tests. Category-theoretic properties (e.g., `fromGraphLens` and `fromPatternGraph` produce equivalent results on equivalent graphs) must be tested explicitly. |
| III. Conceptual Consistency | PASS | `GraphQuery` is a record-of-functions, consistent with `GraphClassifier` design. `TraversalWeight` externalizes traversal policy, consistent with the categorical principle that structure and interpretation are separate. Documentation must state the categorical interpretation of each component. |
| IV. Mathematical Clarity | PASS | `TraversalWeight` must be formally defined as a function `Pattern v → TraversalDirection → Double`. The semantics of infinity (blocking) must be stated. `queryContainers` must be documented as the upward traversal dual to downward decomposition. |
| V. Multi-Language Reference Alignment | PASS | `GraphQuery` uses only standard record-of-functions patterns. `TraversalWeight` is a plain function type. Both translate directly to closures/structs in other languages. Language-specific pragmas (`{-# INLINE #-}`) must be documented as Haskell-specific optimizations. |

**Verdict**: No violations. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/035-graph-query/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (Haskell type signatures)
└── tasks.md             # Phase 2 output (/speckit.tasks — NOT created here)
```

### Source Code (repository root)

```text
libs/pattern/
├── src/
│   └── Pattern/
│       ├── Graph.hs                          # existing — add fromGraphLens; retain bfs/findPath/connectedComponents as wrappers
│       ├── Graph/
│       │   ├── GraphClassifier.hs            # existing — unchanged
│       │   ├── GraphQuery.hs                 # NEW — GraphQuery v, TraversalWeight, TraversalDirection, fromGraphLens, fromPatternGraph
│       │   └── Algorithms.hs                 # NEW — all graph algorithms over GraphQuery v
│       └── PatternGraph.hs                   # existing — add fromPatternGraph export; deprecate toGraphLens
└── tests/
    └── Spec/Pattern/
        ├── GraphSpec.hs                      # existing — extend with wrapper backward-compat tests
        ├── Graph/
        │   ├── GraphClassifierSpec.hs        # existing — unchanged
        │   ├── GraphQuerySpec.hs             # NEW — unit tests for GraphQuery construction and field access
        │   └── AlgorithmsSpec.hs             # NEW — unit + property tests for all algorithms
        └── PatternGraphSpec.hs               # existing — extend with fromPatternGraph tests
```

**Structure Decision**: Single library project (`libs/pattern`). New modules `Pattern.Graph.GraphQuery` and `Pattern.Graph.Algorithms` are added under the existing `Pattern.Graph` namespace, consistent with `Pattern.Graph.GraphClassifier`. `pattern.cabal` must be updated to expose both new modules and add new test modules.

## Complexity Tracking

> No constitution violations requiring justification.
