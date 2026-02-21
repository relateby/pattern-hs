# Implementation Plan: PatternGraph Data Structure

**Branch**: `033-pattern-graph` | **Date**: 2026-02-18 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/033-pattern-graph/spec.md`

**User planning input**: Include producing or updating `docs/` with usage of PatternGraph and a reference for writing a `.graph.gram` file that only uses annotation, nodes, relationships and paths (no square-bracket pattern notation). The gram notation reference should note the resulting data structures within PatternGraph and may use pattern notation to describe those structures (gram → parse → PatternGraph → explained with pattern notation).

## Summary

Implement a **PatternGraph** container type backed by `Pattern v`, storing nodes, relationships, walks, and annotations keyed by identity, with merge-on-insert semantics and recursive decomposition (walks/annotations merge their components). Support building from a list of patterns via `fromPatterns` (fold of merge), signal unrecognized patterns instead of dropping them, and convert PatternGraph to the existing GraphLens view. Documentation will include PatternGraph usage and a `.graph.gram` reference that restricts to graph-only notation and explains resulting PatternGraph structure using pattern notation.

## Technical Context

**Language/Version**: Haskell (GHC 9.12.2 per CLAUDE.md; base >=4.17.0.0 from pattern.cabal)  
**Primary Dependencies**: pattern (Pattern.Core, Pattern.Reconcile), subject, containers, hashable, unordered-containers (from libs/pattern)  
**Storage**: In-memory only (Map keyed by Id v); gram files for round-trip via libs/gram parse/serialize  
**Testing**: Hspec, QuickCheck (pattern-test in libs/pattern); property tests for merge/classification  
**Target Platform**: GHC-supported platforms (Linux, macOS, Windows)  
**Project Type**: Multi-library (libs/pattern, libs/gram, libs/subject) + CLI app (apps/gramref-cli)  
**Performance Goals**: Round-trip and merge at parse/editor scale (no hard SLA; clarity over micro-optimization)  
**Constraints**: Must integrate with existing Pattern.Reconcile policies and Pattern.Graph (GraphLens)  
**Scale/Scope**: Single graph per container; typical use is one gram file ↔ one PatternGraph

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|--------|
| **I. Code Quality** | Pass | PatternGraph and GraphValue will live in documented modules with clear naming and doc comments. |
| **II. Testing Standards** | Pass | Unit tests for merge, fromPatterns, classification, conversion to GraphLens; property tests for merge idempotence and consistency. |
| **III. Conceptual Consistency** | Pass | PatternGraph is a container over Pattern; classification and identity align with existing Pattern/Subject model. |
| **IV. Mathematical Clarity** | Pass | Merge semantics and classification (Node/Annotation/Relationship/Walk) will be stated in docs; pattern notation used in .graph.gram reference. |
| **V. Multi-Language Reference** | Pass | Data model and operations are language-agnostic; Haskell is reference implementation. |
| **Category Theory Foundations** | Pass | No new functor/morphism claims; container + merge are standard. |
| **Cross-Language Consistency** | Pass | Design from proposal is explicit and translatable. |
| **Version Control** | Pass | Checkpoint commits on feature branch. |
| **Documentation Standards** | Pass | Type and function docs; docs/ updates for usage and .graph.gram reference. |

No violations. Complexity tracking table left empty.

## Project Structure

### Documentation (this feature)

```text
specs/033-pattern-graph/
├── plan.md              # This file
├── research.md          # Phase 0
├── data-model.md        # Phase 1
├── quickstart.md        # Phase 1
├── contracts/           # Phase 1 (Pattern.PatternGraph API)
└── tasks.md             # Phase 2 (/speckit.tasks - not created by plan)
```

### Documentation (repository docs/)

Updates and additions under `docs/` (per user planning input):

- **PatternGraph usage**: New or updated guide/reference (e.g. `docs/guide/pattern-graph-usage.md` or section in existing guide, and `docs/reference/features/pattern-graph.md`) covering construction, merge, fromPatterns, conversion to GraphLens, and round-trip with gram.
- **`.graph.gram` reference**: New reference (e.g. `docs/reference/graph-gram-notation.md` or `docs/guide/graph-gram-reference.md`) that:
  - Restricts to **annotation, nodes, relationships, and paths only** — no square-bracket pattern notation in the file format.
  - Describes the **resulting data structures inside PatternGraph** and may use **pattern notation** to explain them. Flow: *gram → parse → PatternGraph → explained with pattern notation*.

### Source Code (repository root)

```text
libs/pattern/
├── src/
│   ├── Pattern/
│   │   ├── Core.hs
│   │   ├── Graph.hs           # Existing GraphLens
│   │   ├── PatternGraph.hs     # NEW: PatternGraph, GraphValue, PatternClass, merge, fromPatterns, toGraphLens
│   │   └── Reconcile.hs
│   └── ...
├── pattern.cabal              # Add Pattern.PatternGraph to exposed-modules
└── tests/
    ├── Spec/Pattern/
    │   ├── GraphSpec.hs
    │   ├── PatternGraphSpec.hs # NEW: unit/integration for PatternGraph
    │   └── PatternGraphProperties.hs # NEW: property tests
    └── Test.hs

libs/gram/
├── src/
│   └── Gram/
│       ├── Parse.hs
│       └── Serialize.hs
└── ...
```

**Structure Decision**: PatternGraph is implemented in libs/pattern as a new module `Pattern.PatternGraph`, reusing Pattern.Core and Pattern.Reconcile. GraphLens remains in Pattern.Graph; conversion from PatternGraph to GraphLens is implemented in Pattern.PatternGraph (or Pattern.Graph) as agreed in Phase 1. Documentation lives under existing docs/ layout (guide + reference).

## Complexity Tracking

*No constitution violations. Table unused.*
