# Implementation Plan: Graph Transform

**Branch**: `036-graph-transform` | **Date**: 2026-02-21 | **Spec**: [specs/036-graph-transform/spec.md](spec.md)
**Input**: Feature specification from `/specs/036-graph-transform/spec.md`

## Summary

Introduce `GraphView` as the universal graph-like interface and pipeline entry point, along with a set of graph transformation operations (`mapGraph`, `filterGraph`, `foldGraph`, `mapWithContext`, `paraGraph`, and `unfoldGraph`) to enable composable bulk transformations, context-aware data enrichment, and topology-aware iterative algorithms over `PatternGraph`s.

## Technical Context

**Language/Version**: Haskell (GHC 9.12.2)
**Primary Dependencies**: `pattern-hs` core (GraphClassifier, GraphQuery, PatternGraph)
**Storage**: In-memory `PatternGraph` and `GraphView` representations
**Testing**: `hspec`, `QuickCheck` (property-based testing required by Constitution)
**Target Platform**: Cross-platform (GHC)
**Project Type**: Library / Data Structure
**Performance Goals**: Efficient lazy composition of graph transformations, zero intermediate copies between chained transformations, GHC inlining for closure overhead elimination.
**Constraints**: Deterministic execution (snapshot semantics for context queries), robustness against floating point convergence equality issues in fixpoint folding.
**Scale/Scope**: Bulk operations over medium-to-large in-memory knowledge graphs.

## Constitution Check

*GATE: Passed. (Re-evaluated after Phase 1 design)*

- **Code Quality**: Functions and types will have explicit documentation and clear structure reflecting mathematical/category-theory properties.
- **Testing Standards**: All transformation functions (`mapGraph`, `paraGraph`, etc.) require unit and property-based tests.
- **Conceptual Consistency**: Category theory foundations like anamorphisms (`unfold`) and their duals (`para`) are explicitly preserved and utilized.
- **Mathematical Clarity**: Formal definitions of convergence, snapshot semantics, and structural folding are documented.
- **Multi-Language Reference Alignment**: Types and concepts (`GraphView`, message-passing paradigms) are defined agnostically where possible to aid porting.

## Project Structure

### Documentation (this feature)

```text
specs/036-graph-transform/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (generated via tasks command)
```

### Source Code (repository root)

```text
src/
├── Pattern/
│   ├── Core.hs                    # unfold implementation
│   ├── Graph.hs                   # GraphView and materialize
│   ├── Graph/
│   │   ├── Transform.hs           # mapGraph, filterGraph, foldGraph, mapWithContext, paraGraph, unfoldGraph
│   │   └── Types.hs               # Substitution
tests/
└── Pattern/
    ├── CoreSpec.hs
    ├── GraphSpec.hs
    └── Graph/
        └── TransformSpec.hs
```

**Structure Decision**: 
The implementation will be integrated into the existing `pattern-hs` library structure. `unfold` will be placed in `Pattern.Core` alongside its dual `para`. `GraphView` and `materialize` will be added to `Pattern.Graph` as they are foundational abstractions unifying querying and transformation. Transformations and graph construction functions will reside in a new module, `Pattern.Graph.Transform`.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | N/A | N/A |
