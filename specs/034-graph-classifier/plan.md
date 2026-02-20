# Implementation Plan: [FEATURE]

**Branch**: `[###-feature-name]` | **Date**: [DATE] | **Spec**: [link]
**Input**: Feature specification from `/specs/[###-feature-name]/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Introduce `GraphClassifier` as the unifying abstraction for interpreting `Pattern v` as a graph. The canonical graph form (currently split across `GraphLens` and `PatternGraph`) is reconceived as a family of views over one storage substrate — `Pattern v`. `GraphLens` is re-derived from `GraphClassifier` as its two-category specialization; `PatternGraph` is updated to accept a `GraphClassifier` at construction. `GraphClassifier` becomes the shared vocabulary that unifies them.

## Technical Context

**Language/Version**: Haskell (GHC 9.10.3)
**Primary Dependencies**: `pattern-hs` ecosystem (`Pattern.Core`, `Subject.Core`)
**Storage**: In-memory `Map` via `PatternGraph`
**Testing**: `hspec`, property-based via QuickCheck/Hedgehog
**Target Platform**: N/A (Library)
**Project Type**: single library (`libs/pattern`)
**Performance Goals**: Graph construction processing time within 5% of the legacy hardcoded implementation.
**Constraints**: 100% backward compatibility for existing `GraphLens` interactions.
**Scale/Scope**: Re-architect `Pattern.Graph` and `Pattern.PatternGraph` internals.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

- [x] Code Quality: Changes simplify typeclasses, making the code clearer.
- [x] Testing Standards: Tests can easily verify the unchanged public APIs.
- [x] Conceptual Consistency: A categorical "Functor" view mapping Patterns to categorical structures applies.
- [x] Mathematical Clarity: Clear classification into distinct categories (Nodes, Relationships, etc.).
- [x] Multi-Language Reference Alignment: Passing functions as records (like `GraphClassifier`) aligns with JS/Rust implementations.

## Project Structure

### Documentation (this feature)

```text
specs/034-graph-classifier/
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── contracts/
└── tasks.md
```

### Source Code (repository root)

```text
libs/pattern/src/Pattern/
├── Graph.hs
├── Graph/
│   └── GraphClassifier.hs
└── PatternGraph.hs

libs/pattern/tests/Spec/Pattern/
├── GraphSpec.hs
├── PatternGraphSpec.hs
└── Graph/
    └── GraphClassifierSpec.hs
```

**Structure Decision**: The logic is added as a new module `Pattern.Graph.GraphClassifier` to establish a shared core vocabulary for classification. `GraphLens` (in `Pattern.Graph`) and `PatternGraph` (in `Pattern.PatternGraph`) will both import and utilize this module.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| Breaking change to `PatternGraph` | Replaces `Unrecognized` with `GOther` bucket. | Leaving `Unrecognized` drops data. |

