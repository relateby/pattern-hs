# Implementation Plan: topoShapeSort — Graph Element Ordering Rename and Behavioral Correction

**Branch**: `037-topo-shape-sort` | **Date**: 2026-02-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/037-topo-shape-sort/spec.md`

## Summary

Rename `sortByArity` to `topoShapeSort` and implement correct within-bucket topological ordering for `GAnnotation` and `GOther` elements. The current bucket-sort correctly orders shape classes (nodes → relationships → walks → annotations → other) but provides no ordering guarantee within the annotation and `GOther` buckets, which can hold references to elements of the same class (annotation-of-annotation, `GOther`-of-`GOther`). The fix applies Kahn's algorithm within those two buckets, using `identify (value e)` to resolve dependencies, with soft-failure (arbitrary ordering) for cycles. Also adds `GraphValue v` constraint to `topoShapeSort`, updates `paraGraph`/`paraGraphWithSeed` docs, adds a dedicated user doc at `docs/reference/features/para-graph.md`, and adds test coverage for within-bucket ordering and cycle behaviour.

## Technical Context

**Language/Version**: Haskell (GHC 9.12.2)
**Primary Dependencies**: `Data.Map.Strict`, `Data.List (foldl')`, `Data.Maybe (mapMaybe)` — all already imported in Transform.hs
**Storage**: N/A — pure in-memory algorithm
**Testing**: hspec (`libs/pattern/tests/Spec/Pattern/Graph/TransformSpec.hs`); property tests used elsewhere in project
**Target Platform**: Library, platform-agnostic
**Project Type**: Single library project
**Performance Goals**: `topoShapeSort` is called once per `paraGraph` invocation. Within-bucket toposort is O(n log n) vs. O(n) for the current bucket-filter approach — acceptable for all practical graph sizes
**Constraints**: `topoShapeSort` must add `GraphValue v` constraint (needed to call `identify` for dependency tracking); this propagates to any call site but `paraGraph` and `paraGraphWithSeed` already carry `GraphValue v`, so no caller-visible API change
**Scale/Scope**: Single module (`Pattern.Graph.Transform`), one internal function rename, one new test group, one new doc file

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Code Quality | ✅ Pass | Rename + algorithm correction + inline "why" comment on cycle handling |
| II. Testing Standards | ✅ Pass | New tests required: annotation-of-annotation ordering, `GOther`-of-`GOther`, cycle soft-failure, `paraGraph` with within-bucket deps (SC-006) |
| III. Conceptual Consistency | ✅ Pass | `topoShapeSort` name aligns with `classifyByShape`; "topological" accurately describes the dependency ordering |
| IV. Mathematical Clarity | ✅ Pass | `para-graph.md` doc provides formal description of processing order and dependency contract |
| V. Multi-Language Reference Alignment | ✅ Pass | `para-graph.md` must address cross-language portability of Kahn's algorithm approach |

*No gate failures. No Complexity Tracking entry required.*

## Project Structure

### Documentation (this feature)

```text
specs/037-topo-shape-sort/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── Transform.hs     # Updated type signatures
└── tasks.md             # Phase 2 output (/speckit.tasks — NOT created here)
```

### Source Code

```text
libs/pattern/src/Pattern/Graph/
└── Transform.hs          # topoShapeSort rename + algorithm + docs update

libs/pattern/tests/Spec/Pattern/Graph/
└── TransformSpec.hs      # New within-bucket ordering + cycle tests

docs/reference/features/
└── para-graph.md         # NEW: user doc for paraGraph mental model
```

## Phase 0: Research

See [research.md](./research.md).

## Phase 1: Design

### Algorithm Design: `topoShapeSort`

The function splits into two concerns:
1. **Inter-bucket ordering** (unchanged): nodes → relationships → walks → annotations → other
2. **Within-bucket ordering** (new): Kahn's algorithm applied to `GAnnotation` and `GOther` buckets

`GNode`, `GRelationship`, and `GWalk` need no within-bucket sort:
- `GNode` elements contain nothing — no within-bucket deps possible
- `GRelationship` elements contain only nodes (cross-bucket) — no within-bucket deps
- `GWalk` elements contain only relationships (cross-bucket) — no within-bucket deps

Only `GAnnotation` (1 element of any type) and `GOther` (arbitrary elements) can reference elements of their own class, requiring within-bucket topological ordering.

**Kahn's algorithm for a single bucket**:

```
Given: elems :: [(GraphClass extra, Pattern v)]

1. Build idMap :: Map (Id v) (GraphClass extra, Pattern v)
   idMap = { identify (value p) → (cls, p) | (cls, p) ← elems }

2. For each element p, find in-bucket dependencies:
   inBucketDepsOf p = { identify (value e) | e ← elements p
                                            , identify (value e) ∈ dom(idMap) }

3. Build:
   inDegree :: Map (Id v) Int
   inDegree[p] = |inBucketDepsOf p|

   dependents :: Map (Id v) [Id v]
   dependents[e] = { p | e ∈ inBucketDepsOf p }

4. Queue ← { p | inDegree[p] = 0 }

5. While Queue non-empty:
   - Dequeue p; append to output
   - For each q in dependents[p]:
     inDegree[q] -= 1
     if inDegree[q] = 0 then enqueue q

6. Remaining elements (inDegree > 0, i.e., in cycles):
   Append in encountered order (soft failure — no error)
```

**Constraint**: `topoShapeSort` requires `GraphValue v` (for `identify`) and `Ord (Id v)` (already implied by `GraphValue`). This is a breaking change to the internal type signature only — `sortByArity` currently has no constraint. Since `topoShapeSort` is not exported and both `paraGraph` and `paraGraphWithSeed` already carry `GraphValue v`, no external API is affected.

### Documentation Design: `para-graph.md`

Follows the style and structure of `docs/reference/features/paramorphism.md`:
- Overview: what `paraGraph` does, how `topoShapeSort` enables it
- Mental model: shape classes, containment order, within-bucket ordering
- Processing contract: what `subResults` contains (including silent-miss for cycles)
- Function reference: `paraGraph`, `paraGraphFixed`, `paraGraphWithSeed`
- Worked example: annotation-of-annotation fold
- Edge cases: cycles, empty buckets, single-element graphs
- Cross-language notes

See [data-model.md](./data-model.md), [quickstart.md](./quickstart.md), and [contracts/Transform.hs](./contracts/Transform.hs) for Phase 1 artifacts.
