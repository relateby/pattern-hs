# Research: topoShapeSort

**Feature**: 037-topo-shape-sort
**Date**: 2026-02-24

## Findings

### Decision 1: Scope of `topoShapeSort`

**Decision**: Full within-bucket topological ordering, not rename-only.

**Rationale**: The `GAnnotation` and `GOther` shape classes can contain references to elements of their own class. The current bucket-sort provides no ordering guarantee within these buckets, so `paraGraph` silently produces empty `subResults` for any within-bucket dependency. Since the name `topoShapeSort` implies topological correctness, the implementation must earn that name.

**Alternatives considered**:
- Rename only, document limitation: rejected — silently wrong results are worse than the complexity of fixing it.
- Full recursive toposort across all buckets: not needed — `GNode`, `GRelationship`, `GWalk` have no within-bucket deps by construction of `classifyByShape`.

---

### Decision 2: Algorithm — Kahn's (BFS-based) toposort

**Decision**: Kahn's algorithm for within-bucket ordering of `GAnnotation` and `GOther`.

**Rationale**: Kahn's algorithm naturally handles cycle detection (elements remaining in the queue after processing are cycle members) without requiring DFS visited state. It produces a stable, deterministic output order for cycle-free inputs and degrades gracefully for cycles (leftover elements appended in encountered order).

**Alternatives considered**:
- DFS-based toposort: works but requires explicit cycle-breaking; Kahn's is simpler to reason about for the soft-failure requirement.
- Recursive dependency resolution: risks stack overflow for deep annotation chains.

---

### Decision 3: Buckets requiring within-bucket toposort

**Decision**: Apply Kahn's algorithm only to `GAnnotation` and `GOther` buckets. `GNode`, `GRelationship`, `GWalk` require no within-bucket sorting.

**Rationale**: By the definition of `classifyByShape`:
- `GNode`: `null els` — no sub-elements, no within-bucket deps
- `GRelationship`: `length els == 2 && all isNodeLike els` — sub-elements are atomic (GNode), cross-bucket dep only
- `GWalk`: `all isRelationshipLike els` — sub-elements are 2-node patterns (GRelationship), cross-bucket dep only
- `GAnnotation`: `length els == 1` — sub-element can be ANY pattern, including another annotation
- `GOther`: everything else — sub-elements are arbitrary

**Alternatives considered**: Applying toposort to all buckets — unnecessary overhead, no correctness benefit for the first three buckets.

---

### Decision 4: Cycle handling — soft failure

**Decision**: Cycle members are appended in encountered order with no error raised.

**Rationale**: `paraGraph`'s `processElem` already uses `maybe [] (:[])` — missing sub-results silently yield empty lists. Making cycle handling a hard error would be a breaking behaviour change and would reject valid (if unusual) graph data. Soft failure is consistent with existing semantics.

**Alternatives considered**:
- Hard error / exception: rejected — breaks existing use cases, inconsistent with `maybe [] (:[])`
- Cycle-breaking by fewest-unresolved-deps: more complex, non-deterministic across runs, no clear benefit over encountered-order

---

### Decision 5: `GraphValue v` constraint on `topoShapeSort`

**Decision**: Add `GraphValue v` constraint (provides `identify :: v -> Id v` and `Ord (Id v)`).

**Rationale**: Within-bucket toposort requires identity-keyed maps to track dependencies. `identify (value e)` is the canonical way to extract element identity in this codebase. The constraint propagates cleanly — both `paraGraph` and `paraGraphWithSeed` already carry `GraphValue v`, so no external API change is required.

**Alternatives considered**:
- Pass a separate identity function: unnecessary indirection; `GraphValue` already provides this
- Use structural equality on `Pattern v`: requires `Eq v`; less precise and less efficient than identity-keyed maps

---

### Codebase Findings

**`sortByArity` references**: 3 locations in `Transform.hs` (definition + 2 call sites). Not exported. No references in other source modules.

**`Pattern v` fields**: `value :: v` and `elements :: [Pattern v]` — direct record field accessors, O(1).

**`GraphValue v`**: `identify :: v -> Id v`, constrained by `Ord (Id v)`. Already required by `paraGraph` and `paraGraphWithSeed`.

**Existing tests**: `TransformSpec.hs` tests `paraGraph` for element count and node atomic behaviour only. No annotation-of-annotation or cycle tests exist. New tests needed for SC-006.

**Docs directory**: `docs/reference/features/` exists; `paramorphism.md` is the style model for the new `para-graph.md`.

**Within-bucket deps for `GWalk`**: Walks cannot be sub-elements of other walks, per `classifyByShape`. A walk requires `all isRelationshipLike els` and each relationship requires `all isNodeLike` sub-elements. This ensures no walk-of-walk is possible with the canonical classifier.
