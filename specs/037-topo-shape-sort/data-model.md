# Data Model: topoShapeSort

**Feature**: 037-topo-shape-sort
**Date**: 2026-02-24

## Overview

`topoShapeSort` operates on `[(GraphClass extra, Pattern v)]` — a list of classified graph elements. Its job is to produce a reordering of this list such that every element appears after all elements it structurally depends on (i.e., contains as sub-elements).

No new persistent data types are introduced. The algorithm uses transient structures local to `topoShapeSort`.

---

## Algorithm State (transient, within `topoShapeSort`)

### `idMap :: Map (Id v) (GraphClass extra, Pattern v)`

Maps each element's identity to its entry. Used to determine whether a sub-element belongs to the current bucket.

- **Key**: `identify (value p)` — canonical identity of the element's value
- **Value**: the `(GraphClass extra, Pattern v)` pair from the input list
- **Purpose**: O(log n) lookup to check "is sub-element e in this bucket?"

### `inBucketDeps :: Map (Id v) [Id v]`

For each element, the list of identities of other elements in the same bucket that it directly depends on.

- `inBucketDeps[p]` = `[ identify (value e) | e ← elements p, identify (value e) ∈ dom(idMap) ]`
- An element with `inBucketDeps[p] = []` has no within-bucket dependencies and can be processed immediately.

### `dependents :: Map (Id v) [Id v]`

Inverse of `inBucketDeps`: for each element, which other elements in this bucket depend on it.

- `dependents[e]` = `[ p | p ← elems, e ∈ inBucketDeps[p] ]`
- Used to determine which elements become "unblocked" when `e` is output.

### `inDegree :: Map (Id v) Int`

Count of unresolved within-bucket dependencies for each element.

- Initially: `inDegree[p] = length (inBucketDeps[p])`
- Decremented when a dependency is output: `inDegree[q] -= 1` for each `q` in `dependents[p]`
- When `inDegree[q] = 0`: `q` is ready to be output

### `queue :: [Id v]` (processing order)

Elements with `inDegree = 0` are queued for output. Initialised with all zero-in-degree elements in encountered order (preserves input order for cycle-free peers).

---

## Shape Class Dependency Model

```
GNode          ←──────────────────────────────────────────┐
                   (contained by)                          │
GRelationship  ← contains 2 × GNode                       │
                                                           │
GWalk          ← contains N × GRelationship               │
                                                           │
GAnnotation    ← contains 1 × (any class, including GAnnotation) ┘
                                              ↑
GOther         ← contains N × (any class, including GOther)
```

Cross-bucket arrows are handled by the fixed inter-bucket ordering.
Within-bucket arrows (e.g., `GAnnotation → GAnnotation`) are handled by Kahn's algorithm.

---

## Invariants

1. **Processing order**: For any element `p` with in-bucket dependency on `e`, `e` appears before `p` in the output — unless they are in a cycle.
2. **Cycle soft failure**: Elements in a dependency cycle appear after all non-cycle elements in the same bucket, in encountered input order.
3. **Cross-bucket deps unaffected**: The `GNode → GRelationship → GWalk → GAnnotation → GOther` bucket ordering is preserved unconditionally.
4. **No element dropped**: `length (topoShapeSort elems) == length elems` always.
5. **`GraphValue v` constraint required**: `topoShapeSort` requires `GraphValue v` (transitively `Ord (Id v)`) in order to call `identify`.
