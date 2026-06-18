# RFC-009: GraphMutation — Coherent In-Memory Graph Mutations

**Status:** draft
**Date:** 2026-02-19
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** [`proposals/graph-mutation.md`](../graph-mutation.md)
**Depends on:** RFC-004 (GraphClassifier), RFC-005 (GraphQuery), RFC-008 (GraphTransform)
**Related modules:** `Pattern.Graph`, `Pattern.PatternGraph`

## Summary

Introduce `GraphMutation v` as a record-of-functions providing merge, update, and delete
operations over a graph representation. `mergeElement` is the single insertion entry
point — total, classifier-driven, recursively decomposing. `updateValue` is partial,
failing only when the element does not exist. `deleteElement` takes a `Substitution v`
that controls what happens to containers when an element is removed. `GraphMutation` is
the complement to `GraphQuery` (RFC-005): together they form the complete read/write
interface over a graph.

## Motivation

A `PatternGraph` has multi-element structural invariants: a relationship must have two
endpoint nodes; a walk's constituent relationships must exist as independent entries; an
annotation's inner element must exist.

**Insertion invariants maintain themselves**: `mergeElement`'s recursive decomposition
ensures coherence — merging a relationship auto-merges its endpoint nodes; merging a walk
auto-merges its constituent relationships and their nodes. The graph is always coherent
after a merge because the operation ensures its own preconditions.

**Deletion invariants cannot maintain themselves without declaring intent**: when an
element is removed, any container that referenced it drops one arity and shifts
classification. The caller must declare via `Substitution v` what should happen: allow
re-classification, replace the removed element, or cascade the deletion.

**Value updates do not affect structure**: the canonical classifier operates on `elements`
not `value`, so updating a value never shifts an element's classification.

`GraphMutation` is a record-of-functions (consistent with `GraphClassifier` and
`GraphQuery`) because:
- **Portable** — maps to interfaces (Java), function pointer structs (Rust/C), plain
  objects (TypeScript) without typeclass machinery
- **Composable** — wrappable with logging, validation, or audit trail functions
- **Future-compatible** — a database-backed graph produces a `GraphMutation v` that
  closes over a connection; the surface is identical to in-memory

## Design

### Error Type

```haskell
data GraphMutationError v = ElementNotFound (Id v)
```

One case. `deleteElement` cannot fail due to containers — `Substitution v` declares
how containers are handled. `updateValue` cannot fail due to classification — value
updates do not affect structure. The only genuine failure is acting on a non-existent element.

### `GraphMutation` — the Interface

```haskell
data GraphMutation v = GraphMutation
  { mergeElement   :: ReconciliationPolicy (MergeStrategy v) -> Pattern v
                   -> PatternGraph v -> PatternGraph v

  , updateValue    :: Id v -> v -> PatternGraph v
                   -> Either (GraphMutationError v) (PatternGraph v)

  , deleteElement  :: Id v -> Substitution v -> PatternGraph v
                   -> Either (GraphMutationError v) (PatternGraph v)

  , updateValues   :: [(Id v, v)] -> PatternGraph v
                   -> ([GraphMutationError v], PatternGraph v)

  , deleteElements :: [Id v] -> Substitution v -> PatternGraph v
                   -> ([GraphMutationError v], PatternGraph v)
  }
```

`mergeElement` is total. `updateValue` and `deleteElement` return `Either`, failing only
with `ElementNotFound`. `updateValues` and `deleteElements` accumulate errors rather than
short-circuiting — the correct behavior for ETL pipelines where missing elements should
be logged and skipped, not abort the operation.

### `Substitution` — Deletion Policy for Containers

```haskell
data Substitution v
  = NoSubstitution                             -- remove; containers drop arity and re-classify
  | SubstituteWith (Pattern v)                 -- replace with a specific element in all containers
  | SubstituteBy   (Pattern v -> [Pattern v])  -- derive zero or more replacements per container
  | CascadeDelete                              -- delete all containers recursively, then element
```

`SubstituteBy` returns `[Pattern v]`:
- `[]` — dissolve the container
- `[p]` — replace the container with `p`
- `[p, q, ...]` — split the container into multiple patterns, each re-merged independently

`SubstituteWith p` is exactly `SubstituteBy (const [p])`.

**`NoSubstitution`** (default) removes the element from each container's `elements` list
and re-merges the modified container. Re-classification follows: a relationship losing a
node becomes an annotation; an annotation losing its inner element becomes a node.

**`CascadeDelete`** is the nuclear option: deletes all containers recursively first, then
the element itself.

**Guarded delete** is a call-site idiom using `queryContainers` (RFC-005), not a
`Substitution` variant:

```haskell
let containers = queryContainers gq element
in if null containers
   then deleteElement mut elementId NoSubstitution graph
   else -- inspect containers, choose Substitution, or abort
```

### Substitution Combinators

```haskell
dissolve   :: Substitution v
dissolve   = SubstituteBy (const [])

reconnect  :: Pattern v -> Substitution v
reconnect p = SubstituteWith p

-- Structural helper: partition a walk's remaining elements into contiguous
-- chains of valid relationships after the deleted element has been removed.
partitionChains :: GraphClassifier extra v -> Pattern v -> [[Pattern v]]
```

Fragment identity generation is always the caller's responsibility:

```haskell
let splitRoute container =
      let chains = partitionChains classifier container
      in  zipWith (\v chain -> Pattern v chain) [routeAValue, routeBValue] chains
deleteElement mut hospitalId (SubstituteBy splitRoute) graph
```

### `mergeElement` Behaviour

`mergeElement` classifies the incoming pattern and dispatches:

- **`GNode`** — stored in `pgNodes`. If identity exists, `ReconciliationPolicy` applies.
- **`GRelationship`** — endpoint nodes are recursively merged first, then stored in
  `pgRelationships`. Always coherent.
- **`GWalk`** — constituent relationships recursively merged (which merges their nodes),
  then stored in `pgWalks`.
- **`GAnnotation`** — inner element recursively merged, then stored in `pgAnnotations`.
- **`GOther extra`** — stored in `pgOther`. No coherence requirement.

### `updateValue` Behaviour

Looks up element by identity; fails with `ElementNotFound` if absent; applies new value
in place within the appropriate map. Classification is unaffected.

### Constructing a `GraphMutation`

```haskell
canonicalMutation :: (GraphValue v, Eq v)
                  => GraphClassifier extra v
                  -> GraphView extra v    -- provides queryContainers for deleteElement
                  -> GraphMutation v

defaultMutation :: (GraphValue v, Eq v) => PatternGraph v -> GraphMutation v
defaultMutation pg =
  canonicalMutation canonicalClassifier (fromPatternGraph canonicalClassifier pg)
```

`GraphView` (RFC-008) must be settled before `canonicalMutation` is finalized, since
the mutation layer needs the full classified element set to know which map an element
lives in.

### Composability

```haskell
auditMutation  :: (String -> IO ()) -> GraphMutation v -> GraphMutation v
dryRunMutation :: GraphMutation v -> GraphMutation v
```

Wrappers compose via the same `GraphMutation v -> GraphMutation v` pattern established by
RFC-005 for `GraphQuery`.

### Structural Update Pattern

Structural updates (rewiring a relationship) are expressed as delete-then-merge:

```haskell
rewire :: GraphMutation v -> Id v -> Pattern v -> PatternGraph v
       -> Either (GraphMutationError v) (PatternGraph v)
rewire mut oldRelId newRelPattern graph = do
  graph' <- deleteElement mut oldRelId NoSubstitution graph
  Right (mergeElement mut LastWriteWins newRelPattern graph')
```

### Relationship to Existing Code

`PatternGraph.merge` and `PatternGraph.fromPatterns` are retained as backward-compatible
wrappers that delegate to `mergeElement` with `LastWriteWins`.

### What Changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Graph.GraphMutation` | **New** | Five-function record including bulk variants |
| `Pattern.Graph.GraphMutationError` | **New** | `ElementNotFound` |
| `Pattern.Graph.Substitution` | **New** | Four cases including `SubstituteBy` |
| `Pattern.Graph.dissolve` | **New** | `SubstituteBy (const [])` |
| `Pattern.Graph.reconnect` | **New** | Alias for `SubstituteWith` |
| `Pattern.Graph.partitionChains` | **New** | Structural helper for walk splitting |
| `Pattern.Graph.canonicalMutation` | **New** | Canonical in-memory constructor |
| `Pattern.Graph.defaultMutation` | **New** | Convenience constructor from `PatternGraph` |
| `Pattern.PatternGraph.merge` | **Retain as wrapper** | Delegates to `mergeElement` |
| `Pattern.PatternGraph.fromPatterns` | **Retain as wrapper** | Delegates to `mergeElement` fold |

## Open Questions

1. **`DetachDelete` semantics for walks** — removing a relationship from a walk may
   leave the walk structurally invalid (broken chain). Should `DetachDelete` on a
   relationship that is part of a walk dissolve the walk entirely, split it into two
   shorter walks, or leave the gap and route it to `pgOther`? Requires a decision about
   walk identity after structural modification.

## Alternatives

**Integrated read/write type** — combining `GraphQuery` and `GraphMutation` into a single
`GraphStore` type was rejected. Keeping them separate preserves the composability story
for each independently and makes it possible to construct read-only handles (a
`GraphQuery v` without a corresponding `GraphMutation v`).

**`GuardedDelete` as a `Substitution` variant** — rejected because the decision logic
belongs at the call site where the intent lives, not inside the mutation layer.

**Dedicated structural constructors** — `rewireRelationship`, `expandNode` as first-class
operations were deferred. Delete-then-merge keeps `GraphMutation` small and intent
explicit; dedicated structural operations belong in a future higher-order graph editing
proposal.
