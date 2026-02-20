# Proposal: GraphMutation — Coherent In-Memory Graph Mutations

**Status**: 📝 Design Only  
**Date**: 2026-02-19  
**Depends on**: GraphClassifier proposal, GraphQuery proposal, GraphTransform proposal  
**Relates to**: Feature 23 (GraphLens), Feature 33 (PatternGraph)

---

## Summary

Introduce `GraphMutation v` as a record-of-functions providing merge, update, and delete
operations over a graph representation. `mergeElement` is the single entry point for all
insertions — total, classifier-driven, recursively decomposing. `updateValue` is partial,
failing only when the element does not exist. `deleteElement` takes a `Substitution v`
that controls what happens to containers when an element is removed — from the safe
default of re-classifying containers after their arity drops, to a full upward cascade.
`GraphMutation` is a companion to `GraphQuery`: together they form the complete read/write
interface over a graph, each independently constructable from any backing representation.

---

## Motivation

### Why mutations need coherence checking

A `PatternGraph` has multi-element structural invariants: a relationship must have two
endpoint nodes, a walk's constituent relationships must exist as independent entries, an
annotation's inner element must exist.

For insertion, these invariants are maintained automatically by `mergeElement`'s
recursive decomposition: merging a relationship auto-merges its endpoint nodes; merging
a walk auto-merges its constituent relationships and their nodes. The graph is always
coherent after a merge because the operation ensures its own preconditions.

For deletion, the invariants cannot be maintained automatically without knowing the
caller's intent. When an element is removed from the graph, any container that referenced
it loses one element from its `elements` list — dropping one arity. A relationship losing
a node becomes an annotation; an annotation losing its inner element becomes a node. The
caller must declare via a `Substitution v` what should happen: allow the re-classification,
replace the removed element with something else, or cascade the deletion upward.

Value updates do not affect structure. Since the canonical classifier operates on
`elements` not on `value`, updating a value never shifts an element's category.
`updateValue` fails only when the element does not exist.

### Why a record-of-functions

Consistent with `GraphClassifier` and `GraphQuery`, `GraphMutation` is a
record-of-functions rather than a typeclass. This makes it:

- **Portable** — maps to interfaces (Java), structs of function pointers (Rust/C),
  plain objects (TypeScript) without typeclass machinery
- **Composable** — a `GraphMutation` can be wrapped with logging, validation, or
  audit trail functions using the same `GraphMutation v -> GraphMutation v` pattern
  established by `GraphQuery`
- **Future-compatible** — a database-backed graph produces a `GraphMutation v` that
  closes over a connection. The mutation surface is identical to the in-memory version;
  only the backing implementation differs. This is deferred but the design accommodates
  it without change.

---

## Design

### Error type

```haskell
data GraphMutationError v
  = ElementNotFound (Id v)
```

One case. `deleteElement` cannot fail due to containers — the `Substitution v` parameter
declares how containers are handled, so there is nothing to reject. `updateValue` cannot
fail due to classification — value updates do not affect structure. The only genuine
failure for either operation is attempting to act on an element that does not exist.

`GuardedDelete` behaviour — surfacing what would be affected before committing — is
achieved by the caller consulting `queryContainers` from `GraphQuery` before calling
`deleteElement`. This keeps the decision logic at the call site where the intent lives.

### `GraphMutation` — the interface

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
with `ElementNotFound`. `updateValues` and `deleteElements` are bulk variants that
accumulate errors rather than short-circuiting — they return all errors encountered
alongside the best-effort updated graph. This is the correct behavior for ETL pipelines
where missing elements should be logged and skipped rather than aborting the operation.

### `Substitution` — deletion policy for containers

```haskell
data Substitution v
  = NoSubstitution                             -- remove element; containers drop arity and re-classify
  | SubstituteWith (Pattern v)                 -- replace with a specific element in all containers
  | SubstituteBy   (Pattern v -> [Pattern v])  -- derive zero or more replacements from each container
  | CascadeDelete                              -- delete all containers recursively, then the element
```

`SubstituteBy` returns `[Pattern v]` — a list of zero or more patterns to merge into the
graph in place of the modified container:

- **`[]`** — delete this container (dissolve it)
- **`[p]`** — replace the container with `p`
- **`[p, q, ...]`** — split the container into multiple patterns, each re-merged
  independently

This makes `SubstituteWith p` exactly sugar for `SubstituteBy (const [p])`, and gives
`SubstituteBy` the expressive power to handle all container outcomes including deletion
and splitting.

**`NoSubstitution`** is the default. The element is removed from each container's
`elements` list. Each container drops one arity and is re-merged via `mergeElement`,
which re-classifies it: a relationship losing a node becomes an annotation; an annotation
losing its inner element becomes a node; a walk losing a relationship that breaks its
chain goes to `pgOther`. The graph remains coherent; categories shift to reflect the new
structure.

**`SubstituteWith p`** replaces the deleted element with `p` in every container's
`elements` list before re-merging. Arity is preserved; only the referenced element
changes. Useful for replacing a node with a placeholder or successor without rewiring
every relationship manually.

**`SubstituteBy f`** applies `f` to each container (with the deleted element already
removed from its `elements`) and re-merges the results. The empty list dissolves the
container; a singleton replaces it; multiple patterns split it. Receives the modified
container, not the original, so `f` can inspect the post-deletion structure.

**`CascadeDelete`** is the nuclear option. All containers of the element are deleted
first, recursively applying `CascadeDelete` to their own containers, until no containers
remain. Then the element itself is deleted. Uses `queryContainers` at each level.

The guarded-delete pattern — inspect before committing — is not a `Substitution`
variant. It is a call-site idiom using `queryContainers`:

```haskell
-- Inspect before deleting
let containers = queryContainers gq element
in if null containers
   then deleteElement mut elementId NoSubstitution graph
   else -- caller decides: which Substitution to use, or abort
```

### Substitution combinators

The library provides named combinators and a structural helper for common `SubstituteBy`
patterns:

```haskell
-- Dissolve the container entirely (delete it from the graph)
dissolve :: Substitution v
dissolve = SubstituteBy (const [])

-- Replace the deleted element with a specific pattern (arity preserved)
-- Same as SubstituteWith; provided for naming symmetry with dissolve
reconnect :: Pattern v -> Substitution v
reconnect p = SubstituteWith p

-- Structural helper: partition a walk's remaining elements into contiguous
-- chains of valid relationships, after the deleted element has been removed.
-- Returns each chain as a list of relationship patterns.
-- Useful inside SubstituteBy when the caller wants to handle fragments
-- deliberately, naming and constructing them with full control.
partitionChains :: GraphClassifier extra v -> Pattern v -> [[Pattern v]]
```

When splitting produces meaningful fragments — entities that will be queried, displayed,
or updated independently — the caller uses `SubstituteBy` directly with `partitionChains`
as the structural building block, supplying identities and values for each fragment
explicitly:

```haskell
let splitRoute container =
      let chains = partitionChains classifier container
      in  zipWith (\v chain -> Pattern v chain)
                  [routeAValue, routeBValue]
                  chains
deleteElement mut hospitalId (SubstituteBy splitRoute) graph
```

The library does not provide combinators that generate fragment identities automatically.
When fragment identity matters — which is the meaningful case — it belongs to the caller.

### `mergeElement` behaviour

`mergeElement` classifies the incoming pattern via `GraphClassifier`, then dispatches:

- **`GNode`** — stored in `pgNodes`. If identity exists, `ReconciliationPolicy` applies.
- **`GRelationship`** — endpoint nodes are recursively merged first, then the
  relationship is stored in `pgRelationships`. Always coherent: endpoints are guaranteed
  to exist because they were just merged.
- **`GWalk`** — constituent relationships are recursively merged (which merges their
  nodes), then the walk is stored in `pgWalks`.
- **`GAnnotation`** — the inner element is recursively merged, then the annotation is
  stored in `pgAnnotations`.
- **`GOther extra`** — stored in `pgOther`. No coherence requirement; the classifier
  has determined this pattern doesn't fit the named categories.

Recursive decomposition means the caller never needs to manually ensure preconditions.
Merging a walk is sufficient to populate nodes, relationships, and the walk itself.

### `updateValue` behaviour

- Looks up the element by identity across all maps; fails with `ElementNotFound` if absent
- Applies the new value in place within the appropriate map
- Classification is unaffected: the canonical classifier operates on `elements` not
  `value`, so no re-classification is needed. Custom classifiers that inspect `value`
  are out of scope for this proposal.

### `deleteElement` behaviour

- Looks up the element by identity across all maps; fails with `ElementNotFound` if absent
- Uses `queryContainers` from `GraphQuery` to find all higher-order structures containing
  the element
- Applies `Substitution v` to each container:
  - `NoSubstitution`: removes the element from each container's `elements` list and
    re-merges the modified container (re-classification follows naturally)
  - `SubstituteWith p`: replaces the element with `p` in each container's `elements`
    list and re-merges
  - `SubstituteBy f`: applies `f` to each container (element already removed); re-merges
    each pattern in the returned list — `[]` dissolves, `[p]` replaces, `[p,q,...]` splits
  - `CascadeDelete`: recursively deletes all containers with `CascadeDelete`, then
    deletes the element itself
- Deletes the element from its map after all containers are handled

### Constructing a `GraphMutation`

The canonical in-memory implementation is provided by the library:

```haskell
canonicalMutation :: (GraphValue v, Eq v)
                  => GraphClassifier extra v
                  -> GraphView extra v    -- provides queryContainers for deleteElement
                  -> GraphMutation v
```

`GraphView` replaces the earlier `GraphQuery v` parameter — `GraphView` contains the
`GraphQuery` and provides classified element enumeration, which is needed by the
mutation layer to know which map an element lives in. `GraphView` must be settled
(in the GraphTransform proposal) before this constructor is finalized.

For the common case, a convenience constructor takes a `PatternGraph` and derives both:

```haskell
defaultMutation :: (GraphValue v, Eq v) => PatternGraph v -> GraphMutation v
defaultMutation pg = canonicalMutation canonicalClassifier (fromPatternGraph canonicalClassifier pg)
```

---

## Usage

### Insertion and merge

All insertions go through `mergeElement`. The `ReconciliationPolicy` controls what
happens when an element with the same identity already exists:

```haskell
let mut = defaultMutation graph

-- Insert-or-update: last write wins on conflict
let graph' = mergeElement mut LastWriteWins nodePattern graph

-- Insert-or-fail on conflict (Strict policy rejects duplicates at reconciliation)
let graph' = mergeElement mut Strict nodePattern graph

-- Merge a relationship: endpoint nodes are auto-inserted if absent
let graph' = mergeElement mut LastWriteWins relPattern graph

-- Merge a walk: relationships and nodes are all auto-inserted
let graph' = mergeElement mut LastWriteWins walkPattern graph
```

### Update

```haskell
-- Update a value in place; fails only if element does not exist
case updateValue mut nodeId newValue graph of
  Left (ElementNotFound i) -> -- element doesn't exist
  Right graph'             -> -- value updated; structure and category unchanged
```

### Deletion

```haskell
-- Default: remove element, containers drop arity and re-classify via re-merge
case deleteElement mut nodeId NoSubstitution graph of
  Left (ElementNotFound i) -> -- element didn't exist
  Right graph'             -> -- element removed; affected containers re-classified

-- Replace with a placeholder in all containers (arity preserved)
case deleteElement mut nodeId (SubstituteWith placeholder) graph of
  Left (ElementNotFound i) -> -- element didn't exist
  Right graph'             -> -- element replaced in all containers

-- Dissolve all containers
case deleteElement mut nodeId dissolve graph of
  Left (ElementNotFound i) -> -- element didn't exist
  Right graph'             -> -- element and all its containers removed

-- Split a walk into named fragments (caller supplies fragment values and identities)
let splitRoute container =
      let chains = partitionChains classifier container
      in  zipWith (\v chain -> Pattern v chain)
                  [routeAValue, routeBValue]
                  chains
case deleteElement mut hospitalId (SubstituteBy splitRoute) graph of
  Left (ElementNotFound i) -> -- element didn't exist
  Right graph'             -> -- walk split; named fragments merged into graph

-- Custom: per-case dispatch on what each container becomes
let policy container = case classify classifier container of
      GWalk -> []           -- dissolve broken walks
      _     -> [container]  -- re-classify everything else normally
case deleteElement mut nodeId (SubstituteBy policy) graph of
  Left (ElementNotFound i) -> -- element didn't exist
  Right graph'             -> -- custom policy applied to each container

-- Nuclear: delete element and full containment closure recursively
case deleteElement mut nodeId CascadeDelete graph of
  Left (ElementNotFound i) -> -- element didn't exist
  Right graph'             -> -- element and all containers at all levels removed

-- Guarded pattern: inspect before committing (call-site idiom)
let containers = queryContainers gq nodePattern
in if null containers
   then deleteElement mut nodeId NoSubstitution graph
   else -- inspect containers, choose Substitution, or abort
```

### Structural update (rewire a relationship)

```haskell
rewire :: GraphMutation v -> Id v -> Pattern v -> PatternGraph v
       -> Either (GraphMutationError v) (PatternGraph v)
rewire mut oldRelId newRelPattern graph = do
  graph' <- deleteElement mut oldRelId NoSubstitution graph
  Right (mergeElement mut LastWriteWins newRelPattern graph')
```

---

## Composability

`GraphMutation` values compose with the same wrapper pattern as `GraphQuery`:

### Audit logging

```haskell
auditMutation :: (String -> IO ()) -> GraphMutation v -> GraphMutation v
```

Wraps each operation to emit a log entry before and after application. Useful for
debugging or building an audit trail without touching operation code.

### Dry-run / validation only

```haskell
dryRunMutation :: GraphMutation v -> GraphMutation v
```

Replaces all operations with their validation pass only — returns the error or a
`Right` with the *original* graph unchanged. Useful for pre-flight checks.

---

## Relationship to existing code

### `PatternGraph.merge`

The existing `merge` function is subsumed by `GraphMutation.mergeElement`. The current
`merge` API is retained as a convenience wrapper:

```haskell
-- Pattern.PatternGraph (backward compat)
merge :: (GraphValue v, ...) => Pattern v -> PatternGraph v -> PatternGraph v
merge p g = mergeElement (defaultMutation g) LastWriteWins p g
```

### `PatternGraph.fromPatterns`

Similarly retained as a convenience wrapper over `mergeElement` with `LastWriteWins`.

---

## What changes in pattern-hs

| Component | Action | Notes |
|---|---|---|
| `Pattern.Graph.GraphMutation` | **New** | Five-function record including bulk variants |
| `Pattern.Graph.GraphMutationError` | **New** | Single-case error type: `ElementNotFound` |
| `Pattern.Graph.Substitution` | **New** | `NoSubstitution`, `SubstituteWith`, `SubstituteBy :: Pattern v -> [Pattern v]`, `CascadeDelete` |
| `Pattern.Graph.dissolve` | **New** | Combinator: `SubstituteBy (const [])` |
| `Pattern.Graph.reconnect` | **New** | Combinator: alias for `SubstituteWith` |
| `Pattern.Graph.partitionChains` | **New** | Structural helper: contiguous valid chains in a walk's elements |
| `Pattern.Graph.canonicalMutation` | **New** | Canonical in-memory `GraphMutation` constructor |
| `Pattern.Graph.defaultMutation` | **New** | Convenience constructor from `PatternGraph` |
| `Pattern.PatternGraph.merge` | **Retain as wrapper** | Delegates to `mergeElement` with `LastWriteWins` |
| `Pattern.PatternGraph.fromPatterns` | **Retain as wrapper** | Delegates to `mergeElement` fold |

---

## Open questions

1. **`DetachDelete` semantics for walks** — removing a relationship from a walk's element
   list may leave the walk structurally invalid (broken chain). Should `DetachDelete` on
   a relationship that is part of a walk dissolve the walk entirely, split it into two
   shorter walks, or leave the gap and route it to `pgOther`? Requires a decision about
   walk identity after structural modification.

---

## Summary of decisions

- **`GraphMutation` lives in `Pattern.Graph`**: alongside `GraphQuery`, `GraphView`, and
  `GraphClassifier`. These are the core graph capability types; keeping them co-located
  maintains cohesive capability namespaces and is consistent with the rest of the design.
- **`GraphMutation` is a record-of-functions**: portable, composable, future-compatible
  with database backends. Consistent with `GraphClassifier` and `GraphQuery` design.
- **Five functions, one error case**: `mergeElement` is total; `updateValue` and
  `deleteElement` return `Either` failing only with `ElementNotFound`; `updateValues`
  and `deleteElements` accumulate errors for bulk operations.
- **Bulk variants accumulate errors**: `updateValues` and `deleteElements` return
  `([GraphMutationError v], PatternGraph v)` — all errors collected, best-effort graph
  returned. Short-circuiting on first missing element is the wrong behavior for ETL
  pipelines.
- **`mergeElement` is the single insertion entry point**: classification, recursive
  decomposition, and reconciliation policy together handle all cases. Nothing to reject.
- **Deletion takes a `Substitution v`**: containers are never left in an undefined state.
  `NoSubstitution` (default) drops arity and re-classifies via re-merge; `SubstituteWith`
  preserves arity; `SubstituteBy :: Pattern v -> [Pattern v]` gives full control — empty
  list dissolves, singleton replaces, multiple patterns split; `CascadeDelete` is the
  nuclear option. Guarded-delete is a call-site idiom using `queryContainers`.
- **`SubstituteWith` is sugar for `SubstituteBy (const [p])`**: the list return type
  unifies all container outcomes in one constructor.
- **Tools not solutions**: the library provides `partitionChains` as a structural
  building block and `dissolve`/`reconnect` as named idioms. Fragment identity generation
  is always the caller's responsibility.
- **Arity drop and re-classification via re-merge**: removing an element from a container
  and re-merging it produces a valid, re-classified result. The graph is never incoherent
  after deletion.
- **Value updates do not affect classification**: `updateValue` is a pure value swap.
  Classification depends on `elements`, not `value`.
- **`queryContainers` is the foundation of deletion**: all container-handling logic is
  expressed in terms of the `GraphQuery` primitive.
- **Structural updates are delete-then-merge**: keeps `GraphMutation` small and intent
  explicit. Dedicated structural constructors deferred.
- **Existing `merge` and `fromPatterns` retained as wrappers**: backward compatibility
  preserved.
- **Database integration is deferred**: the record-of-functions design accommodates it
  without change. Out of scope for this proposal.
- **Normalization is out of scope**: as established in the GraphClassifier proposal.
- **Implementation order**: GraphClassifier → GraphQuery → GraphTransform → GraphMutation.
  `GraphMutation` depends on `GraphView` (from GraphTransform) for its construction
  path, and on `Substitution` (defined in shared types during GraphTransform) for
  deletion. `GraphMutation` is the final foundational feature.
