# RFC-006: Scope Unification — ScopeQuery and paraWithScope

**Status:** draft
**Date:** 2026-03-17
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** [`proposals/scope-unification-proposal.md`](../scope-unification-proposal.md)
**Depends on:** RFC-005 (GraphQuery)
**Followed by:** RFC-007 (RepresentationMap)
**Related modules:** `Pattern.Core`, `Pattern.Graph.Transform`

## Summary

Introduce `ScopeQuery` as a typeclass abstracting over *what is in scope* for a fold
operation, and `paraWithScope` as the unified primitive fold that subsumes both `para`
and `paraGraph` as derivable special cases. The insight: `para` and `paraGraph` are the
same operation at different scope boundaries, where the scope boundary is determined by
a containing element, not by the query interface used to access it.

This is a purely additive refactoring. No existing API is removed or broken. No behavior
changes.

## Motivation

`pattern-hs` has two fold operations with overlapping semantics:

- **`para`** — a paramorphism over `Pattern v`. Each node receives its own value and the
  bottom-up results of its direct children. Scope is implicit: the fold sees only the
  immediate subtree.
- **`paraGraph`** — a fold over a `GraphView`. Each element receives a `GraphQuery v`
  giving access to the full graph context. Scope is the enclosing `GraphView`.

These look like different operations, but they are the same operation at different scope
boundaries. The difference is not "pattern vs. graph" — it is **which containing element
defines the scope**: in `para`, scope is the current node's immediate children; in
`paraGraph`, scope is the enclosing `GraphView` element.

Making this explicit has two benefits:
1. `para` and `paraGraph` can be redefined as derived operations, eliminating duplication.
2. New scope kinds (a gram document, a project corpus, a subgraph) can be introduced
   without adding new fold primitives.

## Design

### `ScopeQuery` — the Typeclass

A `ScopeQuery` is a query interface into a scope defined by some containing element:

```haskell
-- Pattern.Core

class ScopeQuery q v where
  type ScopeId q v

  containers  :: q v -> Pattern v -> [Pattern v]
    -- ^ patterns that directly contain this element within the scope
  siblings    :: q v -> Pattern v -> [Pattern v]
    -- ^ co-elements of the same immediate parent within the scope
  byIdentity  :: q v -> ScopeId q v -> Maybe (Pattern v)
    -- ^ look up any element in scope by identity
  allElements :: q v -> [Pattern v]
    -- ^ enumerate all elements within the scope
```

The type variable `q` is the scope query implementation. Different containers instantiate
`q` differently: `TrivialScope` for subtree scope, graph-view-backed dictionaries for
graph-wide answers, or any future scope the caller defines. The caller is always
responsible for constructing the scope — it is not inferred from the fold target.

### `TrivialScope` — Immediate-Children Scope

The simplest instance. Scope is a single `Pattern v` and covers only its subtree:

```haskell
-- Pattern.Core

newtype TrivialScope v = TrivialScope (Pattern v)

trivialScope :: Pattern v -> TrivialScope v
trivialScope = TrivialScope

instance ScopeQuery TrivialScope v where
  type ScopeId TrivialScope v = Int
  containers  _ _               = []   -- no parent information in trivial scope
  siblings    _ _               = []   -- no sibling information in trivial scope
  byIdentity  (TrivialScope p) i = subtreeElementAt p i
  allElements (TrivialScope p)   = subtreeElements p
```

### `ScopeDict` — First-Class Value Form

For situations where a `ScopeQuery` instance must be stored in a data structure or passed
to a non-polymorphic function, a `ScopeDict` record is provided alongside the typeclass:

```haskell
-- Pattern.Core

data ScopeDict i v = ScopeDict
  { dictContainers  :: Pattern v -> [Pattern v]
  , dictSiblings    :: Pattern v -> [Pattern v]
  , dictByIdentity  :: i -> Maybe (Pattern v)
  , dictAllElements :: [Pattern v]
  }

instance ScopeQuery (ScopeDict i) v where
  type ScopeId (ScopeDict i) v = i
  containers  d = dictContainers d
  siblings    d = dictSiblings d
  byIdentity  d = dictByIdentity d
  allElements d = dictAllElements d

toScopeDict :: ScopeQuery q v => q v -> ScopeDict (ScopeId q v) v
toScopeDict q = ScopeDict
  { dictContainers  = containers q
  , dictSiblings    = siblings q
  , dictByIdentity  = byIdentity q
  , dictAllElements = allElements q
  }
```

`ScopeDict` is an escape hatch, not the primary interface. Prefer the typeclass form in
function signatures; use `ScopeDict` only when a first-class value is genuinely needed.

### Graph-Wide Scope Reification

The existing `GraphQuery v` record alone is not sufficient to answer generic `ScopeQuery`
operations such as `allElements` and graph-wide `byIdentity`, because it only exposes
nodes, relationships, and graph-topology queries. A full `GraphView` (see RFC-008)
contains the complete classified element set:

```haskell
-- Pattern.Graph.Transform

scopeDictFromGraphView :: GraphValue v => GraphView extra v -> ScopeDict (Id v) v
```

This design makes an important invariant explicit: graph-wide `byIdentity` is well-defined
only when `Id v` is unique across the classified `GraphView` snapshot. Duplicate
identities should be rejected rather than silently collapsed.

### `paraWithScope` — the Unified Primitive

```haskell
-- Pattern.Core

paraWithScope
  :: ScopeQuery q v
  => q v                               -- scope: the containing element's query interface
  -> (q v -> Pattern v -> [r] -> r)   -- algebra: scope, current node, child results
  -> Pattern v                         -- root pattern to fold over
  -> r
```

The algebra receives three arguments: the scope query (for lookups and context), the
current pattern node, and the bottom-up results of the current node's direct children.
The fold is bottom-up. The scope is fixed for the duration of the fold.

### Deriving `para` and `paraGraph`

```haskell
-- Pattern.Core

para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f p = paraWithScope (trivialScope p) (\_ pat rs -> f pat rs) p
```

`para` is unchanged at every call site. The `ScopeQuery` parameter is hidden behind
`trivialScope`. Behavior is identical to the current implementation.

```haskell
-- Pattern.Graph

paraGraph
  :: (GraphQuery v -> Pattern v -> [r] -> r)
  -> GraphView extra v
  -> Map (Id v) r
paraGraph f view = ...
```

`paraGraph` is preserved as a graph-specific wrapper. It folds over a classified
`GraphView`, not a single rooted `Pattern`, and passes `GraphQuery v` into the algebra —
preserving the existing scheduling and cycle-softening semantics. The unified generic
scope layer is exposed separately via `scopeDictFromGraphView`.

### `GraphScope` — Graph-Topology Extension (Follow-on)

A natural follow-on is a `GraphScope` subclass adding operations meaningful only when
scoped patterns are graph-classified (`GNode`, `GRelationship`, etc.):

```haskell
-- Pattern.Graph

class ScopeQuery q v => GraphScope q v where
  source    :: q v -> Pattern v -> Maybe (Pattern v)
  target    :: q v -> Pattern v -> Maybe (Pattern v)
  incidents :: q v -> Pattern v -> [Pattern v]
  degree    :: q v -> Pattern v -> Int
  nodes     :: q v -> [Pattern v]
  rels      :: q v -> [Pattern v]
```

This RFC does not implement `GraphScope`; it stops at the generic scope layer.

### Interface Convention

This RFC establishes the going-forward convention for new interfaces in `pattern-hs`:

1. Define the interface as a typeclass
2. Provide a `*Dict` record as the first-class value form, with a `toScopeDict`
   reification function
3. Reify existing records into the new interface when they satisfy it without losing
   information
4. Document the cross-language correspondence

**Cross-language correspondence:**

| Haskell | Rust | Python / TypeScript (via WASM) |
|---|---|---|
| Typeclass | Trait | Protocol / Interface |
| `*Dict` record | Struct implementing trait | Concrete class |
| `toScopeDict` | `impl Trait for Struct` | Protocol conformance |

Existing `GraphQuery` and `GraphClassifier` records are **not** converted to typeclasses
— that would be a breaking change with limited benefit. New interfaces follow this
convention going forward.

### Module Placement

| Definition | Module | Action |
|---|---|---|
| `ScopeQuery` typeclass | `Pattern.Core` | **New** |
| `TrivialScope` + `trivialScope` | `Pattern.Core` | **New** |
| `ScopeDict` + `toScopeDict` | `Pattern.Core` | **New** |
| `paraWithScope` | `Pattern.Core` | **New** |
| `para` | `Pattern.Core` | **Redefine** (no API change) |
| `scopeDictFromGraphView` | `Pattern.Graph.Transform` | **New** |
| Internal `GraphViewScope` adapter | `Pattern.Graph.Transform` | **New (internal)** |
| `paraGraph` | `Pattern.Graph.Transform` | **Preserve wrapper** (no API change) |

### Implementation Sequence

1. Confirm `subtreeElementAt` and `subtreeElements` exist in `Pattern.Core`
2. Add `ScopeQuery`, `TrivialScope`, `ScopeDict`, `toScopeDict` to `Pattern.Core`
3. Add `paraWithScope`; redefine `para` in terms of it; run existing `para` test suite
4. Add `scopeDictFromGraphView` to `Pattern.Graph.Transform`; run existing `paraGraph` tests

### Acceptance Criteria

1. `ScopeQuery`, `TrivialScope`, `ScopeDict`, `toScopeDict`, `paraWithScope` are in
   `Pattern.Core` and exported.
2. `scopeDictFromGraphView` is in `Pattern.Graph.Transform` and exported.
3. `para` and `paraGraph` pass their existing test suites without modification.
4. Full `pattern-hs` test suite passes with no failures.
5. No existing call sites required changes.

## Open Questions

None. The design is fully specified above.

## Alternatives

**Existential wrapper over `ScopeQuery q v`** — preserving polymorphism through an
existential rather than using `ScopeDict` was considered. Rejected: the aspect
combinators are closed operations on scopes (scopes in, scopes out), and the concrete
record representation is sufficient. The existential wrapper would add machinery without
payoff at the framework layer.

**Unifying `para` and `paraGraph` into a single function** — by making `paraGraph` also
call `paraWithScope` directly. Rejected to preserve `paraGraph`'s graph-specific
scheduling and cycle-softening semantics, which are meaningfully different from a
simple bottom-up fold over a rooted `Pattern`.
