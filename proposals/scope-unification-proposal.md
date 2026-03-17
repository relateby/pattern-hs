# Proposal: Scope Unification — `ScopeQuery` and `paraWithScope`

**Status:** Design — ready for Haskell prototype  
**Date:** 2026-03-17  
**Depends on:** GraphQuery proposal (design only; `GraphQuery` record must be in scope)  
**Part of:** `representation-map-proposal.md` (scope unification layer only)  
**Followed by:** RepresentationMap proposal (depends on this proposal being implemented)  
**Location:** `proposals/scope-unification-proposal.md` within the `pattern-hs` workspace

---

## Summary

Introduce `ScopeQuery` as a typeclass abstracting over *what is in scope* for a fold
operation, and `paraWithScope` as the unified primitive fold that replaces `para` and
`paraGraph` as derivable special cases.

This is a purely additive refactoring. No existing API is removed or broken. No
behavior changes. The goal is to reveal structure that is already implicit in
`Pattern<V>`: that `para` and `paraGraph` are the same operation at different scope
boundaries, and that the scope boundary is determined by a containing element, not by
the query interface used to access it.

This proposal is self-contained and does not depend on `GraphTransform`,
`RepresentationMap`, or `pattern-equivalence`. It can be implemented, tested, and
merged independently.

---

## Background: the current situation

`pattern-hs` currently has two fold operations:

- **`para`** — a paramorphism over `Pattern v`. Each node receives its own value and
  the bottom-up results of its direct children. Scope is implicit: the fold sees only
  the immediate subtree.

- **`paraGraph`** — a fold over a `GraphView`. Each element receives a `GraphQuery v`
  giving access to the full graph context: containers, neighbors, incident
  relationships. Scope is the whole enclosing `GraphView`.

These look like two different operations serving two different purposes. The insight
behind this proposal is that they are the same operation at different scope boundaries.
The difference is not "pattern vs. graph" — it is **which containing element defines
the scope**. In `para`, scope is the current node's immediate children. In
`paraGraph`, scope is the enclosing `GraphView` element.

Making this explicit has two benefits:

1. `para` and `paraGraph` can be redefined as derived operations, eliminating the
   duplication in their implementations.
2. New scope kinds — a gram document, a project corpus, a subgraph — can be introduced
   without adding new fold primitives. Any element that can produce a `ScopeQuery`
   value participates in the same fold.

---

## Design

### `ScopeQuery` — the typeclass

A `ScopeQuery` is a query interface into a scope defined by some containing element.
Different containers provide different operations; richer containers extend simpler ones.

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

The type variable `q` is the scope query implementation. Different containers
instantiate `q` differently: `TrivialScope` for subtree scope, `GraphView`-backed
scope dictionaries for graph-wide generic answers, and any future scope the caller
cares to define.

The caller is always responsible for constructing the scope. The scope is not inferred
from the fold target — it is passed in explicitly.

### `TrivialScope` — the immediate-children scope

The simplest `ScopeQuery` instance. Scope is defined by a single `Pattern v` and
covers only that pattern's subtree. This is the scope appropriate for `para`.

```haskell
-- Pattern.Core

newtype TrivialScope v = TrivialScope (Pattern v)

trivialScope :: Pattern v -> TrivialScope v
trivialScope = TrivialScope

instance ScopeQuery TrivialScope v where
  type ScopeId TrivialScope v = Int

  containers  _ _               = []
    -- no parent information available in trivial scope
  siblings    _ _               = []
    -- no sibling information available in trivial scope
  byIdentity  (TrivialScope p) i = subtreeElementAt p i
    -- preorder lookup within the subtree only
  allElements (TrivialScope p)   = subtreeElements p
    -- all elements reachable within the subtree
```

`subtreeElementAt` and `subtreeElements` are straightforward recursive traversals over
`Pattern v`. Both should already exist or be trivial to add in `Pattern.Core`.

### Possible follow-on: `GraphScope`

A natural follow-on is a graph-topology-specific subclass of `ScopeQuery` that adds
operations meaningful only when the scoped patterns are graph-classified
(i.e. satisfy `GNode`, `GRelationship`, etc.). This PR does not implement that
extension; it stops at the generic scope layer plus graph-wide scope reification.

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

`source`, `target`, and `incidents` are only meaningful on `GRelationship`- and
`GNode`-kinded patterns. If added later, the constraint hierarchy would mirror this:
code that needs graph topology would require `GraphScope q v`; code that needs only
containment and lookup would continue to require only `ScopeQuery q v`.

### `GraphView` provides graph-wide generic scope answers

The existing `GraphQuery v` record remains unchanged. In the implemented design,
`GraphQuery` alone is not sufficient to answer generic `ScopeQuery` operations such as
`allElements` and graph-wide `byIdentity`, because it only exposes nodes,
relationships, and graph-topology queries. Walks, annotations, and other classified
elements live in `GraphView`.

Instead, graph-wide generic scope answers are derived from a full `GraphView`
snapshot via an internal `GraphViewScope` adapter and the exported
`scopeDictFromGraphView` helper. This keeps `GraphQuery` additive-only while making
the complete classified `GraphView` available to the generic scope layer.

```haskell
-- Pattern.Graph.Transform

scopeDictFromGraphView :: GraphValue v => GraphView extra v -> ScopeDict (Id v) v
```

This design also makes an important invariant explicit: generic graph-wide
`byIdentity` is well-defined only when `Id v` is unique across the classified
`GraphView` snapshot. Duplicate identities should be rejected rather than silently
collapsed.

### `ScopeDict` — the first-class value form

For situations where a `ScopeQuery` instance needs to be stored in a data structure,
passed to a non-polymorphic higher-order function, or constructed dynamically at
runtime, a `ScopeDict` record is provided alongside the typeclass. The record is itself
a `ScopeQuery` instance, and any instance can be reified into a `ScopeDict` via
`toScopeDict`.

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

  containers  d   = dictContainers d
  siblings    d   = dictSiblings d
  byIdentity  d   = dictByIdentity d
  allElements d   = dictAllElements d

toScopeDict :: ScopeQuery q v => q v -> ScopeDict (ScopeId q v) v
toScopeDict q = ScopeDict
  { dictContainers  = containers q
  , dictSiblings    = siblings q
  , dictByIdentity  = byIdentity q
  , dictAllElements = allElements q
  }
```

`ScopeDict` is an escape hatch, not the primary interface. Prefer the typeclass form
in function signatures. Use `ScopeDict` when a first-class value is genuinely needed.

---

## The unified primitive: `paraWithScope`

```haskell
-- Pattern.Core

paraWithScope
  :: ScopeQuery q v
  => q v                               -- scope: the containing element's query interface
  -> (q v -> Pattern v -> [r] -> r)   -- algebra: scope, current node, child results
  -> Pattern v                         -- root pattern to fold over
  -> r
```

The algebra receives three arguments:
- the scope query, for lookups and context
- the current pattern node
- the bottom-up results of the current node's direct children

The fold is bottom-up. The scope is fixed for the duration of the fold — it is the
scope of the containing element, not of each node as the fold descends.

### `para` redefined

```haskell
-- Pattern.Core

para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f p = paraWithScope (trivialScope p) (\_ pat rs -> f pat rs) p
```

`para` is unchanged at every call site. The `ScopeQuery` parameter is hidden behind
`trivialScope`. Behavior is identical to the current implementation.

### `paraGraph` preserved as the graph-specific wrapper

```haskell
-- Pattern.Graph

paraGraph
  :: (GraphQuery v -> Pattern v -> [r] -> r)
  -> GraphView extra v
  -> Map (Id v) r
paraGraph f view = ...
```

`paraGraph` is unchanged at every call site and still passes a `GraphQuery v` into the
algebra, preserving the existing scheduling and cycle-softening semantics over
`GraphView`. The unified generic scope layer is exposed separately via
`scopeDictFromGraphView`; `paraGraph` itself remains a graph-specific wrapper because
it folds over a classified `GraphView`, not a single rooted `Pattern`.

### Other derived operations

Any other operations currently duplicated between `Pattern.Core` and `Pattern.Graph`
(map, filter) can be redefined against `ScopeQuery q v` in the same way. This is
deferred to a follow-on cleanup pass once `paraWithScope` is in place and the pattern
is confirmed.

---

## Module placement

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

No existing definitions are removed. No existing call sites require changes.

---

## Interface convention note

This proposal introduces `ScopeQuery` as a typeclass rather than a
record-of-functions, and sketches how follow-on interfaces like `GraphScope` would use
the same convention. This is a deliberate choice for new interfaces in `pattern-hs`
going forward, and the reasoning is documented here to be explicit.

The existing `GraphQuery` and `GraphClassifier` records are **not** converted — that
would be a breaking change with limited benefit. Instead, new generic scope behavior is
reified from `GraphView` where the full classified element set is available.

For new interfaces from this point forward:

1. Define the interface as a typeclass
2. Provide a `*Dict` record as the first-class value form, with a `toScopeDict` reification
3. Reify existing records/snapshots into the new interface when they satisfy it without losing information
4. Document the cross-language correspondence in the porting guide

**Cross-language correspondence:**

| Haskell | Rust | Python / TypeScript (via WASM) |
|---|---|---|
| Typeclass | Trait | Protocol / Interface |
| `*Dict` record | Struct implementing trait | Concrete class |
| `toScopeDict` | `impl Trait for Struct` | `__init__` + protocol conformance |
| `&dyn ScopeQuery<V>` (at boundary) | `Box<dyn ScopeQuery<V>>` | WASM-exported object |

The Rust port uses `&dyn ScopeQuery<V>` at FFI and dynamic-dispatch boundaries. Within
Rust, monomorphic trait bounds (`impl ScopeQuery<V>`) are preferred for performance.
The porting guide should document this correspondence explicitly when the Rust port of
this proposal is undertaken.

---

## Implementation sequence

### Step 1 — helpers in `Pattern.Core`

Confirm that `findInSubtree` and `subtreeElements` exist and are correct. Add them if
missing. These are simple recursive traversals; their correctness is a prerequisite for
`TrivialScope`.

### Step 2 — `ScopeQuery`, `TrivialScope`, `ScopeDict`, `toScopeDict`

Add to `Pattern.Core`. No dependencies on other new definitions. Write unit tests
for `TrivialScope`: `containers` and `siblings` return `[]`; `byIdentity` finds
elements within the subtree; `allElements` enumerates the full subtree.

### Step 3 — `paraWithScope` in `Pattern.Core`

Implement `paraWithScope`. Redefine `para` in terms of it. Run the existing `para`
test suite — all tests must pass without modification. This is the primary correctness
check: if `para` is behaviorally identical before and after, the refactoring is sound.

### Step 4 — graph-wide scope reification in `Pattern.Graph.Transform`

Add the internal `GraphViewScope` adapter and exported `scopeDictFromGraphView` helper
to `Pattern.Graph.Transform`. Keep `GraphQuery(..)` unchanged, and preserve
`paraGraph`/`paraGraphFixed` as wrappers over graph-specific scheduling.
Run the existing `paraGraph` test suite — all tests must pass without modification.

### Step 5 — verify no regressions

Run the full `pattern-hs` test suite. No failures expected. No call sites should
require changes.

---

## What this does not include

This proposal deliberately excludes the following, which belong to later proposals:

- **`RepresentationMap`** — named invertible maps between kinds of shape. Depends on
  this proposal and on `GraphTransform`. See `representation-map-proposal.md`.
- **`PatternKind`** — named, predicate-defined shape kinds. Part of the
  `RepresentationMap` proposal.
- **Interface convention for `GraphClassifier`** — `GraphClassifier` can gain a
  typeclass instance following the same pattern as `GraphQuery`, but that change is
  deferred to avoid scope creep here.
- **`mapWithContext` and `filterGraph` redefinitions** — these can be redefined against
  `ScopeQuery q v` once `paraWithScope` is in place, but are a follow-on cleanup, not
  a prerequisite.
- **Rust port** — the Rust port of this proposal follows the standard
  typeclass-to-trait correspondence. That work belongs in `pattern-rs` and is not
  scheduled here.

---

## Acceptance criteria

The implementation is complete when:

1. `ScopeQuery`, `TrivialScope`, `ScopeDict`, `toScopeDict`, and `paraWithScope` are
   present in `Pattern.Core` and exported.
2. `scopeDictFromGraphView` is present in `Pattern.Graph.Transform` and exported.
3. `para` and `paraGraph` preserve their existing call sites and pass their existing
   test suites without modification.
4. The full `pattern-hs` test suite passes with no failures.
5. No existing call sites required changes.

---

## Related documents

- `representation-map-proposal.md` — the larger proposal this is extracted from;
  `RepresentationMap` depends on this proposal being implemented first
- `proposals/graph-query.md` — `GraphQuery` record used by `paraGraph`; generic
  graph-wide scope answers are reified from `GraphView` rather than added here
- `proposals/graph-transform.md` — the next proposal in sequence after this one is
  merged; `RepresentationMap` depends on both
