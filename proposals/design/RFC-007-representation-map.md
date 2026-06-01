# RFC-007: RepresentationMap — Invertible Shape Isomorphisms

**Status:** draft
**Date:** 2026-03-17
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** [`proposals/representation-map-proposal.md`](../representation-map-proposal.md)
**Depends on:** RFC-006 (ScopeQuery / paraWithScope), RFC-008 (GraphTransform, for `GraphTransform` primitives)
**Followed by:** Rust port (`pattern-rs`) → CLI surface (`pato canonicalize`)
**Related modules:** `Pattern.Core`, `Pattern.RepresentationMap`, `Pattern.Graph.Transform`

## Summary

Introduce `RepresentationMap` as a named, invertible, composable isomorphism between two
*kinds of shape* — named, recognizable shapes of `Pattern v` — where both kinds are
subtypes of `Pattern v` and the isomorphism is witnessed by a machine-checkable
round-trip property. Graph form is one common codomain, but any two kinds of shape can be
related by a `RepresentationMap`.

## Motivation

### The Two-Register Problem

The same information can be held in a form optimised for reading (nested pattern structure)
or a form optimised for querying (flat graph topology):

- **Pattern form** — nesting makes local structure immediately visible; optimised for
  authoring, reading, and sequential processing by humans and LLMs top-to-bottom.
- **Graph form** — flat topology makes global structure immediately traversable; optimised
  for querying, graph algorithms, and integration with graph-native tools.

Without explicit machinery, converting between forms requires either ad-hoc one-off
transformations (hard to invert, hard to compose) or committing to one form forever.

`RepresentationMap` makes the relationship explicit: it names the two kinds of shape,
declares the structural conventions that make round-tripping work, and provides a
machine-checkable witness to correctness.

### Use Cases

- Nested diagnostic pattern ↔ flat node-relationship diagnostic graph
- Positional sequence encoding ↔ labeled property encoding
- Hierarchy-as-nesting ↔ hierarchy-as-membership-edges
- Compact scalar representation ↔ expanded structural representation

## Design

### Foundational Principle: Scope is an Element

**Scope is not a query interface. Scope is an element. An element defines the scope of
everything it contains.**

A pattern element is a scope for its direct elements and for everything transitively
nested within it. Query interfaces are not *the scope* — they are efficient accessors
into a scope defined by a containing element. The caller decides what the scope
container is and constructs the appropriate query from it.

Both `para` and `paraGraph` are instances of the same general operation: a fold where
the folding function has access to child results *and* to the query interface for the
broader containing scope. See RFC-006 for the unification.

### Kinds of Shape

A **kind of shape** is a named, recognizable shape of `Pattern v` — a description of
the structural constraints that a pattern must satisfy to be an instance of that kind.
It is a subtype of `Pattern v` defined by a predicate.

```haskell
data PatternKind v = PatternKind
  { kindName    :: Text
    -- ^ e.g. "DiagnosticPattern", "DiagnosticGraph", "GNode"
  , kindPred    :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
    -- ^ true iff a pattern is of this kind within a given scope
  , kindExample :: Pattern v
    -- ^ canonical example for property-based test generation
  }
```

`kindPred` is polymorphic over scope queries: a shape constraint may be purely
structural (check the pattern itself) or scope-relative (check cross-references within
the containing scope). Both cases use the same predicate signature.

The graph classification kinds from RFC-004 (`GNode`, `GRelationship`, `GWalk`,
`GAnnotation`) are kinds of shape in exactly this sense.

### `RepresentationMap`

```haskell
data RepresentationMap v = RepresentationMap
  { name        :: Text
  , domain      :: PatternKind v
  , codomain    :: PatternKind v
  , conventions :: [Text]
    -- ^ named structural decisions that make the round-trip work
  , forward     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
  , inverse     :: forall q. ScopeQuery q v => q v -> Pattern v -> Pattern v
  , roundTrip   :: forall q. ScopeQuery q v => q v -> Pattern v -> Bool
    -- ^ witness: (inverse q . forward q) p == p for all p of kind domain
  }
```

`forward` and `inverse` are polymorphic over scope queries. A transform that needs graph
topology additionally constrains its scope:

```haskell
-- A map whose source is graph-shaped needs GraphScope (from RFC-006 follow-on)
graphAwareForward :: GraphScope q v => q v -> Pattern v -> Pattern v
```

### Conventions

Invertibility is not free. When a kind of shape does not carry all the information
needed to reconstruct the source from the target, the forward transform must encode
structural metadata into the target. These encoding decisions are **conventions** and
are part of the map's identity. An undocumented convention is a convention that will
be misread. Conventions are named text; long-term they may be expressed as gram
annotations on the codomain's schema.

### Composability

Two `RepresentationMap`s compose when the codomain of the first matches the domain of
the second:

```haskell
compose :: RepresentationMap v -> RepresentationMap v -> RepresentationMap v
compose m1 m2 = RepresentationMap
  { name        = name m1 <> " >>> " <> name m2
  , domain      = domain m1
  , codomain    = codomain m2
  , conventions = conventions m1 <> conventions m2
  , forward     = \q p -> forward m2 q (forward m1 q p)
  , inverse     = \q p -> inverse m1 q (inverse m2 q p)
  , roundTrip   = \q p -> roundTrip m1 q p && roundTrip m2 q p
  }
```

### Concrete Example: Diagnostic Map

**Kinds:**
- `DiagnosticPattern` — a location pattern directly containing a diagnostic pattern
  directly containing zero or more remediation patterns; labels `Location`, `Diagnostic`,
  `Remediation`; specific required properties per label.
- `DiagnosticGraph` — flat atomic patterns with labels `Location`, `Diagnostic`,
  `Remediation`; connected by `AT` and `HAS_REMEDIATION` relationships; `_arity` and
  `_depth` properties present on each.

**Conventions:**
- `_arity` on each graph-form node encodes the element count of the corresponding
  nested pattern node, enabling the inverse to reconstruct arity.
- `_depth` encodes nesting depth, enabling the inverse to reconstruct order.

**Forward** (`DiagnosticPattern → DiagnosticGraph`): traverse the nested structure using
`paraWithScope`, emit flat patterns with `_arity`/`_depth` properties, connect with typed
relationships.

**Inverse** (`DiagnosticGraph → DiagnosticPattern`): find all `Diagnostic` patterns via
`allElements`, traverse edges using `containers`, reconstruct nesting from `_arity`/`_depth`.

**Round-trip property**: for all `p` satisfying `kindPred DiagnosticPattern q p`,
`(inverse q . forward q) p == p` structurally.

### Relationship to GraphTransform

`RepresentationMap` sits above `GraphTransform` (RFC-008) in the abstraction stack.
The primitive operations from `GraphTransform` (`mapGraph`, `unfoldGraph`, `foldGraph`)
and `Pattern.Core` (`paraWithScope`, `unfold`, `map`) are how you *build* the `forward`
and `inverse` functions. `RepresentationMap` is how you *declare* that they are each
other's inverse, name the kinds they operate between, and record the conventions.

### Relationship to Pattern Equivalence

Pattern equivalence (RFC draft) handles two gram expressions that are syntactically
different but structurally identical — the same `Pattern Subject`, the same kind of
shape. `RepresentationMap` handles two patterns of *different* kinds of shape that are
informationally equivalent under declared conventions.

### Implementation Sequence

**Step 1 — Haskell prototype** (depends on RFC-006 and RFC-008)

1. Define `PatternKind v` in `Pattern.Core`
2. Define `RepresentationMap v` with `compose` in `Pattern.RepresentationMap`
3. Implement `diagnosticMap` with declared kinds and conventions
4. Write Hedgehog property tests: for all `p` satisfying `kindPred (domain m) q p`,
   `roundTrip q m p` holds and `kindPred (codomain m) q (forward q p)` holds
5. Write composition tests: `roundTrip` holds for composed maps

**Step 2 — Rust port** (`pattern-rs`)

```rust
pub struct PatternKind<V: GraphValue> {
    pub name:    String,
    pub pred:    Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> bool>,
    pub example: Pattern<V>,
}

pub struct RepresentationMap<V: GraphValue> {
    pub name:        String,
    pub domain:      PatternKind<V>,
    pub codomain:    PatternKind<V>,
    pub conventions: Vec<String>,
    pub forward:     Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> Pattern<V>>,
    pub inverse:     Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> Pattern<V>>,
    pub round_trip:  Box<dyn Fn(&dyn ScopeQuery<V>, &Pattern<V>) -> bool>,
}
```

**Step 3 — CLI surface** (`pato canonicalize`)

```
pato canonicalize [--map <n>] [--inverse] <files>...
```

Default map inferred from the document `kind` header. Scope constructed from the
document being transformed.

## Open Questions

1. **Kind compatibility in `compose`** — `compose m1 m2` requires `codomain m1` and
   `domain m2` to be compatible. Prototype uses a runtime check
   (`kindName (codomain m1) == kindName (domain m2)`). Phantom type tags would make
   it compile-time. Defer until the abstraction is stable.

2. **RepresentationMap as a pattern** — Can a `RepresentationMap` itself be represented
   as a `Pattern v`? If so, it could be serialized to gram, stored, and composed at
   the data level. The `forward` and `inverse` *functions* cannot be serialized directly,
   but the *description* (kinds, conventions, structural rules) may be pattern-representable.
   Park until at least two concrete maps are implemented and their structure is well understood.

## Alternatives

**Ad-hoc one-off transformations** — the status quo before this RFC. Rejected because
they accumulate technical debt: each transformation is a bespoke function with an
implicit (undocumented) inverse, making them difficult to compose, test systematically,
or port to other language targets.

**Type-class-based shape constraints** — encoding kinds as type-level predicates. Rejected
because kinds need to be computable at runtime (e.g. document-level schema validation
via `pato validate`), and because the predicate-based approach aligns with the existing
`GraphClassifier` design.
