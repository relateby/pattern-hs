# Proposal: PatternGraph for pattern-hs

**Status**: 📝 Design Only  
**Date**: 2026-02-18

## Summary

Add a `PatternGraph` data structure backed by `Pattern v`, providing a concrete, opinionated
graph representation for the common use case of working with atomic nodes, relationships,
walks, and annotations parsed from gram notation. This complements the existing `GraphLens`
approach, which remains the right abstraction for interpreting arbitrary `Pattern` structures
as graphs.

## Motivation

The `GraphLens` design is correct for the general case, but it is an interpretive layer — a
view over a `Pattern`, not a container. Many users have a simpler, immediate need: parse a
gram file, query and modify its graph structure, then write it back out. For this workflow,
a concrete owning data structure with clear semantics is more ergonomic than constructing
and managing a lens.

The immediate use case is a round-trip workflow:

```
gram file → parse → PatternGraph → query/modify → serialize → gram file
```

`PatternGraph` targets gram files that use nodes, relationships, walks, and annotations —
the full expressive range of gram path notation — and provides a natural home for that data
with merge-on-insert semantics familiar from Cypher's `MERGE` clause.

## Design

### Type

`PatternGraph` is generic over its value type, like `Pattern` itself:

```haskell
data PatternGraph v = PatternGraph
  { pgNodes         :: Map (Id v) (Pattern v)
  , pgRelationships :: Map (Id v) (Pattern v)
  , pgWalks         :: Map (Id v) (Pattern v)
  , pgAnnotations   :: Map (Id v) (Pattern v)
  }
```

The type parameter `v` must satisfy a `GraphValue` constraint that provides identity
extraction and pattern classification:

```haskell
class Ord (Id v) => GraphValue v where
  type Id v
  identify  :: v -> Id v
  classify  :: Pattern v -> PatternClass
```

`Subject` is the primary intended `v`, where `Id Subject = Symbol`.

### Pattern Classification

Patterns are classified by arity, in arity order:

```haskell
data PatternClass
  = Node          -- 0 elements
  | Annotation    -- 1 element
  | Relationship  -- 2 elements (both elements are nodes)
  | Walk          -- n elements (all elements are relationships)
  | Unrecognized  -- anything else
```

Classification is the responsibility of the `GraphValue` instance, keeping the discrimination
logic in one place.

### Smart Construction via `merge`

The primary operation for adding data to a `PatternGraph` is `merge`, named deliberately
after Cypher's `MERGE` clause:

```haskell
merge :: GraphValue v => Pattern v -> PatternGraph v -> PatternGraph v
```

`merge` classifies the incoming `Pattern v`, dispatches it to the appropriate collection,
and reconciles any existing entry at the same identity using the existing
`ReconciliationPolicy` machinery from `Pattern.Reconcile`. It also decomposes recursively:
merging a walk merges its constituent relationships, and merging a relationship merges its
endpoint nodes. The graph is always internally consistent.

Constructing a graph from a list of parsed patterns is then a fold:

```haskell
fromPatterns :: GraphValue v => [Pattern v] -> PatternGraph v
fromPatterns = foldr merge empty
```

### Annotations

Annotations (`@@id:Label @key("value")`) are stored as first-class entries in `pgAnnotations`
with their own identity, labels, and properties. Their single inner element is also
recursively merged into its appropriate collection. This preserves the entity-component-system
character of annotations: multiple independent systems can annotate the same node without
their data colliding in the node's own `Subject`.

### Walks

Merging a walk stores it whole in `pgWalks` *and* recursively merges its component
relationships and nodes. The walk is independently addressable by identity, while its
components remain fully accessible through `pgNodes` and `pgRelationships`.

### Unrecognized Patterns

Patterns that do not classify as Node, Annotation, Relationship, or Walk are not silently
dropped. `merge` signals their presence, leaving the caller to decide — either skip, error,
or handle specially. The recommended approach is for `merge` to return an `Either` or
accumulate warnings via a `Writer`-style layer, rather than having a catch-all `pgOther`
collection that obscures unexpected input.

### Relationship to GraphLens

`PatternGraph` and `GraphLens` are complementary, not competing. A `PatternGraph` can
always be converted to a `GraphLens` by constructing a scope pattern from its contents and
providing the atomic predicate — giving access to the richer graph algorithms already built
on `GraphLens`. `PatternGraph` is the concrete, owning, modifiable container; `GraphLens`
is the interpretive, read-only view.

## Open Questions

1. Should `merge` be total (returning a result plus warnings) or partial (returning
   `Either` on `Unrecognized`)? A `Writer`-based accumulation may give the best of both.

2. Should `PatternGraph` expose a `Monoid` instance via `overlay` (combining two graphs)
   to complement the element-wise `merge`? This would align further with the
   `algebraic-graphs` style and make combining subgraphs natural.

3. The `GraphValue` typeclass bundles identity and classification. Should `classify` be
   separate — e.g., a standalone function or a second typeclass — to allow the
   discrimination logic to be overridden independently of the identity scheme?

## Summary

`PatternGraph` gives users a concrete, ergonomic data structure for the gram round-trip
workflow, with merge semantics that match Cypher intuitions, recursive decomposition that
keeps the graph consistent, and a clean path to `GraphLens` for advanced queries. It does
not replace `GraphLens` — it makes the common case easy while leaving the general case
intact.
