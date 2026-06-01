# RFC-002: Pattern as Container and Graph Substrate

**Status:** accepted
**Date:** 2025-11-01
**Authors:** @akollegger
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** [`proposals/design/DESIGN.md`](DESIGN.md), [`proposals/design/pattern-category.md`](pattern-category.md)
**Related modules:** `Pattern.Core`, `Pattern.Graph.GraphLens`, `Pattern.PatternGraph`

## Summary

`Pattern v` is a recursive decorated-sequence container: a value paired with an ordered
list of element Patterns. It is not itself a graph; graph structure is an *interpretation*
imposed by external machinery. The same `Pattern v` can be interpreted as a directed graph,
an undirected graph, a walk, or any other graph-like structure depending on which view is
applied. This separation of container from interpretation is the load-bearing design choice
that enables flexibility across graph semantics without committing to any single one.

## Motivation

Graph data structures traditionally encode semantic commitments in their types: a
`DiGraph` is directed, an `UndirectedGraph` is not, a `WeightedGraph` carries edge
weights. This forces users to choose an interpretation at construction time and makes
multiple simultaneous interpretations cumbersome.

`Pattern v` inverts this: a universal container accumulates structure, and views impose
interpretations on demand. The same Pattern that represents a person's professional
network can simultaneously represent their social graph, their org chart, or a sequence
of career transitions — without duplicating data. The interpretation that is salient
depends on the question being asked, not on how the data was originally stored.

This is the motivation for separating substrate from interpretation.

## Design

### The Pattern Data Structure

```haskell
data Pattern v = Pattern
  { value    :: v           -- decoration: what kind of pattern this is
  , elements :: [Pattern v] -- the pattern's content, as an ordered sequence
  }
```

Patterns are recursive: every element of a Pattern is itself a Pattern. The `elements`
field is the sequence that defines the pattern's content; the `value` field is a
decoration that describes what the sequence means.

**Key insight**: The elements field IS the pattern — it contains the sequence that defines
the content. The value field decorates that content. For example, a pattern `[chord | C, E, G]`
has value `chord` decorating the sequence `[C, E, G]`.

### Conceptual Model: Decorated Sequences

Conceptually, patterns are **decorated sequences**:
- `elements` holds the sequence that defines the pattern
- `value` provides decoration about what kind of pattern it is
- Order of elements is semantically significant — patterns are ordered
- Recursive structure enables nested sequences (patterns containing patterns)

The tree structure is an implementation detail that supports sequences in memory. There
is no contradiction between these views — the tree structure is how sequences are
represented, and tree traversal preserves sequence order.

### Structural Classifications

Patterns have structural classifications based on element count. These describe what
patterns *are* structurally, independent of any interpretation.

**Atomic Pattern** — `elements == []`. The fundamental building block; the leaf of any
Pattern tree. Nodes in the graph interpretation are atomic patterns.

**Singular Pattern** — exactly one element. A pattern wrapping a single sub-pattern.

**Pattern with Elements** — one or more elements. The general case.

**Nested Pattern** — elements that themselves have elements, enabling arbitrary depth.

### Graph Interpretations

Patterns are *interpreted* as graph elements through views. The same structural
classification rules apply across all interpretations.

#### Nodes

A node is an atomic pattern — a Pattern with no elements.

```haskell
node :: v -> Pattern v
node v = Pattern v []
```

Notation equivalences:
```
Cypher:  (n:Person)
Pattern: [n:Person]
```

#### Relationships

**Undirected**: a pattern with exactly two atomic elements; order is semantically irrelevant.
**Directed**: a pattern with exactly two atomic elements, where element order encodes direction —
`element[0]` is the source, `element[1]` is the target.

```haskell
-- Source/target accessors
source :: Pattern v -> Pattern v
source (Pattern _ (s:_)) = s

target :: Pattern v -> Pattern v
target (Pattern _ [_, t]) = t

-- Reverse direction
reverseRel :: Pattern v -> Pattern v
reverseRel (Pattern v [a, b]) = Pattern v [b, a]
```

Notation equivalences:
```
Cypher:  (a)-[r:KNOWS]->(b)    Pattern: [r:KNOWS | a, b]  -- directed, a→b
Cypher:  (a)<-[r:KNOWS]-(b)   Pattern: [r:KNOWS | b, a]  -- directed, b→a
Cypher:  (a)-[r:KNOWS]-(b)    Pattern: [r:KNOWS | a, b]  -- undirected (order irrelevant)
```

**Design rationale**: direction is encoded positionally in the existing ordered-sequence
structure, with no extra type machinery. Reversing a relationship is a structural
operation: swap the two elements. Directed vs. undirected is a semantic distinction
managed by the consumer, not a structural one.

#### Walks

A walk is an ordered sequence of relationships allowing continuous traversal: entering
each relationship at one endpoint and exiting at the other, which becomes the entry
point of the next relationship.

```haskell
walk :: v -> [Pattern v] -> Pattern v
walk meta relationships = Pattern meta relationships
```

```
Cypher: (a)-[r1]->(b)<-[r2]-(c)-[r3]->(d)
Pattern: [walk | [r1 | a, b], [r2 | c, b], [r3 | c, d]]
```

Valid walk compositions in Pattern notation:
```
[w | [r1 | a, b]]              =~ (a)-[r1]->(b)
[w | [r1 | a, b], [r2 | c, b]] =~ (a)-[r1]->(b)<-[r2]-(c)
[w | [r1 | a, b], [r2 | b, a]] =~ (a)-[r1]->(b)-[r2]->(a)
```

#### Summary

| Concept        | Pattern Structure            | Convention                          |
|----------------|------------------------------|-------------------------------------|
| Node           | `[v]`                        | Atomic pattern (no elements)        |
| Undirected Rel | `[r \| (a), (b)]`            | Order semantically irrelevant       |
| Directed Rel   | `[r \| (a), (b)]`            | element[0]=source, element[1]=target |
| Walk           | `[meta \| rel1, rel2, ...]`  | Consecutive rels share endpoints    |

### Typeclass Instances

`Pattern v` is implemented with the following typeclass instances (all accepted):

- `Functor` — structure-preserving map over values: `fmap f` transforms every `v` while
  preserving the Pattern tree structure
- `Foldable` — fold over all values in traversal order
- `Traversable` — effectful traversal, combining Functor and Foldable
- `Applicative` — zip-like application of a Pattern of functions to a Pattern of values
- `Comonad` — `extract` retrieves the root value; `extend` maps with local context;
  `duplicate` produces the Pattern of all sub-Patterns
- `Eq`, `Ord`, `Hashable` — structural equality, ordering, and hashing
- `Semigroup`, `Monoid` — Pattern combination with identity
- `Show`, `ToJSON`, `FromJSON` — display and serialization

### Category-Theoretic Perspective

`Pattern v` admits a categorical interpretation through a `CategoryLens`, which defines
what counts as objects and how values compose:

```haskell
data CategoryLens v = CategoryLens
  { valueOp :: v -> v -> v      -- value composition (magma operation)
  , isObject :: Pattern v -> Bool  -- which patterns are objects
  }
```

Under a graph lens, leaf patterns (atoms) are objects and arity-2 patterns are morphisms.
Objects and morphisms are identified *post-hoc* through predicates, not enforced at
construction time.

**Composition** preserves structure rather than flattening:

```haskell
compose :: (v -> v -> v) -> Pattern v -> Pattern v -> Pattern v
compose f (Pattern v1 elems1) (Pattern v2 elems2) =
  Pattern (f v1 v2) [Pattern v1 elems1, Pattern v2 elems2]
```

The original patterns become elements of the composite, maintaining internal structure.

**Morphism equivalence** — two patterns are equivalent as morphisms when they have the
same leaf sequence (same objects in sequence order). Three levels:

- *Structural equivalence*: same complete leaf sequence
- *Endpoint equivalence*: same source and target objects (hom-set membership)
- *Custom*: domain-specific equivalence relation

**The category is not intrinsic to `Pattern`**; it emerges from the chosen lens. The
same `Pattern v` can support many different categorical structures by varying the lens,
validity rules, or equivalence relation. This is the "schema-lazy" property: maximum
flexibility during construction, multiple valid interpretations on demand.

### Forgetful Functor Hierarchy

Views form a hierarchy of forgetful functors, each losing information as it descends:

```
Pat[Full] --F₁--> Pat[Shape] --F₂--> Pat[Topology] --F₃--> Pat[Connected]
```

```haskell
forgetValues :: Pattern v -> Pattern ()
forgetValues = fmap (const ())
```

This enables analogical reasoning: match patterns that are structurally similar under
some forgetting, even if their values differ.

### Key Properties

1. **Schema-lazy** — Patterns do not commit to specific graph semantics; interpretation
   happens in the view. The same Pattern is valid under any compatible view.
2. **Compositional** — Views can be composed, stacked, or swapped without changing
   underlying patterns.
3. **Open-ended** — New views can be defined for any graph-like interpretation.
4. **Categorical** — Each view defines a functor; forgetful matching uses functor
   composition.

## Open Questions

1. **Navigation functions** — `source`, `target`, `nodes`, `relationships` as explicit
   Pattern operations are planned but not implemented. They would provide ergonomic
   access to graph-structured patterns without requiring a full view.

2. **Zipper for focus** — A `Zipper v` data structure supporting `parents` /
   `ancestors` operations would enable DOM-style upward navigation and focus-based
   editing. Currently deferred; comonadic `extend` and `duplicate` partially address
   the use cases.

3. **Pattern morphisms** — Explicit `PatternMorphism v w` types with `homomorphism`
   and `forget` combinators are planned as future work for categorical reasoning.

4. **Standard view instances** — Named instances like `DirectedView` and `UndirectedView`
   are planned to reduce boilerplate for the common cases.

## Alternatives

**Typed graph representations** — encoding node/relationship/walk distinction in the
type system (e.g. GADTs indexed by sort) was rejected. `Pattern v`'s uniform-value
architecture is load-bearing for typeclass instances, and sort indexing would require
either a parallel type or a substantial rewrite. The runtime cost of kind predicates
is acceptable.

**Forcing interpretation at construction** — building a `Graph` type with committed
semantics at the constructor site was rejected in favor of the substrate posture.
Post-hoc interpretation via views is more flexible without being more complex.
