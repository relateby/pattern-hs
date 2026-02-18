# PatternGraph Usage Guide

**Status**: Implemented (Feature 33)  
**Location**: `libs/pattern/src/Pattern/PatternGraph.hs`

## Overview

PatternGraph is a container for graph data backed by `Pattern v`. It stores **nodes**, **relationships**, **walks**, and **annotations** keyed by identity, with merge-on-insert semantics. Use it for the round-trip workflow: parse a gram file → load into a PatternGraph → query or modify → serialize back.

## Construction

### Empty graph

```haskell
import Pattern.PatternGraph (empty)

g = empty  -- all four maps (nodes, relationships, walks, annotations) are empty
```

### From a list of patterns: `fromPatterns`

```haskell
import Pattern.PatternGraph (fromPatterns, MergeResult(..))

patterns = [ point "a", point "b", pattern "r1" [point "a", point "b"] ]
MergeResult graph unrecognized = fromPatterns patterns
-- graph has 2 nodes and 1 relationship; unrecognized is [] when all are recognized
```

### Merge one pattern at a time: `merge`

```haskell
import Pattern.PatternGraph (empty, merge, MergeResult(..))

let MergeResult g1 _ = merge (point "a") empty
let MergeResult g2 _ = merge (point "b") g1
let MergeResult g3 _ = merge (pattern "r" [point "a", point "b"]) g2
```

## Merge and reconciliation

- **Default policy**: `merge` and `fromPatterns` use **LastWriteWins** for duplicate identities.
- **Explicit policy**: Use `mergeWithPolicy` and `fromPatternsWithPolicy` with `ReconciliationPolicy` from `Pattern.Reconcile` (e.g. `LastWriteWins`, `FirstWriteWins`, `Merge ...`).

When the same identity is merged twice, the reconciliation policy determines the stored value.

## Unrecognized patterns

Merge and fromPatterns **never drop** input. If a pattern does not classify as Node, Annotation, Relationship, or Walk, it is returned in `MergeResult.unrecognized` and **not** stored in the graph. Check or log `unrecognized` as needed.

## Round-trip with gram

1. Parse: `fromGram gramText` → `[Pattern Subject]`
2. Load: `fromPatterns parsed` → `MergeResult graph unrecognized`
3. Modify: e.g. `merge` more patterns into `graph`
4. Serialize: flatten `pgNodes`, `pgRelationships`, `pgWalks`, `pgAnnotations` and call `toGram`. See `specs/033-pattern-graph/quickstart.md` and the integration test in `libs/gram/tests/Spec/Gram/RoundtripSpec.hs`.

## Conversion to GraphLens: `toGraphLens`

Convert a PatternGraph to the existing GraphLens view so you can use graph algorithms (nodes, relationships, walks, neighbors, bfs, findPath, etc.):

```haskell
import Pattern.PatternGraph (fromPatterns, toGraphLens)
import Pattern.Graph (nodes, relationships)

MergeResult graph _ = fromPatterns [ point "a", point "b", pattern "r" [point "a", point "b"] ]
lens = toGraphLens graph
nodeList = nodes lens
relList = relationships lens
```

## See also

- **Data model**: `specs/033-pattern-graph/data-model.md`
- **Quick start**: `specs/033-pattern-graph/quickstart.md`
- **Reference**: `docs/reference/features/pattern-graph.md`
- **.graph.gram notation**: `docs/reference/graph-gram-notation.md`
