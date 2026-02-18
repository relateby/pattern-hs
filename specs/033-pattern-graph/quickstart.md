# Quick Start: PatternGraph

**Feature**: 033-pattern-graph  
**Date**: 2026-02-18  
**Phase**: 1 – Design

## Overview

PatternGraph is a container for graph data backed by `Pattern v`. It stores nodes, relationships, walks, and annotations keyed by identity, with merge-on-insert semantics. Use it for the round-trip workflow: parse a gram file → load into a PatternGraph → query or modify → serialize back. Unrecognized patterns are reported, not dropped. You can convert a PatternGraph to a GraphLens to use existing graph algorithms.

## Prerequisites

- `Pattern.Core` (Pattern, pattern, point)
- `Pattern.Reconcile` (ReconciliationPolicy, e.g. LastWriteWins)
- A value type with a `GraphValue` instance (e.g. Subject, with `Id Subject = Symbol`)

## Basic usage

### Imports

```haskell
import Pattern.Core (Pattern(..), pattern, point)
import Pattern.PatternGraph (PatternGraph, empty, merge, mergeWithPolicy, fromPatterns, fromPatternsWithPolicy, toGraphLens, MergeResult(..))
import Pattern.Reconcile (LastWriteWins)
import Pattern.Graph (GraphLens, nodes, relationships)
```

### Empty graph and merge

```haskell
-- Start with an empty graph
let g0 = empty

-- Merge one node
let (MergeResult g1 _) = merge (point "a:Person") g0

-- Merge another node and a relationship
let rel = pattern "r:KNOWS" [point "a:Person", point "b:Person"]
let (MergeResult g2 _) = merge (point "b:Person") g1
let (MergeResult g3 unk) = merge rel g2
-- unk should be [] since relationship is recognized
```

### Build from a list of patterns

```haskell
let patterns =
      [ point "a"
      , point "b"
      , pattern "r1" [point "a", point "b"]
      ]
let (MergeResult graph, unrecognized) = fromPatterns patterns
-- All three are recognized; graph has 2 nodes and 1 relationship
-- If any pattern is not Node/Annotation/Relationship/Walk, it appears in unrecognized
```

### Using a reconciliation policy

```haskell
import Pattern.Reconcile (ReconciliationPolicy, LastWriteWins, Merge, defaultSubjectMergeStrategy, UnionElements)

let (MergeResult g, _) = mergeWithPolicy LastWriteWins (point "n:User") empty
-- When the same identity is merged again, LastWriteWins determines the stored value
```

### Convert to GraphLens

```haskell
let lens = toGraphLens graph
let nodeList = nodes lens
let relList = relationships lens
-- Use existing GraphLens operations: neighbors, bfs, findPath, etc.
```

### Round-trip with gram

```haskell
-- Parse gram (libs/gram) → list of Pattern v
-- Load into graph
(MergeResult pg, unk) = fromPatterns parsedPatterns
-- Modify pg (e.g. merge more patterns)
-- Serialize: build a pattern from pg contents and use gram serialize
-- (Exact serialize API depends on gram library; see docs when implemented.)
```

## Unrecognized patterns

Merge and fromPatterns never drop input. If a pattern does not classify as Node, Annotation, Relationship, or Walk, it is returned in the `unrecognized` list:

```haskell
let (MergeResult g, unk) = fromPatterns [point "a", someWeirdPattern]
-- g contains the node "a"; someWeirdPattern is in unk
-- Caller can log, error, or handle unk as needed
```

## Documentation (planned)

- **Usage**: Guide and reference in `docs/` for PatternGraph (construction, merge, fromPatterns, round-trip, conversion to GraphLens).
- **`.graph.gram` reference**: A reference for writing `.graph.gram` files that use only annotations, nodes, relationships, and paths (no square-bracket pattern notation). It will describe the resulting data structures in PatternGraph and may use pattern notation to explain them: *gram → parse → PatternGraph → explained with pattern notation*.

## Next steps

- Implement `Pattern.PatternGraph` and `GraphValue` (see [data-model.md](./data-model.md) and [contracts/](./contracts/)).
- Add tests (unit and property) for merge, fromPatterns, classification, and toGraphLens.
- Add or update `docs/` for PatternGraph usage and the `.graph.gram` notation reference.
