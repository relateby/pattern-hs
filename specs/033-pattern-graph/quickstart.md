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
import Pattern.PatternGraph (PatternGraph, empty, merge, mergeWithPolicy, fromPatterns, fromPatternsWithPolicy, toGraphLens, toGraphLensWithScope, MergeResult(..))
import Pattern.Reconcile (ReconciliationPolicy(..))  -- LastWriteWins, FirstWriteWins, etc.
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
let MergeResult graph unrecognized = fromPatterns patterns
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
-- toGraphLens returns Nothing for empty graph; Just lens for non-empty
let lens = toGraphLens graph  -- Maybe (GraphLens v)
let nodeList = maybe [] nodes lens
let relList = maybe [] relationships lens
-- Or with a known non-empty graph: let Just lens = toGraphLens graph
-- For empty graphs use toGraphLensWithScope scopeVal graph (total).
-- Use existing GraphLens operations: neighbors, bfs, findPath, etc.
```

### Round-trip with gram

Flow: **parse → fromPatterns → modify → serialize**. Parse a gram string with `Gram.Parse.fromGram`, load into a PatternGraph with `fromPatterns`, optionally modify (e.g. merge more patterns), then serialize by collecting graph contents and calling `Gram.Serialize.toGram`:

```haskell
-- Parse gram (libs/gram) → list of Pattern v
case fromGram gramText of
  Right parsedPatterns -> do
    let MergeResult pg unk = fromPatterns parsedPatterns
    -- Modify pg (e.g. merge more patterns)
    -- Serialize: flatten graph and use gram
    let flat = Map.elems (pgNodes pg) ++ Map.elems (pgRelationships pg)
          ++ Map.elems (pgWalks pg) ++ Map.elems (pgAnnotations pg)
    let serialized = toGram flat
    -- Re-parse with fromGram to verify round-trip
  Left _ -> -- handle parse error
```

An integration test that asserts the same logical graph after round-trip lives in `libs/gram/tests/Spec/Gram/RoundtripSpec.hs` (PatternGraph round-trip).

## Unrecognized patterns

Merge and fromPatterns never drop input. If a pattern does not classify as Node, Annotation, Relationship, or Walk, it is returned in the `unrecognized` list:

```haskell
let MergeResult g unk = fromPatterns [point "a", someWeirdPattern]
-- g contains the node "a"; someWeirdPattern is in unk
-- Caller can log, error, or handle unk as needed
```

## Documentation

- **Usage**: [docs/guide/pattern-graph-usage.md](../../docs/guide/pattern-graph-usage.md) — construction, merge, fromPatterns, round-trip, toGraphLens.
- **Reference**: [docs/reference/features/pattern-graph.md](../../docs/reference/features/pattern-graph.md).
- **`.graph.gram` notation**: [docs/reference/graph-gram-notation.md](../../docs/reference/graph-gram-notation.md) — annotations, nodes, relationships, paths only; resulting PatternGraph structures explained with pattern notation.
