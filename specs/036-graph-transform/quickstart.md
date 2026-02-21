# Quickstart: Graph Transform

This guide demonstrates how to apply `GraphView` to execute bulk-transformations and topologies.

## 1. Establishing a GraphView

A pipeline always starts by applying a `GraphView` wrapper against your data source. Assuming we have an existing `PatternGraph`:

```haskell
import Pattern.Graph
import Pattern.Graph.Transform

-- Convert existing graph into uniform view
let view = fromPatternGraph canonicalClassifier myGraph
```

## 2. Pipelined Transformations

Transformations are entirely lazy over `GraphView`. You chain functions directly and finalize them by invoking `materialize`.

```haskell
pipeline :: PatternGraph Subject -> PatternGraph Subject
pipeline graph =
  materialize canonicalClassifier LastWriteWins  -- Back to storage
  . mapWithContext canonicalClassifier enrich     -- Advanced mapped queries
  . filterGraph canonicalClassifier isRelevant dissolve -- Pruning bad connections
  . mapAllGraph updateTimestamp                   -- Quick blanket update
  . fromPatternGraph canonicalClassifier          -- Entry
  $ graph
```

## 3. Creating Graphs from Arbitrary Data Seeds

Using `unfoldGraph`, you can bypass manual edge generation and recursively compile raw external items (e.g., CSV rows) directly to unified Knowledge Graphs.

```haskell
rowToPatterns :: Row -> [Pattern Subject]
rowToPatterns row = [ personNode row, departmentNode row, worksInRel row ]

-- Auto-merges overlaps across all seeds
let etlGraph = unfoldGraph canonicalClassifier LastWriteWins rowToPatterns rows
```

## 4. Run Message-Passing Graph Algorithms

Use `paraGraphFixed` to run algorithms natively matching PageRank styles until numerical convergence.

```haskell
pageRankConverge :: Double -> Double -> Bool
pageRankConverge old new = abs (old - new) < 0.0001

results :: Map (Id Subject) Double
results = paraGraphFixed pageRankConverge algorithmStep 1.0 view
```