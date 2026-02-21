# Quickstart: Graph Classifier

The Graph Classifier unifies graph views for `pattern-hs`. It allows defining custom structural rules and organizing patterns without data loss.

## Standard Usage

Most users will use the built-in `canonicalClassifier`:

```haskell
import Pattern.Graph.GraphClassifier (canonicalClassifier)
import Pattern.PatternGraph (fromPatterns)

-- Construct a canonical PatternGraph
let graph = fromPatterns canonicalClassifier patterns

-- Access elements
let nodes = pgNodes graph
let rels  = pgRelationships graph
```

## Custom Classifiers

For domain-specific organization, define an `extra` type and a classifier:

```haskell
import Pattern.Graph.GraphClassifier
import Pattern.PatternGraph

data MyDomain = Hyperedge | Metadata deriving Show

myClassifier :: GraphClassifier MyDomain Subject
myClassifier = GraphClassifier
  { classify = \pat ->
      if isHyperedge pat then GOther Hyperedge
      else if isMetadata pat then GOther Metadata
      else -- fallback to arity logic for everything else
           case classifyByShape pat of
             GNode -> GNode
             GRelationship -> GRelationship
             GAnnotation -> GAnnotation
             GWalk -> GWalk
             GOther () -> GOther Metadata
  }

-- Construct the custom graph
let customGraph = fromPatterns myClassifier patterns

-- Query custom domain items from pgOther:
-- pgOther is a `Map (Id v) (MyDomain, Pattern v)`
let hyperedges = Map.filter (\(domain, _) -> domain == Hyperedge) (pgOther customGraph)
```

## Legacy GraphLens compatibility

If you just need an on-demand view from a single node predicate:

```haskell
import Pattern.Graph (GraphLens(..), mkGraphLens, neighbors)

-- Same as before, using mkGraphLens! Internally it creates a two-category GraphClassifier.
let lens = mkGraphLens scopePattern isAtomic

-- use algorithms
let nbrs = neighbors lens nodeA
```
