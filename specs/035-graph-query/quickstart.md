# Quickstart: GraphQuery

`GraphQuery v` is a portable, composable graph query interface. It decouples graph algorithms from any specific graph representation — the same algorithm works on a `PatternGraph`, a `GraphLens`, a database-backed graph, or any custom source.

---

## Standard Usage: Algorithms on a PatternGraph

```haskell
import Pattern.Graph.GraphClassifier (canonicalClassifier)
import Pattern.Graph.GraphQuery      (fromPatternGraph, undirected)
import Pattern.Graph.Algorithms      (shortestPath, connectedComponents)
import Pattern.PatternGraph          (fromPatterns)

-- Build a PatternGraph from patterns
let pg = fromPatterns canonicalClassifier myPatterns

-- Construct a GraphQuery directly from the PatternGraph (no GraphLens needed)
let gq = fromPatternGraph pg

-- Run algorithms
let path       = shortestPath gq undirected nodeA nodeB  -- Maybe [Pattern v]
let components = connectedComponents gq undirected        -- [[Pattern v]]
```

---

## Traversal Direction at the Call Site

The same graph, queried with different traversal policies:

```haskell
import Pattern.Graph.GraphQuery (fromPatternGraph, directed, undirected, directedReverse)
import Pattern.Graph.Algorithms (hasPath)

let gq = fromPatternGraph pg

-- Directed: only follows relationships source → target
hasPath gq directed nodeA nodeB   -- True if A→B path exists

-- Undirected: ignores direction, treats all relationships as bidirectional
hasPath gq undirected nodeA nodeB -- True if any path exists

-- Reverse: only follows relationships target → source
hasPath gq directedReverse nodeB nodeA
```

---

## Custom Traversal Weight

Read edge weights from relationship properties:

```haskell
import Pattern.Graph.GraphQuery (TraversalWeight, TraversalDirection(..), fromPatternGraph)
import Pattern.Graph.Algorithms (shortestPath)
import Subject.Core             (Symbol)

-- A weight function that reads a "cost" property from the relationship value
weightedTraversal :: TraversalWeight Subject
weightedTraversal rel Forward  = maybe 1.0 id (lookupCost rel)
weightedTraversal _   Backward = 1/0  -- block reverse traversal

let gq   = fromPatternGraph pg
let path = shortestPath gq weightedTraversal nodeA nodeB
```

---

## Composing Graph Views

### Subgraph frames (filter by predicate)

```haskell
import Pattern.Graph.GraphQuery (fromPatternGraph, frameQuery, undirected)
import Pattern.Graph.Algorithms (connectedComponents)

-- Only include nodes and relationships with a "Person" label
let personFrame = frameQuery isPerson (fromPatternGraph pg)

-- Algorithms operate only within the frame
let components = connectedComponents personFrame undirected
```

### Memoizing incident relationship lookups

```haskell
import Pattern.Graph.GraphQuery (fromPatternGraph, memoizeIncidentRels, undirected)
import Pattern.Graph.Algorithms (betweennessCentrality)

-- Wrap with memoization before running a centrality algorithm
let gq = memoizeIncidentRels (fromPatternGraph pg)
let centrality = betweennessCentrality gq undirected  -- Map (Id v) Double
```

### Composing combinators

```haskell
-- Frame + memoize in one expression
let gq = memoizeIncidentRels . frameQuery isPerson $ fromPatternGraph pg
```

---

## Context Queries

Ask "what contains this element?" without precomputing a context record:

```haskell
import Pattern.Graph.GraphClassifier (canonicalClassifier)
import Pattern.Graph.GraphQuery      (fromPatternGraph)
import Pattern.Graph.Algorithms      (queryAnnotationsOf, queryWalksContaining, queryCoMembers)

let gq = fromPatternGraph pg

-- All annotations attached to a node
let annotations = queryAnnotationsOf canonicalClassifier gq myNode

-- All walks that contain a relationship
let walks = queryWalksContaining canonicalClassifier gq myRel

-- All elements sharing a container with a given element
let coMembers = queryCoMembers gq myNode myContainer
```

---

## Algorithms on a GraphLens (existing code, unchanged)

Existing `GraphLens`-based calls continue to work without modification:

```haskell
import Pattern.Graph (bfs, findPath, connectedComponents)

-- These are now wrappers over Pattern.Graph.Algorithms with undirected default
let reachable  = bfs lens startNode
let path       = findPath lens nodeA nodeB
let components = connectedComponents lens
```

---

## Implementing a Custom GraphQuery

Any value that can provide the required functions produces a `GraphQuery v`. No inheritance or typeclass instance required:

```haskell
import Pattern.Graph.GraphQuery (GraphQuery(..))

-- A hand-constructed query over a custom data source
myQuery :: GraphQuery MyValue
myQuery = GraphQuery
  { queryNodes            = fetchNodes myDb
  , queryRelationships    = fetchRelationships myDb
  , queryIncidentRels     = \n -> fetchIncident myDb (nodeId n)
  , querySource           = \r -> Just (fetchSource myDb r)
  , queryTarget           = \r -> Just (fetchTarget myDb r)
  , queryDegree           = \n -> fetchDegree myDb (nodeId n)
  , queryNodeById         = \i -> fetchNodeById myDb i
  , queryRelationshipById = \i -> fetchRelById myDb i
  , queryContainers       = \p -> fetchContainers myDb p
  }
```

All algorithms in `Pattern.Graph.Algorithms` work against this query without modification.
