-- | Graph Lens: Interpretive view of Pattern structures as graphs.
--
-- This module provides the Graph Lens feature, which enables interpreting
-- Pattern structures as graph structures (nodes, relationships, walks) through
-- a minimal, elegant design based on a single predicate.
--
-- == Overview
--
-- A Graph Lens provides an interpretive view of a Pattern as a graph structure.
-- Rather than defining graph concepts (nodes, relationships, walks) as intrinsic
-- properties of Pattern, they emerge through the lens's interpretation. This
-- design enables multiple graph views of the same Pattern and supports
-- higher-order graphs where relationships or entire graphs become nodes.
--
-- == Core Design
--
-- The Graph Lens consists of:
--
-- * @scopePattern@: The Pattern that defines the boundary for all graph operations.
--   Only direct elements of this pattern are considered for graph structure.
-- * @testNode@: A predicate determining which direct elements are nodes.
--   All other graph concepts (relationships, walks) derive from this single predicate.
--
-- == Design Principles
--
-- 1. **Scope-bounded operations**: All graph operations only consider direct elements
--    of @scopePattern@, never descending into nested structures.
--
-- 2. **Single predicate foundation**: Only @testNode@ is required. All other graph
--    predicates (relationships, walks, etc.) are derived from this.
--
-- 3. **Context captured at construction**: If a predicate needs context, that context
--    must be captured when the predicate is created, not during evaluation.
--
-- 4. **Interpretation, not intrinsic**: Graph structure is not a property of Pattern
--    itself, but an interpretation through the lens.
--
-- == Categorical Interpretation
--
-- Graph Lens provides a functorial interpretation where Pattern structures are
-- transformed into graph interpretations. The transformation Pattern → Graph
-- interpretation is functorial in nature.
--
-- == Example
--
-- >>> let graphPattern = pattern "graph" [point "a", point "b", pattern "r1" [point "a", point "b"]]
-- >>> let isAtomic (Pattern _ els) = null els
-- >>> let atomicLens = GraphLens graphPattern isAtomic
-- >>> nodes atomicLens
-- [Pattern "a" [],Pattern "b" []]
--
-- See @design/graph-lens.md@ and @specs/023-graph-lens/quickstart.md@ for
-- comprehensive examples and usage patterns.
module Pattern.Graph
  ( -- * Graph Lens Type
    GraphLens(..)
    -- * Node Operations
  , nodes
  , isNode
    -- * Relationship Operations
  , isRelationship
  , relationships
  , source
  , target
  , reverseRel
    -- * Walk Operations
  , isWalk
  , walks
  , walkNodes
    -- * Navigation Operations
  , neighbors
  , incidentRels
  , degree
    -- * Graph Analysis Operations
  , connectedComponents  -- Requires Ord v
  , bfs                   -- Requires Ord v
  , findPath              -- Requires Ord v
  ) where

import Pattern.Core (Pattern(..))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Map ()

-- | A Graph Lens provides an interpretive view of a Pattern as a graph structure.
-- 
-- The lens consists of:
-- * @scopePattern@: The Pattern that defines the boundary for all graph operations.
--   Only direct elements of this pattern are considered for graph structure.
-- * @testNode@: A predicate determining which direct elements are nodes.
--   All other graph concepts (relationships, walks) derive from this predicate.
--
-- == Categorical Interpretation
--
-- Graph Lens provides a functorial interpretation where Pattern structures are
-- transformed into graph interpretations. The transformation Pattern → Graph
-- interpretation is functorial in nature.
--
-- == Design Principles
--
-- 1. Scope-bounded: All operations only consider direct elements of scopePattern
-- 2. Single predicate foundation: Only testNode is required, all else derives
-- 3. Context at construction: Predicate context captured when lens is created
-- 4. Interpretation, not intrinsic: Graph structure is an interpretation, not
--    a property of Pattern itself
--
-- == Example
--
-- >>> let atomicLens = GraphLens pattern (\(Pattern _ els) -> null els)
-- >>> nodes atomicLens
-- [[a], [b], [c]]
data GraphLens v = GraphLens
  { scopePattern :: Pattern v
    -- ^ The Pattern that defines the graph scope
  , testNode     :: Pattern v -> Bool
    -- ^ Predicate determining which elements are nodes
  }

-- | Extract all nodes from the graph lens.
--
-- Nodes are direct elements of scopePattern that satisfy the testNode predicate.
--
-- == Time Complexity
-- O(n) where n is the number of direct elements in scopePattern
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> nodes lens
-- [[a], [b], [c]]
nodes :: GraphLens v -> [Pattern v]
nodes lens@(GraphLens (Pattern _ elems) _) = 
  filter (isNode lens) elems

-- | Determine if a Pattern is a node according to the lens.
--
-- This is the context-aware version that uses the lens's testNode predicate.
-- The lens parameter provides the predicate context.
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> isNode lens (point "a")
-- True
-- >>> isNode lens (pattern "rel" [point "a", point "b"])
-- False
isNode :: GraphLens v -> Pattern v -> Bool
isNode (GraphLens _ testNodePred) p = testNodePred p

-- * Relationship Operations

-- | Determine if a Pattern is a relationship according to the lens.
--
-- A relationship is a non-node pattern with exactly two node elements.
--
-- == Properties
-- * Must not be a node (does not satisfy testNode predicate)
-- * Must have exactly two elements
-- * Both elements must be nodes (according to the lens)
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let rel = pattern "knows" [point "Alice", point "Bob"]
-- >>> isRelationship lens rel
-- True
isRelationship :: GraphLens v -> Pattern v -> Bool
isRelationship lens@(GraphLens _ _) p@(Pattern _ els) =
  not (isNode lens p) &&
  length els == 2 &&
  all (isNode lens) els

-- | Extract all relationships from the graph lens.
--
-- Relationships are non-node patterns with exactly two node elements.
--
-- == Time Complexity
-- O(n) where n is the number of direct elements in scopePattern
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> relationships lens
-- [[knows | [Alice], [Bob]], [likes | [Bob], [Charlie]]]
relationships :: GraphLens v -> [Pattern v]
relationships lens@(GraphLens (Pattern _ elems) _) =
  filter (isRelationship lens) elems

-- | Extract the source node from a relationship.
--
-- For directed relationships, the source is the first element.
-- Returns Nothing if the pattern is not a relationship.
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let rel = pattern "knows" [point "Alice", point "Bob"]
-- >>> source lens rel
-- Just (point "Alice")
source :: GraphLens v -> Pattern v -> Maybe (Pattern v)
source lens p@(Pattern _ (s:_))
  | isRelationship lens p = Just s
  | otherwise = Nothing
source _ _ = Nothing

-- | Extract the target node from a relationship.
--
-- For directed relationships, the target is the second element.
-- Returns Nothing if the pattern is not a relationship.
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let rel = pattern "knows" [point "Alice", point "Bob"]
-- >>> target lens rel
-- Just (point "Bob")
target :: GraphLens v -> Pattern v -> Maybe (Pattern v)
target lens p@(Pattern _ [_, t])
  | isRelationship lens p = Just t
  | otherwise = Nothing
target _ _ = Nothing

-- | Reverse the direction of a relationship pattern.
--
-- Swaps the first and second elements, effectively reversing the
-- relationship direction.
--
-- == Example
--
-- >>> let rel = pattern "knows" [point "Alice", point "Bob"]
-- >>> reverseRel rel
-- pattern "knows" [point "Bob", point "Alice"]
reverseRel :: Pattern v -> Pattern v
reverseRel (Pattern v [a, b]) = Pattern v [b, a]
reverseRel p = p  -- Return unchanged if not a 2-element pattern

-- * Walk Operations

-- | Check if a list of relationships are consecutively connected.
--
-- Relationships are consecutively connected if the target of one
-- equals the source of the next.
--
-- == Internal Function
-- This is an internal helper function used by isWalk.
consecutivelyConnected :: Eq v => GraphLens v -> [Pattern v] -> Bool
consecutivelyConnected lens rels =
  case rels of
    [] -> True
    [_] -> True
    (r1:r2:rest) ->
      case (target lens r1, source lens r2) of
        (Just t, Just s) -> t == s && consecutivelyConnected lens (r2:rest)
        _ -> False

-- | Determine if a Pattern is a walk according to the lens.
--
-- A walk is a non-node pattern whose elements are all relationships,
-- where consecutive relationships share nodes (target of one equals
-- source of next).
--
-- == Properties
-- * Must not be a node (does not satisfy testNode predicate)
-- * All elements must be relationships (according to the lens)
-- * Consecutive relationships must be connected
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let walk = pattern "path" [rel1, rel2, rel3]
-- >>> isWalk lens walk
-- True
isWalk :: Eq v => GraphLens v -> Pattern v -> Bool
isWalk lens@(GraphLens _ _) p@(Pattern _ elems) =
  not (isNode lens p) &&
  all (isRelationship lens) elems &&
  consecutivelyConnected lens elems

-- | Extract all walks from the graph lens.
--
-- Walks are non-node patterns whose elements are all relationships,
-- where consecutive relationships share nodes.
--
-- == Time Complexity
-- O(n) where n is the number of direct elements in scopePattern
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> walks lens
-- [[path | [rel1], [rel2], [rel3]]]
walks :: Eq v => GraphLens v -> [Pattern v]
walks lens@(GraphLens (Pattern _ elems) _) =
  filter (isWalk lens) elems

-- | Extract nodes from a walk in traversal order.
--
-- Returns the source of the first relationship, followed by the targets
-- of subsequent relationships. Returns empty list if the pattern is not
-- a valid walk.
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> let walk = pattern "path" [rel1, rel2]
-- >>> walkNodes lens walk
-- [pattern "A", pattern "B", pattern "C"]
walkNodes :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
walkNodes lens p@(Pattern _ rels)
  | isWalk lens p = case rels of
      [] -> []
      (r:rest) -> case source lens r of
        Just s -> s : mapMaybe (target lens) (r:rest)
        Nothing -> []
  | otherwise = []

-- * Navigation Operations

-- | Find all neighbors of a node.
--
-- Neighbors are nodes connected to the given node via relationships
-- (either as source or target).
--
-- == Time Complexity
-- O(r) where r is the number of relationships in the graph
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> neighbors lens (point "Alice")
-- [point "Bob", pattern "Charlie"]
neighbors :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
neighbors lens node =
  let rels = relationships lens
      connectedNodes = concatMap (\r -> 
        case (source lens r, target lens r) of
          (Just s, Just t) | s == node -> [t]
                           | t == node -> [s]
          _ -> []
        ) rels
  in connectedNodes

-- | Find all relationships involving a node.
--
-- Returns relationships where the node is either source or target.
--
-- == Time Complexity
-- O(r) where r is the number of relationships in the graph
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> incidentRels lens (point "Alice")
-- [[knows | [Alice], [Bob]], [likes | [Charlie], [Alice]]]
incidentRels :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
incidentRels lens node =
  filter (\r ->
    case (source lens r, target lens r) of
      (Just s, _) | s == node -> True
      (_, Just t) | t == node -> True
      _ -> False
    ) (relationships lens)

-- | Compute the degree of a node (number of incident relationships).
--
-- The degree is the count of relationships where the node is either
-- source or target.
--
-- == Time Complexity
-- O(r) where r is the number of relationships in the graph
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> degree lens (point "Alice")
-- 3
degree :: Eq v => GraphLens v -> Pattern v -> Int
degree lens node = length (incidentRels lens node)

-- * Graph Analysis Operations

-- | Find all connected components in the graph.
--
-- A connected component is a set of nodes that are reachable from
-- each other via relationships. Returns a list of lists, where each
-- inner list represents a component.
--
-- == Time Complexity
-- O(n + r) where n is number of nodes and r is number of relationships
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> connectedComponents lens
-- [[pattern "A", pattern "B", pattern "C"], [pattern "D", pattern "E"]]
connectedComponents :: Ord v => GraphLens v -> [[Pattern v]]
connectedComponents lens = findComponents lens (nodes lens) Set.empty []

findComponents :: Ord v => GraphLens v -> [Pattern v] -> Set.Set (Pattern v) -> [[Pattern v]] -> [[Pattern v]]
findComponents _ [] _ acc = reverse acc
findComponents lens (n:ns) visited acc =
  if Set.member n visited
  then findComponents lens ns visited acc
  else
    let component = bfs lens n
        newVisited = Set.union visited (Set.fromList component)
        newAcc = component : acc
    in findComponents lens ns newVisited newAcc

-- | Perform breadth-first search from a starting node.
--
-- Returns all nodes reachable from the starting node via relationships.
--
-- == Time Complexity
-- O(n + r) where n is number of nodes and r is number of relationships
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> bfs lens (point "Alice")
-- [point "Alice", point "Bob", pattern "Charlie"]
bfs :: Ord v => GraphLens v -> Pattern v -> [Pattern v]
bfs lens start = bfsHelper lens Set.empty [start] []

bfsHelper :: Ord v => GraphLens v -> Set.Set (Pattern v) -> [Pattern v] -> [Pattern v] -> [Pattern v]
bfsHelper _ _ [] acc = reverse acc
bfsHelper lens visited (n:queue) acc
  | Set.member n visited = bfsHelper lens visited queue acc
  | otherwise =
      let newVisited = Set.insert n visited
          newAcc = n : acc
          nodeNeighbors = Pattern.Graph.neighbors lens n
          newQueue = queue ++ filter (not . (`Set.member` newVisited)) nodeNeighbors
      in bfsHelper lens newVisited newQueue newAcc

-- | Find a path between two nodes if one exists.
--
-- Returns Just [nodes] if a path exists, Nothing otherwise.
-- The path is a sequence of nodes connecting start to end.
--
-- == Time Complexity
-- O(n + r) where n is number of nodes and r is number of relationships
--
-- == Example
--
-- >>> let lens = GraphLens pattern isAtomic
-- >>> findPath lens (point "Alice") (pattern "Charlie")
-- Just [point "Alice", point "Bob", pattern "Charlie"]
findPath :: Ord v => GraphLens v -> Pattern v -> Pattern v -> Maybe [Pattern v]
findPath lens start end
  | start == end = Just [start]
  | otherwise = findPathHelper lens Set.empty [(start, [start])] end

findPathHelper :: Ord v => GraphLens v -> Set.Set (Pattern v) -> [(Pattern v, [Pattern v])] -> Pattern v -> Maybe [Pattern v]
findPathHelper _ _ [] _ = Nothing
findPathHelper lens visited ((n, path):queue) targetNode
  | n == targetNode = Just (reverse path)
  | Set.member n visited = findPathHelper lens visited queue targetNode
  | otherwise =
      let newVisited = Set.insert n visited
          nodeNeighbors = Pattern.Graph.neighbors lens n
          newPaths = map (\neighbor -> (neighbor, neighbor:path)) nodeNeighbors
          unvisitedPaths = filter (\(neighbor, _) -> not (Set.member neighbor newVisited)) newPaths
          newQueue = queue ++ unvisitedPaths
      in findPathHelper lens newVisited newQueue targetNode

