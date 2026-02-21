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
-- == GraphView
--
-- 'GraphView' is the universal transformation interface produced from a 'GraphLens'
-- (via 'toGraphView') or from a 'Pattern.PatternGraph.PatternGraph'
-- (via 'Pattern.PatternGraph.fromPatternGraph'). It pairs a snapshot 'GraphQuery'
-- with a categorized, traversable list of graph elements, enabling lazy pipeline
-- transformations:
--
-- > pipeline :: PatternGraph Subject -> PatternGraph Subject
-- > pipeline graph =
-- >   materialize canonicalClassifier LastWriteWins
-- >   . mapWithContext canonicalClassifier enrich
-- >   . filterGraph canonicalClassifier isRelevant dissolve
-- >   . mapAllGraph updateTimestamp
-- >   . fromPatternGraph canonicalClassifier
-- >   $ graph
--
-- Finalize a 'GraphView' pipeline back to a 'Pattern.PatternGraph.PatternGraph'
-- by calling 'Pattern.PatternGraph.materialize'.
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
{-# LANGUAGE TypeFamilies #-}
module Pattern.Graph
  ( -- * Graph Lens Type
    GraphLens(..)
  , mkGraphLens
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
    -- * GraphQuery constructor
  , fromGraphLens
    -- * GraphView constructor
  , toGraphView
    -- * GraphView and Substitution (re-exported from Pattern.Graph.Types)
  , GraphView(..)
  , Substitution(..)
  ) where

import Pattern.Core (Pattern(..))
import Data.Maybe (mapMaybe)

import Pattern.Graph.GraphClassifier (GraphClass(..), GraphClassifier(..), GraphValue(..))
import Pattern.Graph.GraphQuery (GraphQuery(..))
import Pattern.Graph.Types (GraphView(..), Substitution(..))

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

-- | Construct a 'GraphLens' using a predicate to identify nodes.
--
-- This encapsulates the context for interpreting graph structure.
mkGraphLens :: Pattern v -> (Pattern v -> Bool) -> GraphLens v
mkGraphLens = GraphLens

-- Helper to check walk validity under a specific node predicate
isValidWalk :: GraphValue v => (Pattern v -> Bool) -> [Pattern v] -> Bool
isValidWalk _ [] = False
isValidWalk p rels = not (null (foldl step [] rels))
  where
    step [] (Pattern _ [a, b]) = if p a && p b then [a, b] else []
    step active (Pattern _ [a, b]) =
      if p a && p b then
        let fromA = if any (\x -> identify (value a) == identify (value x)) active then [b] else []
            fromB = if any (\x -> identify (value b) == identify (value x)) active then [a] else []
        in fromA ++ fromB
      else []
    step _ _ = []

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
isNode (GraphLens _ test) p = test p

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
isRelationship lens@(GraphLens _ test) p@(Pattern _ els) =
  not (test p) && length els == 2 && all test els

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
consecutivelyConnected :: GraphValue v => GraphLens v -> [Pattern v] -> Bool
consecutivelyConnected lens rels =
  case rels of
    [] -> True
    [_] -> True
    (r1:r2:rest) ->
      case (target lens r1, source lens r2) of
        (Just t, Just s) -> identify (value t) == identify (value s) && consecutivelyConnected lens (r2:rest)
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
isWalk :: GraphValue v => GraphLens v -> Pattern v -> Bool
isWalk lens@(GraphLens _ test) p@(Pattern _ els) =
  not (test p) && not (null els)
  && all (\e -> length (elements e) == 2 && all test (elements e)) els
  && isValidWalk test els

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
walks :: GraphValue v => GraphLens v -> [Pattern v]
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
walkNodes :: GraphValue v => GraphLens v -> Pattern v -> [Pattern v]
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
neighbors :: GraphValue v => GraphLens v -> Pattern v -> [Pattern v]
neighbors lens node =
  let rels = relationships lens
      nodeId = identify (value node)
      connectedNodes = concatMap (\r ->
        case (source lens r, target lens r) of
          (Just s, Just t) | identify (value s) == nodeId -> [t]
                           | identify (value t) == nodeId -> [s]
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
incidentRels :: GraphValue v => GraphLens v -> Pattern v -> [Pattern v]
incidentRels lens node =
  let nodeId = identify (value node)
  in filter (\r ->
    case (source lens r, target lens r) of
      (Just s, _) | identify (value s) == nodeId -> True
      (_, Just t) | identify (value t) == nodeId -> True
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
degree :: GraphValue v => GraphLens v -> Pattern v -> Int
degree lens node = length (incidentRels lens node)

-- ============================================================================
-- GraphQuery constructor
-- ============================================================================

-- | Construct a 'GraphQuery' from a 'GraphLens'.
--
-- All fields are derived from existing 'Pattern.Graph' functions.
-- 'queryNodeById' and 'queryRelationshipById' perform O(n) \/ O(r) scans
-- (no index available from 'GraphLens'). 'queryContainers' scans relationships
-- and walks.
--
-- Note: defined here (not in "Pattern.Graph.GraphQuery") to avoid a circular
-- import between GraphQuery and Graph.
fromGraphLens :: (GraphValue v, Eq v) => GraphLens v -> GraphQuery v
fromGraphLens lens = GraphQuery
  { queryNodes            = nodes lens
  , queryRelationships    = relationships lens
  , queryIncidentRels     = incidentRels lens
  , querySource           = source lens
  , queryTarget           = target lens
  , queryDegree           = degree lens
  , queryNodeById         = \i ->
      foldr (\n acc -> if identify (value n) == i then Just n else acc)
            Nothing
            (nodes lens)
  , queryRelationshipById = \i ->
      foldr (\r acc -> if identify (value r) == i then Just r else acc)
            Nothing
            (relationships lens)
  , queryContainers       = \p ->
      let nodeId = identify (value p)
          inRel r = case (source lens r, target lens r) of
            (Just s, _) | identify (value s) == nodeId -> True
            (_, Just t) | identify (value t) == nodeId -> True
            _ -> False
          containingRels  = filter inRel (relationships lens)
          containingWalks = filter
            (\w -> any (\r -> identify (value r) == nodeId) (elements w))
            (walks lens)
      in containingRels ++ containingWalks
  }

-- ============================================================================
-- GraphView constructor from GraphLens
-- ============================================================================

-- | Construct a 'GraphView' from a 'GraphLens' and a 'GraphClassifier'.
--
-- The classifier assigns a 'GraphClass' tag to each element in the lens scope.
-- 'viewQuery' is the snapshot query built from the same lens.
--
-- Note: defined here (not in "Pattern.Graph.GraphQuery") to avoid a circular
-- import — GraphQuery cannot import Pattern.Graph.
toGraphView
  :: (GraphValue v, Eq v)
  => GraphClassifier extra v
  -> GraphLens v
  -> GraphView extra v
toGraphView classifier lens =
  GraphView
    { viewQuery    = fromGraphLens lens
    , viewElements = map (\p -> (classify classifier p, p)) scopeElems
    }
  where
    GraphLens (Pattern _ scopeElems) _ = lens
