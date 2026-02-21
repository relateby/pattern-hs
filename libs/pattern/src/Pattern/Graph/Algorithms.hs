-- | Graph algorithms operating on 'GraphQuery v'.
--
-- All traversal algorithms accept a 'TraversalWeight v' at the call site,
-- enabling the same 'GraphQuery' to be used with directed, undirected, or
-- custom-weighted traversal without any conversion.
--
-- == Categorical Interpretation
--
-- Algorithms are natural transformations over the 'GraphQuery' coalgebra.
-- They unfold the coalgebra according to a traversal policy ('TraversalWeight')
-- and accumulate results.
--
-- == Complexity Note
--
-- 'betweennessCentrality' uses the Brandes algorithm: O(n·(n+r)·log n).
-- It calls 'queryIncidentRels' in the inner loop. For large graphs, wrap
-- the 'GraphQuery' with 'memoizeIncidentRels' before calling this function.
-- TODO: bulk adjacency — see open question §1 in the feature proposal.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Pattern.Graph.Algorithms
  ( -- * Traversal
    bfs
  , dfs
    -- * Paths
  , shortestPath
  , hasPath
  , allPaths
    -- * Boolean queries
  , isNeighbor
  , isConnected
    -- * Structural
  , connectedComponents
  , topologicalSort
  , hasCycle
    -- * Spanning
  , minimumSpanningTree
    -- * Centrality
  , degreeCentrality
  , betweennessCentrality
    -- * Context query helpers
  , queryAnnotationsOf
  , queryWalksContaining
  , queryCoMembers
  ) where

import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Pattern.Core (Pattern(..))
import Pattern.Graph.GraphClassifier (GraphValue(..), GraphClass(..), GraphClassifier(..))
import Pattern.Graph.GraphQuery (GraphQuery(..), TraversalWeight, TraversalDirection(..))

-- ============================================================================
-- Internal helper: reachable neighbors given a traversal weight
-- ============================================================================

-- | Given a 'GraphQuery', a 'TraversalWeight', and a node, return all
-- neighbor nodes reachable via relationships with finite traversal cost.
--
-- Inlined by GHC at call sites to eliminate the function-call overhead in
-- the inner loop of every traversal algorithm.
{-# INLINE reachableNeighbors #-}
reachableNeighbors :: GraphValue v => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]
reachableNeighbors gq weight node =
  mapMaybe neighborOf (queryIncidentRels gq node)
  where
    neighborOf rel =
      let fwdCost = weight rel Forward
          bwdCost = weight rel Backward
          src = querySource gq rel
          tgt = queryTarget gq rel
          nodeId = identify (value node)
      in case (src, tgt) of
           (Just s, Just t)
             | identify (value s) == nodeId && isFinite fwdCost -> Just t
             | identify (value t) == nodeId && isFinite bwdCost -> Just s
           _ -> Nothing
    isFinite x = not (isInfinite x) && not (isNaN x)

-- ============================================================================
-- Traversal
-- ============================================================================

-- | Breadth-first search from a starting node.
--
-- Returns all nodes reachable from @start@ in BFS order, including @start@.
-- Traversal direction and cost are governed by @weight@.
bfs :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]
bfs gq weight start = go (Seq.singleton start) (Set.singleton (identify (value start))) []
  where
    go queue visited acc =
      case Seq.viewl queue of
        Seq.EmptyL -> reverse acc
        n Seq.:< rest ->
          let nbrs = filter (\nb -> not (Set.member (identify (value nb)) visited))
                            (reachableNeighbors gq weight n)
              newVisited = foldl' (\s nb -> Set.insert (identify (value nb)) s) visited nbrs
              newQueue   = foldl' (Seq.|>) rest nbrs
          in go newQueue newVisited (n : acc)

-- | Depth-first search from a starting node.
--
-- Returns all nodes reachable from @start@ in DFS order, including @start@.
-- Traversal direction and cost are governed by @weight@.
dfs :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> Pattern v -> [Pattern v]
dfs gq weight start = go [start] Set.empty []
  where
    go [] _ acc = reverse acc
    go (n:stack) visited acc
      | Set.member (identify (value n)) visited = go stack visited acc
      | otherwise =
          let newVisited = Set.insert (identify (value n)) visited
              nbrs = filter (\nb -> not (Set.member (identify (value nb)) newVisited))
                            (reachableNeighbors gq weight n)
          in go (nbrs ++ stack) newVisited (n : acc)

-- ============================================================================
-- Paths
-- ============================================================================

-- | Shortest path between two nodes using Dijkstra's algorithm.
--
-- Returns 'Just' a list of nodes (including endpoints) if a path exists,
-- 'Nothing' otherwise. Edge costs are determined by @weight@.
shortestPath :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Maybe [Pattern v]
shortestPath gq weight start end
  | identify (value start) == identify (value end) = Just [start]
  | otherwise = dijkstra
      -- Priority queue keyed by (cost, nodeId) so deleteFindMin gives lowest-cost entry
      (Map.singleton (0.0, identify (value start)) [start])
      -- Best known cost per node
      (Map.singleton (identify (value start)) 0.0)
      Set.empty
  where
    endId = identify (value end)

    dijkstra pq bestCost settled
      | Map.null pq = Nothing
      | otherwise =
          let (((cost, nId), path), rest) = Map.deleteFindMin pq
          in case path of
            [] -> dijkstra rest bestCost settled
            (n:_)
              | Set.member nId settled -> dijkstra rest bestCost settled
              | nId == endId -> Just (reverse path)
              | otherwise ->
                  let newSettled = Set.insert nId settled
                      rels = queryIncidentRels gq n
                      updates = mapMaybe (edgeUpdate cost path n newSettled) rels
                      (pq', bestCost') = foldl' insertIfBetter (rest, bestCost) updates
                  in dijkstra pq' bestCost' newSettled

    edgeUpdate cost path node settled rel =
      let fwdCost = weight rel Forward
          bwdCost = weight rel Backward
          src = querySource gq rel
          tgt = queryTarget gq rel
          nodeId = identify (value node)
      in case (src, tgt) of
           (Just s, Just t)
             | identify (value s) == nodeId && isFinite fwdCost && not (Set.member (identify (value t)) settled) ->
                 Just (identify (value t), cost + fwdCost, t : path)
             | identify (value t) == nodeId && isFinite bwdCost && not (Set.member (identify (value s)) settled) ->
                 Just (identify (value s), cost + bwdCost, s : path)
           _ -> Nothing

    insertIfBetter (pq, bestCost) (nId, newCost, newPath) =
      case Map.lookup nId bestCost of
        Just oldCost | oldCost <= newCost -> (pq, bestCost)
        _ -> ( Map.insert (newCost, nId) newPath pq
             , Map.insert nId newCost bestCost
             )

    isFinite x = not (isInfinite x) && not (isNaN x)

-- | Return 'True' if a path exists between @src@ and @tgt@.
hasPath :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Bool
hasPath gq weight src tgt = case shortestPath gq weight src tgt of
  Just _  -> True
  Nothing -> False

-- | All simple paths between two nodes (DFS-based, no repeated nodes).
--
-- Returns @[]@ if no path exists or the graph is empty.
-- Warning: exponential in the worst case for dense graphs.
allPaths :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> [[Pattern v]]
allPaths gq weight start end = go [start] (Set.singleton (identify (value start)))
  where
    endId = identify (value end)
    go path visited = case path of
      [] -> []
      (n:_)
        | identify (value n) == endId -> [reverse path]
        | otherwise ->
            let nbrs = filter (\nb -> not (Set.member (identify (value nb)) visited))
                              (reachableNeighbors gq weight n)
            in concatMap (\nb -> go (nb : path) (Set.insert (identify (value nb)) visited)) nbrs

-- ============================================================================
-- Boolean queries
-- ============================================================================

-- | Return 'True' if @a@ and @b@ are directly connected by a relationship
-- with finite traversal cost.
isNeighbor :: (GraphValue v, Eq (Id v)) => GraphQuery v -> TraversalWeight v -> Pattern v -> Pattern v -> Bool
isNeighbor gq weight a b =
  let bId = identify (value b)
  in any (\nb -> identify (value nb) == bId) (reachableNeighbors gq weight a)

-- | Return 'True' if the graph is connected under the given traversal weight.
--
-- An empty graph is considered connected (vacuously true).
isConnected :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> Bool
isConnected gq weight =
  case queryNodes gq of
    []    -> True
    (n:_) -> length (bfs gq weight n) == length (queryNodes gq)

-- ============================================================================
-- Structural
-- ============================================================================

-- | Find all connected components under the given traversal weight.
--
-- Returns a list of node lists, each representing one component.
connectedComponents :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> [[Pattern v]]
connectedComponents gq weight = go (queryNodes gq) Set.empty []
  where
    go [] _ acc = reverse acc
    go (n:ns) visited acc
      | Set.member (identify (value n)) visited = go ns visited acc
      | otherwise =
          let component = bfs gq weight n
              newVisited = foldl' (\s m -> Set.insert (identify (value m)) s) visited component
          in go ns newVisited (component : acc)

-- | Topological sort using DFS (Kahn-style post-order).
--
-- Returns 'Nothing' if the graph contains a cycle.
-- Operates on the directed structure implied by relationship endpoint order
-- (source → target), ignoring 'TraversalWeight'.
topologicalSort :: (GraphValue v, Ord (Id v)) => GraphQuery v -> Maybe [Pattern v]
topologicalSort gq = go (queryNodes gq) Set.empty Set.empty []
  where
    go [] _ _ acc = Just acc
    go (n:ns) visited inStack acc
      | Set.member (identify (value n)) visited = go ns visited inStack acc
      | otherwise = case visit n visited inStack acc of
          Nothing -> Nothing
          Just (visited', acc') -> go ns visited' inStack acc'

    visit n visited inStack acc
      | Set.member nId inStack = Nothing
      | Set.member nId visited = Just (visited, acc)
      | otherwise =
          let newInStack = Set.insert nId inStack
              -- Only follow edges where n is the source (outgoing edges)
              outgoing = filter (\r -> case querySource gq r of
                                         Just s -> identify (value s) == nId
                                         Nothing -> False)
                                (queryIncidentRels gq n)
              nbrs = mapMaybe (queryTarget gq) outgoing
          in case foldl' (visitStep newInStack) (Just (visited, acc)) nbrs of
               Nothing -> Nothing
               Just (visited', acc') ->
                 Just (Set.insert nId visited', n : acc')
      where nId = identify (value n)

    visitStep _ Nothing _ = Nothing
    visitStep inStack (Just (visited, acc)) nb = visit nb visited inStack acc

-- | Return 'True' if the graph contains a directed cycle.
hasCycle :: (GraphValue v, Ord (Id v)) => GraphQuery v -> Bool
hasCycle gq = case topologicalSort gq of
  Nothing -> True
  Just _  -> False

-- ============================================================================
-- Spanning
-- ============================================================================

-- | Minimum spanning tree using Kruskal's algorithm.
--
-- Returns the list of nodes in the MST (not edges). For a forest (disconnected
-- graph), returns nodes reachable in the minimum spanning forest.
-- Edge weight is the average of forward and backward traversal costs.
minimumSpanningTree :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> [Pattern v]
minimumSpanningTree gq weight =
  let rels = queryRelationships gq
      edgesWithCost = mapMaybe edgeCost rels
      sortedEdges = sortBy (comparing (\(c, _, _) -> c)) edgesWithCost
      nodeIds = map (identify . value) (queryNodes gq)
      initialUF = Map.fromList [(i, i) | i <- nodeIds]
      (_, mstNodes) = foldl' addEdge (initialUF, Set.empty) sortedEdges
  in filter (\n -> Set.member (identify (value n)) mstNodes) (queryNodes gq)
  where
    edgeCost rel = case (querySource gq rel, queryTarget gq rel) of
      (Just s, Just t) ->
        let fwd = weight rel Forward
            bwd = weight rel Backward
            cost = min fwd bwd
        in if isInfinite cost then Nothing else Just (cost, s, t)
      _ -> Nothing

    addEdge (uf, nodes) (_, s, t) =
      let sRoot = find uf (identify (value s))
          tRoot = find uf (identify (value t))
      in if sRoot == tRoot
         then (uf, nodes)
         else ( Map.insert sRoot tRoot uf
              , Set.insert (identify (value s)) (Set.insert (identify (value t)) nodes)
              )

    find uf i = case Map.lookup i uf of
      Nothing -> i
      Just p  -> if p == i then i else find uf p

-- ============================================================================
-- Centrality
-- ============================================================================

-- | Degree centrality: normalized count of incident relationships per node.
--
-- Returns a map from node identity to normalized degree in [0, 1].
-- Normalization factor is (n - 1) where n is the number of nodes.
degreeCentrality :: (GraphValue v, Ord (Id v)) => GraphQuery v -> Map (Id v) Double
degreeCentrality gq =
  let ns = queryNodes gq
      n  = length ns
      norm = if n <= 1 then 1.0 else fromIntegral (n - 1)
  in Map.fromList
       [ (identify (value node), fromIntegral (queryDegree gq node) / norm)
       | node <- ns
       ]

-- | Betweenness centrality using the Brandes algorithm.
--
-- Returns a map from node identity to betweenness score (unnormalized).
-- Complexity: O(n·(n+r)·log n). For large graphs, wrap the 'GraphQuery'
-- with 'memoizeIncidentRels' before calling this function.
--
-- TODO: bulk adjacency — see open question §1 in the feature proposal.
-- This implementation calls 'queryIncidentRels' in the inner loop, which
-- is O(r) per call. A bulk adjacency representation would reduce this to O(1).
betweennessCentrality :: (GraphValue v, Ord (Id v)) => GraphQuery v -> TraversalWeight v -> Map (Id v) Double
betweennessCentrality gq weight =
  let ns = queryNodes gq
      initial = Map.fromList [(identify (value n), 0.0) | n <- ns]
  in foldl' (accumulate ns) initial ns
  where
    accumulate ns betweenness s =
      let (sigma, pred, dist) = bfsPhase s ns
          delta = Map.fromList [(identify (value n), 0.0) | n <- ns]
          stack = sortBy (comparing (\n -> negate (fromMaybe 0.0 (Map.lookup (identify (value n)) dist)))) ns
          delta' = foldl' (backProp sigma pred) delta stack
      in Map.mapWithKey (\k v -> v + fromMaybe 0.0 (Map.lookup k delta')) betweenness

    bfsPhase s ns =
      let sId = identify (value s)
          sigma0 = Map.fromList [(identify (value n), 0.0) | n <- ns]
          sigma1 = Map.insert sId 1.0 sigma0
          dist0  = Map.fromList [(identify (value n), -1.0) | n <- ns]
          dist1  = Map.insert sId 0.0 dist0
          emptyPreds = [] `asTypeOf` [identify (value s)]
          pred0  = Map.fromList [(identify (value n), emptyPreds) | n <- ns]
      in bfsLoop (Seq.singleton s) sigma1 pred0 dist1

    bfsLoop queue sigma pred dist =
      case Seq.viewl queue of
        Seq.EmptyL -> (sigma, pred, dist)
        v Seq.:< rest ->
          let vId   = identify (value v)
              vDist = fromMaybe 0.0 (Map.lookup vId dist)
              vSig  = fromMaybe 0.0 (Map.lookup vId sigma)
              rels  = queryIncidentRels gq v
              (sigma', pred', dist', newQueue) =
                foldl' (processNeighbor vId vDist vSig) (sigma, pred, dist, rest) rels
          in bfsLoop newQueue sigma' pred' dist'

    processNeighbor vId vDist vSig (sigma, pred, dist, queue) rel =
      let fwdCost = weight rel Forward
          bwdCost = weight rel Backward
          src = querySource gq rel
          tgt = queryTarget gq rel
      in case (src, tgt) of
           (Just s, Just t)
             | identify (value s) == vId && isFinite fwdCost ->
                 updateNeighbor vId vDist vSig (identify (value t)) t (sigma, pred, dist, queue)
             | identify (value t) == vId && isFinite bwdCost ->
                 updateNeighbor vId vDist vSig (identify (value s)) s (sigma, pred, dist, queue)
           _ -> (sigma, pred, dist, queue)

    updateNeighbor vId vDist vSig wId w (sigma, pred, dist, queue) =
      let wDist = fromMaybe (-1.0) (Map.lookup wId dist)
          wSig  = fromMaybe 0.0   (Map.lookup wId sigma)
          vSig' = fromMaybe 0.0   (Map.lookup vId sigma)
      in if wDist < 0
         then ( Map.insert wId (wSig + vSig') sigma
              , Map.adjust (vId :) wId pred
              , Map.insert wId (vDist + 1) dist
              , queue Seq.|> w
              )
         else if wDist == vDist + 1
              then ( Map.insert wId (wSig + vSig') sigma
                   , Map.adjust (vId :) wId pred
                   , dist
                   , queue
                   )
              else (sigma, pred, dist, queue)

    backProp sigma pred delta w =
      let wId = identify (value w)
          preds = fromMaybe [] (Map.lookup wId pred)
          wSig  = fromMaybe 1.0 (Map.lookup wId sigma)
          wDelta = fromMaybe 0.0 (Map.lookup wId delta)
          delta' = foldl' (\d vId ->
            let vSig  = fromMaybe 1.0 (Map.lookup vId sigma)
                vDelta = fromMaybe 0.0 (Map.lookup vId d)
                contribution = (vSig / wSig) * (1.0 + wDelta)
            in Map.insert vId (vDelta + contribution) d
            ) delta preds
      in delta'

    isFinite x = not (isInfinite x) && not (isNaN x)

-- ============================================================================
-- Context query helpers
-- ============================================================================

-- | Return all annotations that directly contain the given element.
--
-- Filters the result of 'queryContainers' to elements classified as 'GAnnotation'.
queryAnnotationsOf :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]
queryAnnotationsOf classifier gq p =
  filter (isAnnotation . classify classifier) (queryContainers gq p)
  where
    isAnnotation GAnnotation = True
    isAnnotation _           = False

-- | Return all walks that directly contain the given element.
--
-- Filters the result of 'queryContainers' to elements classified as 'GWalk'.
queryWalksContaining :: GraphClassifier extra v -> GraphQuery v -> Pattern v -> [Pattern v]
queryWalksContaining classifier gq p =
  filter (isWalk . classify classifier) (queryContainers gq p)
  where
    isWalk GWalk = True
    isWalk _     = False

-- | Return all co-members of @element@ within @container@.
--
-- Co-members are the other elements that are contained by @container@ (i.e. all
-- elements that share this container with @element@), excluding @element@ itself.
-- E.g. for two nodes that share a common walk, calling 'queryCoMembers' with
-- one node and the walk as container returns the other node(s) in that walk.
queryCoMembers :: (GraphValue v, Eq (Id v)) => GraphQuery v -> Pattern v -> Pattern v -> [Pattern v]
queryCoMembers gq element container =
  let containerId = identify (value container)
      elementId   = identify (value element)
      inContainer e = any (\c -> identify (value c) == containerId) (queryContainers gq e)
      candidates     = queryNodes gq ++ queryRelationships gq
      allInContainer = filter inContainer candidates
  in filter (\e -> identify (value e) /= elementId) allInContainer
