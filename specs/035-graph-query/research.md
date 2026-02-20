# Research: GraphQuery — Portable, Composable Graph Query Interface

**Branch**: `035-graph-query` | **Date**: 2026-02-20  
**Purpose**: Resolve all design unknowns before Phase 1 design artifacts.

---

## Decision 1: Record-of-Functions vs. Typeclass for `GraphQuery`

**Decision**: Record-of-functions (`data GraphQuery v = GraphQuery { ... }`)

**Rationale**: Consistent with the existing `GraphClassifier` design in this codebase. A record-of-functions is a first-class value: it can be passed, stored, transformed, and composed without typeclass machinery. This enables `frameQuery`, `memoizeIncidentRels`, and database-backed implementations as plain `GraphQuery v → GraphQuery v` transformations. A typeclass would require `newtype` wrappers for each variant and cannot be composed at runtime.

**Alternatives considered**:
- Typeclass `class GraphQuery g where ...` — rejected because it cannot be composed at runtime (framing, caching, logging all require wrapping), and it is inconsistent with `GraphClassifier`.
- MTL-style `ReaderT GraphQuery` — rejected as unnecessary complexity for a pure query interface.

---

## Decision 2: `TraversalWeight` as a Function vs. Enum

**Decision**: `type TraversalWeight v = Pattern v -> TraversalDirection -> Double`

**Rationale**: A function type subsumes all enum cases and allows user-defined weights (reading numeric properties from relationships, temporal costs, etc.) without any extension points. The canonical cases (`undirected`, `directed`, `directedReverse`) are provided as library values of this type. Infinity (`1/0`) encodes impassability, which is standard in shortest-path algorithms (Dijkstra, Bellman-Ford).

**Alternatives considered**:
- `data TraversalPolicy = Undirected | Directed | DirectedReverse | Weighted (Pattern v -> Double)` — rejected because it requires pattern-matching in every algorithm and cannot compose policies.
- Separate `directed :: Bool` parameter — rejected because it cannot encode weighted traversal.

---

## Decision 3: `queryNeighbors` Omission

**Decision**: `queryNeighbors` is absent from `GraphQuery`. Algorithms derive reachable neighbors from `queryIncidentRels` + `querySource` + `queryTarget` + `TraversalWeight`.

**Rationale**: "Neighbors" is not a fixed concept when traversal direction is external. With `undirected`, both endpoints of every incident relationship are neighbors. With `directed`, only the target of forward-traversable relationships is a neighbor. Encoding this in `queryNeighbors` would require `TraversalWeight` as a parameter to the field itself, making it a higher-order field that complicates construction. Deriving neighbors in algorithms from the three primitives is straightforward and keeps the interface minimal.

**Alternatives considered**:
- `queryNeighbors :: TraversalWeight v -> Pattern v -> [Pattern v]` as a field — rejected because it embeds traversal policy in the interface, conflating structure and interpretation.

---

## Decision 4: `queryContainers` as a First-Class Field

**Decision**: `queryContainers :: Pattern v -> [Pattern v]` is a required field of `GraphQuery`, not a derived helper.

**Rationale**: Upward traversal ("what contains this element?") cannot be derived from the other fields without O(n) scans of all relationships, walks, and annotations. Making it a field allows implementations (e.g., `fromPatternGraph`) to provide O(log n) map lookups. It is required by the planned `GraphMutation` feature for coherent deletion and by context query helpers. Omitting it would force callers to reconstruct containment from scratch.

**Alternatives considered**:
- Derived function scanning all relationships/walks/annotations — rejected because it is O(n) and cannot be optimized by implementations.
- Separate `ContainerIndex` type — rejected as unnecessary indirection; the field achieves the same result.

---

## Decision 5: `fromPatternGraph` Direct Construction (No `toGraphLens` Intermediary)

**Decision**: `fromPatternGraph :: (GraphValue v, Eq v) => PatternGraph v -> GraphQuery v` reads directly from `pgNodes`, `pgRelationships`, `pgWalks`, `pgAnnotations` maps.

**Rationale**: `toGraphLens` materializes all elements into a flat scope pattern and uses an atomic predicate, losing the typed map structure. `fromPatternGraph` reads from the typed maps directly, giving O(log n) lookups for `queryNodeById` and `queryRelationshipById` instead of O(n) scans. `queryContainers` can filter the typed maps directly. This is the primary motivation for the feature.

**Alternatives considered**:
- `fromPatternGraph pg = fromGraphLens (toGraphLens pg)` — rejected because it defeats the purpose (O(n) scans, loss of typed structure).

---

## Decision 6: Backward Compatibility Strategy for `GraphLens` Algorithms

**Decision**: Retain `bfs`, `findPath`, `connectedComponents` in `Pattern.Graph` as wrappers that call `Pattern.Graph.Algorithms.*` with `fromGraphLens` and `undirected` as defaults.

**Rationale**: Existing callers have no changes to make. The wrappers are one-liners. The `GraphValue v` constraint already present on these functions is preserved. This satisfies FR-011 and SC-002.

**Implementation**:
```haskell
-- Pattern.Graph (backward compat wrappers)
bfs :: GraphValue v => GraphLens v -> Pattern v -> [Pattern v]
bfs lens start = Algorithms.bfs (fromGraphLens lens) undirected start

findPath :: GraphValue v => GraphLens v -> Pattern v -> Pattern v -> Maybe [Pattern v]
findPath lens s e = Algorithms.shortestPath (fromGraphLens lens) undirected s e

connectedComponents :: GraphValue v => GraphLens v -> [[Pattern v]]
connectedComponents lens = Algorithms.connectedComponents (fromGraphLens lens) undirected
```

---

## Decision 7: `toGraphLens` Deprecation Strategy

**Decision**: Mark `toGraphLens` and `toGraphLensWithScope` as deprecated with a Haddock `{-# DEPRECATED #-}` pragma pointing to `fromPatternGraph`. Remove in a future version after callers have migrated.

**Rationale**: Hard removal in this feature would break any callers outside the library. Deprecation gives a migration path. The spec says "retire" — deprecation is the correct Haskell mechanism for a controlled retirement.

**Alternatives considered**:
- Hard removal now — rejected because it may break downstream callers not visible in this repo.
- No deprecation — rejected because it leaves the API ambiguous about the preferred path.

---

## Decision 8: Module Layout

**Decision**:
- `Pattern.Graph.GraphQuery` — `GraphQuery v`, `TraversalDirection`, `TraversalWeight`, `fromGraphLens`, `fromPatternGraph`, `frameQuery`, `memoizeIncidentRels`
- `Pattern.Graph.Algorithms` — all graph algorithms (`bfs`, `dfs`, `shortestPath`, `hasPath`, `allPaths`, `isNeighbor`, `isConnected`, `connectedComponents`, `topologicalSort`, `hasCycle`, `minimumSpanningTree`, `degreeCentrality`, `betweennessCentrality`, context helpers)

**Rationale**: Keeps the interface type and its constructors/combinators together in one module. Algorithms are a separate concern and may grow into sub-modules (`Algorithms.Path`, `Algorithms.Centrality`) in a future feature. The split mirrors the `GraphClassifier` pattern: type in one module, usage in another.

**Alternatives considered**:
- Everything in `Pattern.Graph.GraphQuery` — rejected because algorithms would make the module very large.
- Sub-modules now (`Algorithms.Path`, `Algorithms.Centrality`) — deferred; premature until the algorithm surface is larger.

---

## Decision 9: `queryDegree` as Explicit Field

**Decision**: Include `queryDegree :: Pattern v -> Int` as an explicit field even though it is derivable from `queryIncidentRels`.

**Rationale**: Implementations backed by a database or index may provide O(1) degree queries. Making it a field allows those implementations to avoid O(n) scans. The default implementation in `fromGraphLens` and `fromPatternGraph` derives it from `queryIncidentRels`, so there is no cost for in-memory implementations.

---

## Decision 10: Context Query Helpers as Derived Functions

**Decision**: `queryAnnotationsOf`, `queryWalksContaining`, and `queryCoMembers` are plain functions in `Pattern.Graph.Algorithms`, not fields of `GraphQuery`.

**Rationale**: They are derived from `queryContainers` + `GraphClassifier` filtering. No new interface fields are needed. Callers pay only for what they use. This is consistent with the proposal's "derived, not primitive" decision.

---

## Decision 12: `endpoints` Helper in `frameQuery` (Proposal Example Clarification)

**Decision**: The proposal's `frameQuery` example uses `filter (any include . endpoints)` to filter incident relationships. `endpoints` is **not** an existing exported function in `Pattern.Graph`. The correct implementation uses `querySource` and `queryTarget` from the `GraphQuery` being wrapped.

**Correct `frameQuery` filter for `queryIncidentRels`**:
```haskell
queryIncidentRels = \n ->
  filter (\r -> maybe False include (querySource base r)
             && maybe False include (queryTarget base r))
         (queryIncidentRels base n)
```

This excludes any relationship where either endpoint falls outside the frame. It uses only `querySource` and `queryTarget` — both already fields of `GraphQuery` — with no need for a separate `endpoints` helper.

**Rationale**: The proposal example was illustrative pseudocode. The actual implementation must use the `GraphQuery` interface consistently. Introducing a separate `endpoints` function would add surface area without benefit.

**Impact on tasks**: T045 description is correct in referencing `querySource`/`queryTarget`. No task changes needed; this decision documents the implementation approach.

---

## Decision 11: `TraversalWeight` Module Placement (Deviation from Proposal)

**Decision**: `TraversalDirection`, `TraversalWeight`, and its canonical values (`undirected`, `directed`, `directedReverse`) are defined in `Pattern.Graph.GraphQuery`, not in a separate `Pattern.Graph.TraversalWeight` module.

**Rationale**: The proposal's "What changes" table lists `Pattern.Graph.TraversalWeight` as a distinct new module. However, `TraversalWeight` is a type alias and three small values — insufficient to justify a standalone module. It is conceptually inseparable from `GraphQuery`: every `GraphQuery` consumer also needs `TraversalWeight`. Co-locating them reduces import boilerplate and is consistent with how `GraphClassifier` co-locates `GraphClass`, `GraphValue`, and `GraphClassifier` in one module. The proposal's module table is a design sketch; this plan supersedes it on module granularity.

**Alternatives considered**:
- `Pattern.Graph.TraversalWeight` as a separate module (per proposal) — rejected because the module would be tiny (one type alias + three values) and every `GraphQuery` user would need both imports.
- Re-export from `Pattern.Graph` — possible in a future polish pass; not needed for this feature.

**Impact on artifacts**: `pattern.cabal` exposes `Pattern.Graph.GraphQuery` (not `Pattern.Graph.TraversalWeight`). Contracts, tasks, and data-model all reflect this decision.

---

## Open Questions Resolved

| Question from Proposal | Resolution |
|------------------------|------------|
| `queryAdjacency` for bulk algorithms | Deferred to a follow-on feature (`Pattern.Graph.Algorithms.Bulk`). Not in scope for this feature. |
| `Pattern.Graph.Algorithms` module granularity | Single module for now; sub-modules deferred until algorithm surface grows. |
| Centrality algorithm tuning parameters | Exact parameters (convergence tolerance, damping factor) determined during implementation. Signatures are representative. |
| Record-of-functions performance vs. typeclass | `{-# INLINE #-}` pragmas on hot-path functions (`queryIncidentRels`, `querySource`, `queryTarget`, `queryDegree`). Benchmarking against typeclass deferred to a performance-focused follow-on. |
