# Feature Specification: GraphQuery — Portable, Composable Graph Query Interface

**Feature Branch**: `035-graph-query`  
**Created**: 2026-02-20  
**Status**: Draft  
**Input**: User description: "GraphQuery — A Portable, Composable Graph Query Interface as described in @proposals/graph-query.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Run Graph Algorithms on Any Representation (Priority: P1)

A developer working with a `PatternGraph` wants to run a shortest-path or connected-components query without first converting the graph to a `GraphLens`. They construct a `GraphQuery` from their `PatternGraph` and call the algorithm directly, receiving results without any intermediate conversion or data loss.

**Why this priority**: This is the core value of the feature — decoupling algorithms from representation. Without it, the feature delivers nothing. Every other story depends on this foundation.

**Independent Test**: Can be fully tested by constructing a `GraphQuery` from a `PatternGraph`, calling `shortestPath` or `connectedComponents`, and verifying correct results — without ever touching `GraphLens`.

**Acceptance Scenarios**:

1. **Given** a `PatternGraph` with nodes and relationships, **When** `fromPatternGraph` is called, **Then** a `GraphQuery` is produced that correctly enumerates nodes, relationships, incident relationships, sources, targets, and degrees.
2. **Given** a `GraphQuery` derived from a `PatternGraph`, **When** `shortestPath` is called with two nodes, **Then** the correct path (or `Nothing`) is returned.
3. **Given** a `GraphQuery` derived from a `GraphLens`, **When** `connectedComponents` is called, **Then** results match those previously returned by the `GraphLens`-specific implementation.
4. **Given** a `GraphQuery` derived from either source, **When** `bfs` or `dfs` is called with a start node, **Then** all reachable nodes are returned in the correct order.

---

### User Story 2 - Control Traversal Direction and Weight at the Call Site (Priority: P2)

A developer running a path-finding algorithm wants to treat the same graph as directed in one call and undirected in another, without creating separate graph structures. They supply a `TraversalWeight` value (`directed`, `undirected`, or a custom function) as a parameter to each algorithm call.

**Why this priority**: Traversal policy is the primary differentiator between many graph use cases. Without `TraversalWeight`, the interface is less expressive than what already exists on `GraphLens`.

**Independent Test**: Can be fully tested by calling `hasPath` on the same `GraphQuery` with `directed` and `undirected` weights and verifying that results differ when the graph contains one-way relationships.

**Acceptance Scenarios**:

1. **Given** a graph with a directed relationship A→B, **When** `hasPath` is called with `directed` weight from A to B, **Then** the result is `True`.
2. **Given** the same graph, **When** `hasPath` is called with `directed` weight from B to A, **Then** the result is `False`.
3. **Given** the same graph, **When** `hasPath` is called with `undirected` weight from B to A, **Then** the result is `True`.
4. **Given** a custom `TraversalWeight` that reads a numeric property from a relationship, **When** `shortestPath` is called, **Then** the path with the lowest total weight is returned.

---

### User Story 3 - Compose Graph Views Without New Types (Priority: P3)

A developer wants to run algorithms over a filtered subgraph — for example, only nodes with a specific label, or only relationships within a time window. They use `frameQuery` to wrap an existing `GraphQuery` with a predicate, producing a new `GraphQuery` that algorithms treat as a complete graph.

**Why this priority**: Composability is what makes the interface extensible without new types or instances. It enables database-backed graphs, caching, and logging as well as subgraph views.

**Independent Test**: Can be fully tested by applying `frameQuery` to a `GraphQuery`, calling `queryNodes` on the result, and verifying only matching nodes are returned; then running an algorithm and confirming it operates only within the frame.

**Acceptance Scenarios**:

1. **Given** a `GraphQuery` with mixed-label nodes, **When** `frameQuery` is applied with a label predicate, **Then** `queryNodes` on the result returns only matching nodes.
2. **Given** a framed `GraphQuery`, **When** `queryIncidentRels` is called for a node in the frame, **Then** only relationships whose endpoints are both within the frame are returned.
3. **Given** a `GraphQuery` wrapped with `memoizeIncidentRels`, **When** `queryIncidentRels` is called repeatedly for the same node, **Then** the underlying function is invoked only once.
4. **Given** composing `memoizeIncidentRels . frameQuery predicate $ fromPatternGraph pg`, **When** any algorithm is run, **Then** it operates correctly on the filtered, memoized view.

---

### User Story 4 - Upward Context Traversal (Priority: P4)

A developer needs to know what higher-order structures contain a given node or relationship — which walks include it, which annotations are attached to it, which containers reference it. They call `queryContainers` or the derived helpers (`queryAnnotationsOf`, `queryWalksContaining`, `queryCoMembers`) to answer these questions without precomputing a context record.

**Why this priority**: Required by the planned GraphMutation feature for coherent deletion, and independently useful for impact analysis and pattern matching. Lower priority than core traversal because it is not needed for basic algorithm use.

**Independent Test**: Can be fully tested by constructing a graph with annotations and walks, calling `queryContainers` on a node, and verifying all containing structures are returned.

**Acceptance Scenarios**:

1. **Given** a node that participates in a walk and has an annotation, **When** `queryContainers` is called, **Then** both the walk and the annotation are returned.
2. **Given** a `GraphClassifier` and a `GraphQuery`, **When** `queryAnnotationsOf` is called for a node, **Then** only annotation-classified containers are returned.
3. **Given** two nodes that share a common walk, **When** `queryCoMembers` is called with one node and the walk as container, **Then** the other node is returned.

---

### User Story 5 - Backward-Compatible GraphLens Algorithms (Priority: P5)

A developer using the existing `bfs`, `findPath`, or `connectedComponents` functions on `GraphLens` makes no changes to their code after this feature is introduced. The functions continue to work as before, now implemented as wrappers over the new algorithm module.

**Why this priority**: Preserving backward compatibility is a constraint, not a new capability. It must hold but is not a primary user goal.

**Independent Test**: Can be fully tested by running the existing test suite for `GraphLens` algorithms without modification and verifying all tests pass.

**Acceptance Scenarios**:

1. **Given** existing code calling `bfs lens startNode`, **When** the feature is deployed, **Then** the call compiles and returns the same result as before.
2. **Given** existing code calling `connectedComponents lens`, **When** the feature is deployed, **Then** results are identical to the pre-feature implementation.

---

### Edge Cases

- What happens when `queryNodeById` or `queryRelationshipById` is called with an identifier that does not exist in the graph? The result must be `Nothing`, not an error.
- What happens when `shortestPath` is called between two nodes with no path (given the supplied `TraversalWeight`)? The result must be `Nothing`.
- What happens when `topologicalSort` is called on a graph with a cycle? The result must be `Nothing` (or an explicit failure value), not an infinite loop.
- What happens when `frameQuery` produces an empty graph (no nodes match the predicate)? Algorithms must terminate correctly and return empty results.
- What happens when `queryContainers` is called on an element that belongs to no containers? An empty list is returned.
- What happens when a custom `TraversalWeight` returns infinity for all relationships from a node? That node is treated as a dead end; traversal does not proceed through it.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The library MUST provide a `GraphQuery v` type that abstracts graph traversal and lookup as a record of functions, independent of any specific graph representation.
- **FR-002**: `GraphQuery v` MUST include fields for: enumerating all nodes, enumerating all relationships, finding incident relationships for a node, finding the source of a relationship, finding the target of a relationship, computing the degree of a node, looking up a node by identifier, looking up a relationship by identifier, and finding all containers of an element.
- **FR-003**: The library MUST provide a `TraversalWeight v` type that encodes both traversal direction and edge weight as a function from a relationship and direction to a numeric cost.
- **FR-004**: The library MUST provide canonical `TraversalWeight` values: `undirected` (uniform cost, direction ignored), `directed` (forward only), and `directedReverse` (backward only).
- **FR-005**: The library MUST provide `fromGraphLens` to construct a `GraphQuery v` from a `GraphLens v`.
- **FR-006**: The library MUST provide `fromPatternGraph` to construct a `GraphQuery v` directly from a `PatternGraph v`, without going through `GraphLens`.
- **FR-007**: The library MUST provide a `Pattern.Graph.Algorithms` module with graph algorithms expressed as functions over `GraphQuery v` and `TraversalWeight v`, including: `bfs`, `dfs`, `shortestPath`, `hasPath`, `allPaths`, `isNeighbor`, `isConnected`, `connectedComponents`, `minimumSpanningTree`, `degreeCentrality`, and `betweennessCentrality`. `topologicalSort` and `hasCycle` MUST also be provided but do NOT accept a `TraversalWeight` parameter — they operate on the directed structure implied by relationship endpoint order (source → target).
- **FR-008**: The library MUST provide `frameQuery` to construct a filtered subgraph view as a `GraphQuery v -> GraphQuery v` transformation.
- **FR-009**: The library MUST provide `memoizeIncidentRels` as a `GraphQuery v -> GraphQuery v` transformation that caches incident relationship lookups.
- **FR-010**: The library MUST provide context query helpers `queryAnnotationsOf`, `queryWalksContaining`, and `queryCoMembers` as derived functions built on `GraphQuery` primitives.
- **FR-011**: The existing `bfs`, `findPath`, and `connectedComponents` functions on `GraphLens` MUST be retained as backward-compatible wrappers that delegate to the new `Algorithms` module with `undirected` as the default `TraversalWeight`.
- **FR-012**: `PatternGraph.toGraphLens` MUST be retired; `fromPatternGraph` supersedes it for algorithm access. *Implemented as removal of `toGraphLens` and `toGraphLensWithScope` in this feature (breaking change); migration path is `fromPatternGraph`. See research.md Decision 7.*
- **FR-013**: All traversal algorithms MUST accept a `TraversalWeight v` parameter so that direction and weight are determined at the call site, not embedded in the graph structure.

### Key Entities

- **`GraphQuery v`**: A record of traversal and lookup functions that abstracts over any graph representation. Parameterized by the graph value type `v`. Provides upward traversal via `queryContainers` as well as downward traversal via incident relationship and endpoint accessors.
- **`TraversalWeight v`**: A function from a relationship pattern and traversal direction to a numeric cost. Encodes both directionality and edge weight. Supplied by callers to traversal algorithms; not embedded in the graph structure.
- **`TraversalDirection`**: An enumeration (`Forward` | `Backward`) indicating which direction along a relationship is being considered during traversal.
- **Graph Algorithm**: A pure function in `Pattern.Graph.Algorithms` that accepts a `GraphQuery v` (and for traversal algorithms, a `TraversalWeight v`) and returns a result. Algorithms have no dependency on any specific graph representation.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All graph algorithms (`bfs`, `dfs`, `shortestPath`, `hasPath`, `allPaths`, `isNeighbor`, `isConnected`, `connectedComponents`, `topologicalSort`, `hasCycle`, `minimumSpanningTree`, `degreeCentrality`, `betweennessCentrality`) produce correct results when given a `GraphQuery` derived from either `GraphLens` or `PatternGraph`.
- **SC-002**: All existing tests for `GraphLens`-based algorithms pass without modification after the feature is introduced.
- **SC-003**: A `GraphQuery` derived from `PatternGraph` produces the same algorithm results as a `GraphQuery` derived from an equivalent `GraphLens`, for all implemented algorithms.
- **SC-004**: Calling the same traversal algorithm with `directed` and `undirected` `TraversalWeight` values on a graph containing directed relationships produces demonstrably different results, confirming traversal policy is correctly applied.
- **SC-005**: `frameQuery` correctly restricts node and relationship enumeration so that algorithms operating on a framed view never access elements outside the frame.
- **SC-006**: `queryContainers` returns all and only the higher-order structures (walks, annotations, relationships) that directly contain a given element, with no false positives or false negatives.
- **SC-007**: The `GraphQuery` interface can be implemented for a custom graph source (e.g., a hand-constructed record) and all algorithms operate correctly against it, confirming representation independence.

## Assumptions

- `GraphValue v` (providing identity extraction via `identify`) is a prerequisite typeclass already available in the codebase; `GraphQuery` depends on it but does not extend it.
- `GraphClassifier` (from feature 034) is available and stable before this feature is implemented, as context query helpers depend on it for category filtering.
- Performance benchmarking of the record-of-functions approach against a typeclass-based equivalent is treated as an implementation concern, not a specification requirement. The specification requires correct behavior; performance optimization (`INLINE`, `UNPACK` pragmas, bulk adjacency) is deferred to the planning phase.
- The `Pattern.Graph.Algorithms` module is initially a single module. Subdivision into sub-modules (`Algorithms.Path`, `Algorithms.Centrality`, etc.) is deferred until the algorithm surface grows large enough to warrant it.
- Centrality algorithm signatures (betweenness, closeness, PageRank) are representative of the category. Exact tuning parameters (convergence tolerance, damping factor) are determined during implementation, not specified here.
