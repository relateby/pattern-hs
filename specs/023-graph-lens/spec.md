# Feature Specification: Graph Lens Implementation

**Feature Branch**: `023-graph-lens`  
**Created**: 2025-01-27  
**Status**: Ready for Planning  
**Input**: User description: "Implement the graph lens as decribed in the design document at @design/graph-lens.md and using the analysis provided in @022-graph-lens-review"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Create Graph Lens and Identify Nodes (Priority: P1)

A developer needs to create a GraphLens from a Pattern and identify which elements are nodes according to a predicate, enabling basic graph interpretation of Pattern structures.

**Why this priority**: This is the foundational capability - without the ability to create a lens and identify nodes, no graph operations are possible. This delivers immediate value by enabling graph interpretation of existing Pattern data.

**Independent Test**: Can be fully tested by creating a GraphLens with a simple predicate (e.g., atomic patterns as nodes), applying it to a Pattern, and verifying that nodes are correctly identified. This provides a complete, testable unit of functionality.

**Acceptance Scenarios**:

1. **Given** a Pattern with atomic and composite elements, **When** a GraphLens is created with a predicate identifying atomic patterns as nodes, **Then** the `nodes` function returns only atomic patterns from the scope
2. **Given** a Pattern with elements of different types, **When** a GraphLens is created with a value-based predicate (e.g., patterns with specific value prefix), **Then** the `nodes` function returns only patterns matching the predicate
3. **Given** a GraphLens and a Pattern, **When** `isNode` is called on each direct element of the scope, **Then** it returns True only for elements that satisfy the predicate
4. **Given** an empty Pattern as scope, **When** a GraphLens is created and nodes are queried, **Then** an empty list is returned

---

### User Story 2 - Identify and Query Relationships (Priority: P1)

A developer needs to identify relationships (non-node patterns connecting exactly two nodes) and query them from a GraphLens, enabling graph structure analysis.

**Why this priority**: Relationships are fundamental to graph structure. This capability enables understanding connections between nodes and is required for navigation and analysis operations. This builds directly on User Story 1.

**Independent Test**: Can be fully tested by creating a GraphLens with nodes identified, providing Patterns that connect two nodes, and verifying that relationships are correctly identified and queried. This provides complete relationship functionality.

**Acceptance Scenarios**:

1. **Given** a GraphLens with identified nodes, **When** a Pattern contains exactly two node elements, **Then** `isRelationship` returns True and the pattern appears in `relationships`
2. **Given** a GraphLens and a relationship pattern, **When** `source` and `target` are called, **Then** they return the first and second node elements respectively
3. **Given** a Pattern with more than two node elements, **When** `isRelationship` is called, **Then** it returns False (not a relationship, potentially a hyperedge)
4. **Given** a Pattern with two elements where one is not a node, **When** `isRelationship` is called, **Then** it returns False
5. **Given** a GraphLens with no relationships in scope, **When** `relationships` is called, **Then** an empty list is returned

---

### User Story 3 - Navigate Graph Structure (Priority: P2)

A developer needs to navigate graph structure by finding neighbors of nodes, incident relationships, and computing node degrees, enabling graph exploration.

**Why this priority**: Navigation is essential for graph exploration and analysis. This enables users to traverse graph structure and understand connectivity, building on the foundation of nodes and relationships.

**Independent Test**: Can be fully tested by creating a GraphLens with nodes and relationships, selecting a node, and verifying that neighbors, incident relationships, and degree are correctly computed. This provides complete navigation functionality.

**Acceptance Scenarios**:

1. **Given** a GraphLens with nodes and relationships, **When** `neighbors` is called on a node, **Then** it returns all nodes connected via relationships (both as source and target)
2. **Given** a GraphLens and a node, **When** `incidentRels` is called, **Then** it returns all relationships where the node is either source or target
3. **Given** a GraphLens and a node, **When** `degree` is called, **Then** it returns the count of incident relationships
4. **Given** an isolated node (no relationships), **When** `neighbors`, `incidentRels`, and `degree` are called, **Then** they return empty list, empty list, and 0 respectively
5. **Given** a node with multiple relationships, **When** `degree` is called, **Then** it returns the total count of all incident relationships

---

### User Story 4 - Identify and Analyze Walks (Priority: P2)

A developer needs to identify walks (sequences of consecutively connected relationships) and extract nodes from walks, enabling path analysis in graphs.

**Why this priority**: Walks enable path analysis and are a natural extension of relationships. This supports understanding multi-hop connections and path-based queries.

**Independent Test**: Can be fully tested by creating a GraphLens with relationships, providing a Pattern containing consecutively connected relationships, and verifying that walks are identified and nodes can be extracted. This provides complete walk functionality.

**Acceptance Scenarios**:

1. **Given** a GraphLens with relationships, **When** a Pattern contains relationships that are consecutively connected (target of one equals source of next), **Then** `isWalk` returns True and the pattern appears in `walks`
2. **Given** a valid walk pattern, **When** `walkNodes` is called, **Then** it returns nodes in traversal order (source of first relationship, then targets of subsequent relationships)
3. **Given** a Pattern with relationships that are not consecutively connected, **When** `isWalk` is called, **Then** it returns False
4. **Given** a Pattern containing non-relationship elements, **When** `isWalk` is called, **Then** it returns False
5. **Given** an empty walk pattern, **When** `walkNodes` is called, **Then** an empty list is returned

---

### User Story 5 - Analyze Graph Connectivity (Priority: P3)

A developer needs to analyze graph connectivity by finding connected components, performing breadth-first search, and finding paths between nodes, enabling comprehensive graph analysis.

**Why this priority**: Connectivity analysis is advanced functionality that enables understanding graph structure at scale. This builds on all previous capabilities and provides powerful analysis tools.

**Independent Test**: Can be fully tested by creating a GraphLens with a graph containing multiple connected components, and verifying that connected components are correctly identified, BFS works correctly, and paths can be found between connected nodes. This provides complete connectivity analysis.

**Acceptance Scenarios**:

1. **Given** a GraphLens with a graph containing multiple disconnected subgraphs, **When** `connectedComponents` is called, **Then** it returns separate lists of nodes for each component
2. **Given** a GraphLens and a starting node, **When** `bfs` is called, **Then** it returns all nodes reachable from the starting node via relationships
3. **Given** a GraphLens with two connected nodes, **When** `findPath` is called with start and end nodes, **Then** it returns a path (list of nodes) connecting them
4. **Given** a GraphLens with two disconnected nodes, **When** `findPath` is called, **Then** it returns Nothing
5. **Given** a GraphLens with a single isolated node, **When** `connectedComponents` is called, **Then** it returns a list containing a single list with that node

---

### Edge Cases

- What happens when `scopePattern` is empty (no elements)?
- What happens when `isNode` predicate returns False for all elements (no nodes)?
- What happens when relationships have incorrect structure (not exactly 2 elements)?
- What happens when a relationship's elements are not nodes according to the lens?
- What happens when `source` or `target` is called on a non-relationship pattern?
- What happens when navigation functions are called on a node not in the lens scope?
- What happens when walks contain relationships from different lenses or scopes?
- What happens when `findPath` is called with the same node as start and end?
- What happens when graph operations are performed on very large Patterns (performance considerations)?
- What happens when predicate functions have side effects or are impure?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a `GraphLens` data structure with `scopePattern` and `isNode` predicate fields
- **FR-002**: System MUST provide a `nodes` function that returns all direct elements of `scopePattern` satisfying the `isNode` predicate
- **FR-003**: System MUST provide an `isNode` function (taking lens context) that determines if a Pattern is a node according to the lens
- **FR-004**: System MUST provide an `isRelationship` function that identifies patterns with exactly two node elements
- **FR-005**: System MUST provide a `relationships` function that returns all relationship patterns from the scope
- **FR-006**: System MUST provide `source` and `target` functions that extract the first and second node elements from relationships
- **FR-007**: System MUST provide a `reverseRel` function that reverses the direction of a relationship pattern
- **FR-008**: System MUST provide a `neighbors` function that returns all nodes connected to a given node via relationships
- **FR-009**: System MUST provide an `incidentRels` function that returns all relationships involving a given node
- **FR-010**: System MUST provide a `degree` function that returns the count of incident relationships for a node
- **FR-011**: System MUST provide an `isWalk` function that identifies patterns containing consecutively connected relationships
- **FR-012**: System MUST provide a `walks` function that returns all walk patterns from the scope
- **FR-013**: System MUST provide a `walkNodes` function that extracts nodes from a walk in traversal order
- **FR-014**: System MUST provide a `connectedComponents` function that identifies all disconnected subgraphs
- **FR-015**: System MUST provide a `bfs` function that performs breadth-first search from a starting node
- **FR-016**: System MUST provide a `findPath` function that finds a path between two nodes if one exists
- **FR-017**: All graph operations MUST only consider direct elements of `scopePattern`, never descending into nested structures
- **FR-018**: All graph predicates MUST be pure functions (no side effects, deterministic results)
- **FR-019**: System MUST handle edge cases gracefully (empty patterns, no nodes, invalid structures) without errors
- **FR-020**: System MUST support multiple lenses on the same Pattern, enabling different graph interpretations

### Key Entities

- **GraphLens**: A data structure containing a `scopePattern` (defining graph boundaries) and an `isNode` predicate (determining which elements are nodes). All graph concepts derive from this single predicate.
- **Node**: A direct element of `scopePattern` that satisfies the `isNode` predicate. Nodes are the fundamental graph elements.
- **Relationship**: A non-node pattern with exactly two node elements. Relationships connect nodes in the graph.
- **Walk**: A non-node pattern whose elements are all relationships, where consecutive relationships share nodes (target of one equals source of next).
- **Scope Pattern**: The Pattern that defines the boundary for all graph operations. Only direct elements are considered, never nested structures.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can create a GraphLens and identify nodes from a Pattern with 100+ elements in under 100 milliseconds
- **SC-002**: Developers can query all relationships from a GraphLens with 50+ relationships in under 50 milliseconds
- **SC-003**: Developers can find neighbors of a node in a graph with 1000+ nodes in under 200 milliseconds
- **SC-004**: Developers can identify walks in a Pattern containing 20+ relationships in under 100 milliseconds
- **SC-005**: Developers can find connected components in a graph with 500+ nodes and 1000+ relationships in under 1 second
- **SC-006**: Developers can find a path between two nodes in a connected graph with 200+ nodes in under 500 milliseconds
- **SC-007**: All graph operations correctly handle edge cases (empty patterns, no nodes, invalid structures) without runtime errors
- **SC-008**: Multiple lenses can be applied to the same Pattern, producing different but consistent graph interpretations
- **SC-009**: Graph operations maintain mathematical correctness (relationships connect nodes, walks are valid sequences, paths are reachable)

## Assumptions

- The Pattern library (`libs/pattern`) provides the `Pattern v` data structure with `value` and `elements` fields as described in the design document
- Pattern equality (`Eq` instance) is available for comparing nodes and relationships
- The `isNode` predicate provided by users is a pure function with no side effects
- Users understand that graph operations are scope-bounded (only direct elements of `scopePattern` are considered)
- Performance requirements are reasonable for in-memory graph operations (no distributed or persistent graph requirements)
- The implementation follows the design document's mathematical definitions for nodes, relationships, and walks
- Edge cases (empty patterns, no nodes) are handled gracefully with empty results rather than errors

## Dependencies

- **Pattern Library**: Requires `libs/pattern` with `Pattern v` data structure, `Eq` instance, and basic query functions
- **Design Document**: Implementation must follow the design specified in `design/graph-lens.md`
- **Analysis Recommendations**: Implementation should consider recommendations from `specs/022-graph-lens-review/analysis.md`
- **Standard Libraries**: Requires standard Haskell libraries for collections (Set, Map) for efficient graph operations

## Out of Scope

- Lens composition (combining multiple lenses) - explicitly deferred in design document
- Indexed lens variant for performance optimization - mentioned as future work
- Temporal graph extensions - mentioned as future consideration
- Integration with Pattern Matching DSL - depends on Feature 14 which is deferred
- Graph visualization or rendering - UI concerns not part of core library
- Persistent graph storage - data persistence is separate concern
- Graph algorithms beyond basic connectivity (shortest path, minimum spanning tree, etc.) - can be added later if needed
- Hypergraph support beyond basic relationship definition - relationships are strictly binary in this implementation
