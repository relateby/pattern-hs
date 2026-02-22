# Feature Specification: Graph Transform

**Feature Branch**: `036-graph-transform`  
**Created**: 2026-02-21  
**Status**: Draft  
**Input**: User description: "graph-transform to add GraphView, Construction, Transformation, and Context-Aware Mapping as described in the proposal @proposals/graph-transform.md"

## User Scenarios & Testing *(mandatory)*

## Clarifications

### Session 2026-02-21
- Q: The data-model statement "Folds structurally down topological sorts of the internal DAG." implies `paraGraph` requires a total ordering of the graph. Could the total ordering depend on grouping patterns by increasing arity, then ordering by v within each group (i.e. pattern-centric bottom-up execution)? → A: Yes, we adopt a pattern-centric, bottom-up structural folding execution model rather than attempting topological graph sorts.

### User Story 1 - Constructing a Graph from External Data (Priority: P1)

A pipeline author needs to turn non-graph external data (like CSV rows or JSON documents) into a full graph without an impedance mismatch, maintaining a graph-shaped model from ingestion to output.

**Why this priority**: Essential for bootstrapping any graph processing pipeline where the source is not already a graph. It is the entry point for real-world ETL.

**Independent Test**: Can be independently tested by feeding nested, non-graph records into the construction functions and verifying that the resulting structures form a merged, coherent graph.

**Acceptance Scenarios**:

1. **Given** a collection of structured, non-graph data records, **When** the pipeline author expands them and batch-merges the results, **Then** the result is a unified graph where overlapping entities are correctly reconciled.
2. **Given** a recursive document hierarchy, **When** the pipeline author expands it, **Then** the nested structure is successfully captured as an interconnected set of graph elements.

---

### User Story 2 - Composable Bulk Graph Transformations (Priority: P1)

A pipeline author needs to transform graph elements (e.g., nodes, relationships, walks) efficiently and compose multiple transformations (map, filter, fold) without creating intermediate copies of the graph structure.

**Why this priority**: Bulk transformations form the core of most data-cleaning and reshaping tasks within pipelines.

**Independent Test**: Can be fully tested by applying successive map and filter operations on a large graph and verifying that changes compose properly and elements are mapped according to their category.

**Acceptance Scenarios**:

1. **Given** a graph representing raw entities, **When** a pipeline author maps over specific entity categories with transformations, **Then** only the targeted categories are updated while everything else remains intact.
2. **Given** a graph with invalid elements, **When** a filter operation is applied, **Then** the invalid elements are discarded, and any containers holding them are adjusted according to the specified substitution policy.
3. **Given** multiple sequential graph transformations, **When** the pipeline executes them, **Then** all operations compose into a single view without materializing intermediate states.

---

### User Story 3 - Context-Aware Data Enrichment (Priority: P2)

A data scientist needs to compute metrics for individual nodes (e.g., counting incident relationships, checking annotation presence) based on their surrounding graph context, without losing deterministic transformation properties.

**Why this priority**: Enables advanced feature engineering, allowing nodes to be enriched with topological context before further processing or export.

**Independent Test**: Can be fully tested by applying transformations where an element's new value relies on querying its neighbors from the original graph.

**Acceptance Scenarios**:

1. **Given** a graph with unannotated nodes, **When** an enrichment mapping is run, **Then** each node is updated with a count of its incident edges from the original snapshot.
2. **Given** a transformation modifying graph structure, **When** a context-aware mapping is running concurrently on elements, **Then** the context map relies strictly on the original graph snapshot, ensuring deterministic behavior.

---

### Iterative Topology-Aware Algorithms (Priority: P3)

A data scientist needs to compute iterative algorithms (like PageRank or belief propagation) across a graph topology until the results converge, relying on message passing between neighboring elements.

**Why this priority**: Crucial for analytical algorithms and knowledge graph refinement tasks, but relies on the foundation built by the core transformation features.

**Independent Test**: Can be independently tested by implementing a basic label propagation algorithm and ensuring it converges accurately over a cyclic graph.

**Acceptance Scenarios**:

1. **Given** a graph with initial values on nodes, **When** a topology-aware folding operation is executed on a DAG, **Then** results compute based on pattern-centric bottom-up structural dependencies.
2. **Given** a cyclic graph with convergence criteria, **When** a fixpoint topology fold is executed, **Then** the process iterates bottom-up structural rounds until the user-supplied threshold is met and terminates safely.

### Edge Cases

- What happens when filtering removes an element that belongs to a critical graph structure (e.g., a walk gap)? The system must determine if a substitution fills the gap or if the walk becomes segmented.
- How does the system handle floating-point calculations for topology-folding convergence? Exact equality checks are risky; the user-supplied predicate must robustly handle precision limits.
- What happens when seed data expansion results in massive intermediate node creation before batch merging? Performance edge cases in memory consumption should be handled by efficient deferred operations.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST provide a unified abstraction over any graph-like source (database, raw text, programmatic structs) that pairs traversal queries with classified element enumeration.
- **FR-002**: The system MUST allow explicit materialization of the unified abstraction into concrete storage.
- **FR-003**: The system MUST enable recursive construction of graph elements from non-graph seed data.
- **FR-004**: The system MUST allow building full graphs by expanding collections of non-graph seeds and batch-merging the results according to reconciliation policies.
- **FR-005**: The system MUST support mapping over elements grouped by their specific categories (e.g., distinct transformations for nodes vs relationships).
- **FR-006**: The system MUST support filtering graph elements and predictably handle the containers (e.g., walks) of removed elements using user-supplied substitution rules.
- **FR-007**: The system MUST support folding across all graph categories to yield aggregate summaries.
- **FR-008**: The system MUST allow mapping over elements where the transformation function can query the original graph context (snapshot semantics).
- **FR-009**: The system MUST provide topology-aware folding where computed results are evaluated pattern-centrically, ordered bottom-up by structural depth (e.g., atomic nodes, then relationships, then walks).
- **FR-010**: The system MUST support iterative fixpoint folding over graph topologies, driven by user-supplied convergence criteria, running successive structural bottom-up rounds.

### Key Entities

- **GraphView**: A lightweight pairing of a traversal capability with categorized element enumeration; acts as the universal graph interface and entry point for transformation pipelines.
- **Substitution**: A policy object indicating how containers (like walks or relationships) should adapt when their internal elements are filtered out.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Pipeline authors can chain 5 or more map/filter transformations without triggering intermediate graph copies in memory.
- **SC-002**: A data pipeline can convert non-graph JSON structures into a unified, queryable graph using the provided expansion functions.
- **SC-003**: Data scientists can implement a multi-pass algorithm like PageRank using only the provided topology folding operations, with results matching known-good implementations.
- **SC-004**: Execution of context-aware maps is fully deterministic, meaning the output graph is exactly identical regardless of the order in which elements are processed.
