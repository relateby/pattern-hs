# Feature Specification: PatternGraph Data Structure

**Feature Branch**: `033-pattern-graph`  
**Created**: 2026-02-18  
**Status**: Draft  
**Input**: User description: "PatternGraph data structure backed by Pattern v as described in proposals/pattern-graph.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Round-trip gram file via a graph container (Priority: P1)

A user parses a gram file into a set of pattern elements, loads them into a single graph container, then queries or modifies that graph and serializes it back to a gram file. The container owns the data and keeps nodes, relationships, walks, and annotations internally consistent so the user does not have to manage a separate interpretive layer.

**Why this priority**: This is the core workflow the feature exists to support; without it there is no value.

**Independent Test**: Can be fully tested by parsing a small gram file into the graph, making a change (e.g., add a node or relationship), serializing back, and verifying the output matches expectations. Delivers a single, concrete place to hold and manipulate gram graph data.

**Acceptance Scenarios**:

1. **Given** a valid gram file containing nodes and relationships, **When** the user parses it and loads the result into the graph container, **Then** all nodes and relationships are stored and addressable by identity.
2. **Given** a populated graph container, **When** the user adds a new element that references existing nodes, **Then** the new element is stored and the graph remains consistent (e.g., referenced nodes exist).
3. **Given** a populated graph container, **When** the user serializes it back to gram notation, **Then** the output can be parsed again and reproduces the same logical graph.

---

### User Story 2 - Merge-on-insert semantics (Priority: P2)

A user adds pattern elements one at a time (or in batch). When an element with an existing identity is added, the system reconciles it with the existing entry using a defined policy (merge), rather than overwriting or duplicating. This matches familiar “merge” semantics from graph query languages and keeps the graph deduplicated and consistent.

**Why this priority**: Merge semantics are essential for building the graph from multiple sources or repeated inserts without manual deduplication.

**Independent Test**: Can be tested by inserting the same logical node twice and verifying a single stored entry with reconciled content, and by inserting a walk and verifying its component relationships and nodes are also merged.

**Acceptance Scenarios**:

1. **Given** an empty graph container, **When** the user adds a node with identity X and then adds another node with the same identity X, **Then** there is one stored node for X, with content reconciled per the defined policy.
2. **Given** an empty graph container, **When** the user adds a walk that references relationships and nodes, **Then** the walk is stored as a first-class element and its component relationships and nodes are also merged into the container so the graph is fully navigable.
3. **Given** a graph containing a node, **When** the user adds an annotation that attaches to that node, **Then** the annotation is stored under its own identity and the annotated node (or its identity) remains consistent and discoverable.

---

### User Story 3 - Clear handling of unrecognized or out-of-scope patterns (Priority: P3)

A user loads a mix of pattern elements, some of which are valid graph elements (nodes, relationships, walks, annotations) and some of which do not fit those categories. The system does not silently drop the unrecognized ones; it signals their presence so the caller can decide whether to skip, fail, or handle them specially.

**Why this priority**: Predictable behavior and visibility into bad or unexpected input reduce support burden and integration errors.

**Independent Test**: Can be tested by feeding a set that includes both recognized and unrecognized pattern shapes and verifying that recognized elements are merged into the graph and unrecognized ones are reported (e.g., via a result wrapper or accumulated warnings).

**Acceptance Scenarios**:

1. **Given** a set of pattern elements that includes both node/relationship/walk/annotation shapes and other shapes, **When** the user loads them into the graph container, **Then** recognized elements are merged and unrecognized elements are reported (e.g., in a list or side channel), not silently dropped.
2. **Given** a report of unrecognized elements, **When** the user inspects it, **Then** they can identify which inputs were not merged and act accordingly (skip, error, or custom handling).

---

### User Story 4 - Interoperability with existing graph view (Priority: P3)

A user who has built a graph container needs to run existing graph algorithms or queries that operate on the existing interpretive graph view. The system allows the graph container to be converted into that view so all existing capabilities (e.g., traversal, path finding) work without duplicating data or logic.

**Why this priority**: Enables reuse of existing graph tooling and keeps the graph container as the single source of truth for the common round-trip workflow.

**Independent Test**: Can be tested by building a graph container from a gram file, converting it to the existing graph view, and running an existing query or algorithm; results must be consistent with the data in the container.

**Acceptance Scenarios**:

1. **Given** a populated graph container, **When** the user converts it to the existing graph view, **Then** all nodes, relationships, walks, and annotations present in the container are visible and queryable in that view.
2. **Given** a graph container and its graph view, **When** the user runs an existing graph algorithm on the view, **Then** the outcome is consistent with the structure stored in the container (no missing or extra elements).

---

### Edge Cases

- What happens when the same identity appears in different roles (e.g., as both a node and a relationship)? The system must classify each pattern by shape and store it in exactly one category; identity collisions across categories are handled by the same reconciliation rules as within a category, or explicitly disallowed and reported.
- How does the system handle annotations that reference the same underlying element? Each annotation has its own identity and is stored separately; the underlying element is merged once so that multiple annotations can attach to the same node or relationship without data collision.
- What happens when merge encounters conflicting or duplicate content for the same identity? Reconciliation follows the defined policy (e.g., from existing pattern-reconciliation work); the exact policy is out of scope of this spec but must be applied consistently for all merge operations.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST provide a graph container that stores four categories of elements: nodes, relationships, walks, and annotations, each addressable by a stable identity.
- **FR-002**: The system MUST support loading pattern elements into the container via a merge operation that classifies each element (node, relationship, walk, or annotation), stores it in the correct category, and reconciles with any existing entry for the same identity using a defined reconciliation policy.
- **FR-003**: The system MUST support building a graph from a list of pattern elements by repeatedly applying merge (e.g., in a single batch operation) so that the resulting container is internally consistent (e.g., relationships reference existing nodes, walks reference existing relationships).
- **FR-004**: When merging a walk, the system MUST store the walk as a first-class element and MUST also merge its component relationships and nodes so that the graph can be navigated both by walks and by individual relationships and nodes.
- **FR-005**: When merging an annotation, the system MUST store the annotation under its own identity and MUST merge its single inner element into the appropriate category so that multiple annotations can attach to the same underlying element without overwriting each other.
- **FR-006**: The system MUST NOT silently drop pattern elements that do not classify as node, relationship, walk, or annotation; it MUST signal their presence (e.g., via a result wrapper or accumulated warnings) so the caller can skip, error, or handle them.
- **FR-007**: The system MUST support converting the graph container to the existing interpretive graph view so that existing graph algorithms and queries can be run on the same data without duplication.
- **FR-008**: Classification of pattern elements (node vs relationship vs walk vs annotation vs unrecognized) MUST be defined in one place and applied consistently for all merge operations; the container MUST NOT allow the same identity to be stored in more than one category for the same logical role.

### Key Entities

- **Graph container**: The primary data structure that holds nodes, relationships, walks, and annotations, each keyed by identity. It is the single owning representation for the round-trip workflow.
- **Node**: A graph element with zero “arity” in the proposal’s classification; represents a single entity in the gram model.
- **Relationship**: A graph element linking two nodes; classified by having two node-shaped elements.
- **Walk**: An ordered sequence of relationships; stored as its own element while its component relationships and nodes are also merged.
- **Annotation**: An element that attaches metadata or labels to another element; stored with its own identity; its inner element is merged so the underlying node or relationship is shared.
- **Identity**: A stable key for each element; used to deduplicate and reconcile on merge. Provided by the value model (e.g., symbol or other identifier).
- **Interpretive graph view**: The existing read-only view over graph-like data used by existing algorithms; the container must be convertible to this view.

## Assumptions

- A reconciliation policy for merging duplicate identities already exists or will be defined elsewhere (e.g., existing pattern-reconciliation work); this feature uses it consistently for all merge operations.
- The existing interpretive graph view and its query/algorithm capabilities are unchanged; this feature only adds a container and a way to convert the container into that view.
- Identity and classification of pattern elements are defined by the value model (e.g., symbol-based identity); the graph container relies on that contract and does not define it.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A user can complete the round-trip (parse gram file → load into graph container → modify → serialize) for a file containing nodes, relationships, and at least one walk or annotation, and obtain output that parses back to an equivalent graph.
- **SC-002**: Merge operations for the same identity produce a single stored element per category; duplicate identities do not create duplicate entries when merged.
- **SC-003**: Unrecognized pattern elements are reported in every case they are encountered during load; zero unrecognized elements are silently dropped.
- **SC-004**: A graph container built from a given set of pattern elements can be converted to the existing graph view and produce the same query results as if the data had been supplied directly to that view.
- **SC-005**: Adding a walk to the container results in all of its component relationships and nodes being present and navigable; adding an annotation results in the annotated element being present and discoverable without data collision with other annotations.
