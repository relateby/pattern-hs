# Feature Specification: Graph Classifier

**Feature Branch**: `034-graph-classifier`  
**Created**: 2026-02-20  
**Status**: Draft  
**Input**: User description: "Graph classifier as a unified, extensible graph view for patterns described in @proposals/graph-classifier.md"

## Clarifications

### Session 2026-02-20

- Q: Should the system attempt to normalize/reshape patterns to fit a structural category during classification, or require data patterns to be structurally valid for the target category? → A: Require data patterns to be structurally valid for the target category.
- Q: How should custom domain categories be stored in the `pgOther` map? → A: Store as `Map (Id v) (extra, Pattern v)` to keep the specific `extra` classification tag paired with the pattern.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Standard Graph Construction (Priority: P1)

As a data modeler, I want to construct a graph from pattern data using standard built-in rules, so that I don't have to define complex classification logic for common use cases.

**Why this priority**: The standard built-in rules are the default and most common way users interact with patterns as graphs. This core flow must work out of the box.

**Independent Test**: Can be fully tested by taking a set of standard data patterns, processing them with the default classifier, and verifying that items are correctly categorized into basic graph elements (nodes, relationships, walks, annotations).

**Acceptance Scenarios**:

1. **Given** a standard dataset of connected elements, **When** constructing a graph using the default rules, **Then** standalone items are categorized as nodes and connected items are categorized as relationships, walks, or annotations.
2. **Given** a graph constructed with default rules, **When** querying the elements, **Then** no elements are rejected during construction and all are available in their respective structural categories.

---

### User Story 2 - Custom Classification Rules (Priority: P1)

As an advanced user, I want to provide custom rules when viewing or storing a graph, so that I can map patterns to my domain-specific structures.

**Why this priority**: Extensibility is the core architectural goal of this feature, allowing users to move beyond the fixed default rules.

**Independent Test**: Can be fully tested by defining a custom rule set with novel domain categories and verifying that the resulting graph correctly assigns data elements to those custom categories.

**Acceptance Scenarios**:

1. **Given** a custom rule set with domain-specific categories, **When** constructing a graph view, **Then** the resulting graph organizes elements according to the custom rules.
2. **Given** an interactive query session, **When** applying a custom rule set on-the-fly, **Then** elements are categorized dynamically based on the provided rules.

---

### User Story 3 - Legacy Compatibility (Priority: P2)

As an existing user of the previous on-demand graph views, I want my current rule sets to continue working without changing my application setup, so that upgrading is seamless.

**Why this priority**: Preserving backwards compatibility for widely used existing workflows minimizes disruption while migrating to the new underlying architecture.

**Independent Test**: Can be fully tested by running existing workflows with binary (node/not-node) rules against the new engine and verifying identical behavior.

**Acceptance Scenarios**:

1. **Given** an existing setup with a simple custom rule for identifying nodes, **When** interacting with the graph, **Then** elements are categorized identically to the previous implementation.
2. **Given** a legacy view instance, **When** performing structural queries (like finding neighbors or walks), **Then** the correct results are returned without exposing the new internal multi-category model.

### Edge Cases

- What happens when a data pattern fundamentally conflicts with a custom rule set's assumptions? (The system must safely place it in an extensible "other" category rather than failing or rejecting the element).
- **Validation**: Does the system attempt to normalize/reshape patterns to fit a structural category? No, patterns must be structurally valid for the target category at construction time. If not, they fall back to the "Other" category or remain in their literal structural shape.
- How does the system handle extremely large or deeply nested datasets during on-demand classification? (The evaluation logic must process without degrading system stability, potentially utilizing caching).
- How are previously "unrecognized" or malformed elements handled now? (They are retained in a designated catch-all category rather than being silently dropped or causing errors).

## Requirements *(mandatory)*

### Dependencies & Assumptions

- **Assumptions**:
  - The default rule set will perfectly mirror the logic of the legacy built-in rules for atomic/connected elements.
  - The system has sufficient memory to cache on-demand evaluations if lazy evaluation proves computationally expensive.
- **Dependencies**:
  - Requires the existing data pattern infrastructure to be in place.

### Functional Requirements

- **FR-001**: The system MUST define a structural vocabulary comprising exactly five primary categories: Node, Relationship, Annotation, Walk, and an extensible "Other" category.
- **FR-002**: The system MUST encapsulate classification rules as independent configurations that can be plugged in or swapped at runtime.
- **FR-003**: The system MUST provide a default rule set that implements standard structural categorization.
- **FR-004**: The system MUST allow users to define arbitrary sub-categories within the extensible "Other" category for domain-specific needs.
- **FR-005**: The system MUST support applying classification rules both eagerly (during upfront storage) and lazily (evaluated on-demand during queries).
- **FR-006**: The system MUST ensure backward compatibility with existing binary classification workflows, preserving existing interfaces and behaviors.
- **FR-007**: The system MUST guarantee that no data elements are rejected or lost during the categorization process; all inputs MUST be assigned a valid category.
- **FR-008**: The system MUST separate the logic that identifies an element from the logic that determines its graph category.

### Key Entities

- **Classification Rule Set**: A pluggable configuration that dictates how raw data elements map to structural graph categories.
- **Graph View**: A presentation of pattern data organized into structural categories, either pre-calculated or evaluated on-the-fly.
- **Structural Category**: One of the standard buckets (Node, Relationship, Walk, Annotation) or a custom domain-specific bucket.
- **Extensible Storage (`pgOther`)**: An internal storage map pairing each classified "Other" element's identifier with a tuple containing both its `extra` user-defined type tag and the original `Pattern v`, preserving classification context.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of existing client integrations and automated tests relying on previous graph views continue to function without modification.
- **SC-002**: Users can define and utilize custom domain-specific categories without modifying the core system architecture.
- **SC-003**: Data retention during graph construction improves to 100%, with zero elements rejected as "unrecognized".
- **SC-004**: Graph construction processing time with the default rule set is within 5% of the processing time of the legacy hardcoded implementation.
