# Data Model: PatternGraph (033-pattern-graph)

**Phase**: 1 – Design  
**Date**: 2026-02-18

## Overview

PatternGraph is a container that holds four categories of graph elements (nodes, relationships, walks, annotations), each keyed by a stable identity. Elements are stored as `Pattern v`; classification and identity are provided by a `GraphValue v` constraint. The model supports merge-on-insert, recursive decomposition (walks and annotations merge their components), and conversion to the existing GraphLens view.

## Entities

### PatternGraph (graph container)

- **Purpose**: Single owning structure for the round-trip workflow (parse → load → modify → serialize).
- **Fields** (logical; implementation uses four maps keyed by `Id v`):
  - **Nodes**: Map from identity to pattern. Each entry is a pattern classified as Node (0 elements).
  - **Relationships**: Map from identity to pattern. Each entry has 2 node elements.
  - **Walks**: Map from identity to pattern. Each entry has n relationship elements (ordered).
  - **Annotations**: Map from identity to pattern. Each entry has 1 element (the annotated target).
- **Invariants**:
  - Each identity appears in at most one category (per classification).
  - Relationships reference nodes that exist in the nodes map (after merge).
  - Walks reference relationships (and transitively nodes) that exist in their maps.
  - Annotations’ inner element is merged into the appropriate category so the target exists.
- **State**: Immutable; “update” is by producing a new PatternGraph (e.g. after merge).

### GraphValue (typeclass / value model)

- **Purpose**: Supplies identity and classification for the value type `v` used in `Pattern v`.
- **Operations**:
  - **Identity**: Extract a comparable key `Id v` from a value (e.g. from the pattern’s value).
  - **Classify**: Map a `Pattern v` to one of Node, Annotation, Relationship, Walk, or Unrecognized (by arity and element shape).
- **Primary instance**: Subject; `Id Subject = Symbol`.
- **Validation**: Classification must be deterministic and consistent for the same pattern shape; identity must be stable for the same logical element.

### PatternClass (classification)

- **Values**: Node (0 elements), Annotation (1 element), Relationship (2 elements, both nodes), Walk (n elements, all relationships), Unrecognized (anything else).
- **Use**: Determines which PatternGraph category an element goes into; Unrecognized is not stored and is returned in the “unrecognized” result.

### Node, Relationship, Walk, Annotation (element roles)

- **Node**: Pattern with 0 elements; stored in nodes map.
- **Relationship**: Pattern with 2 elements, both node-shaped; stored in relationships map; endpoint identities must exist in nodes after merge.
- **Walk**: Pattern with n elements, all relationship-shaped; stored in walks map; component relationships (and their nodes) are also merged.
- **Annotation**: Pattern with 1 element; stored in annotations map; inner element is merged into its category (node or relationship).

### Identity (Id v)

- **Purpose**: Stable key for deduplication and reconciliation.
- **Constraints**: Must be `Ord` (for Map keys). For Subject, identity is Symbol.
- **Uniqueness**: Within each category, one entry per identity; reconciliation policy applies when merge sees a duplicate identity in the same category.

### MergeResult (or equivalent)

- **Purpose**: Result of merge / fromPatterns that satisfies “do not silently drop unrecognized.”
- **Fields**: The updated PatternGraph; a collection of unrecognized patterns (e.g. list or non-empty list).
- **Use**: Callers can ignore, log, or error on unrecognized.

### GraphLens (existing interpretive view)

- **Purpose**: Read-only graph view over a scope pattern and a node predicate.
- **Relationship**: PatternGraph can be converted to GraphLens by building a scope pattern from the container’s contents and using the same atomic-node notion as classification. No new entity; reuse existing type and operations.

## Validation Rules (from requirements)

- **FR-002**: Every merge classifies the pattern and reconciles with existing entry at that identity using the defined reconciliation policy (from Pattern.Reconcile).
- **FR-004**: Merging a walk stores the walk and merges its component relationships and nodes.
- **FR-005**: Merging an annotation stores the annotation and merges its single inner element.
- **FR-006**: Unrecognized patterns are never stored; they appear only in the “unrecognized” part of the result.
- **FR-008**: Classification is defined in one place (GraphValue) and used consistently; an identity is stored in exactly one category.

## State Transitions

- **Empty → Populated**: Apply `merge` (or `fromPatterns`) to add elements. Unrecognized patterns do not change the graph; they are only accumulated in the result.
- **Populated → Updated**: Apply `merge` with a pattern whose identity already exists; reconciliation produces the new entry for that identity in the same category; graph is replaced with the updated one.
- **Populated → GraphLens**: Conversion builds a scope pattern and a predicate; the graph structure is read-only and reflects the container at conversion time.

## Dependencies

- **Pattern.Core**: Pattern type and construction.
- **Pattern.Reconcile**: ReconciliationPolicy and per-identity merge (reconcile) for duplicate identities.
- **Pattern.Graph**: GraphLens type and operations (for conversion target).
- **Subject** (or other `v`): Provides value type and GraphValue instance (e.g. Id Subject = Symbol).
