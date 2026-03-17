# Data Model: Scope Unification for Structure-Aware Operations

**Branch**: `038-scope-unification` | **Date**: 2026-03-17

## Overview

This feature does not introduce persisted storage. The "data model" is the public in-memory API surface that defines how scope-aware folding is represented and how existing graph/tree behavior is preserved.

## Entities

### 1. `ScopeQuery`

**Purpose**: The generic contract for "what is in scope" during a structure-aware operation.

**Core behaviors**:
- `containers`: direct containers of an element within the current scope
- `siblings`: co-elements sharing the same direct parent/container within the current scope
- `byIdentity`: direct lookup of an in-scope element by identity
- `allElements`: complete enumeration of the current scope

**Validation rules**:
- Must answer only for the current scope boundary.
- Must not infer a broader enclosing scope than the caller supplied.
- Must return empty results rather than errors when a query is unsupported by the chosen scope.

**Relationships**:
- Implemented by `TrivialScope`
- Reified as `ScopeDict`
- Consumed by `paraWithScope`

### 2. `TrivialScope`

**Purpose**: The subtree-only scope used to preserve current `para` behavior.

**Fields / state**:
- `rootPattern`: the `Pattern v` that defines the subtree boundary

**Validation rules**:
- `byIdentity` searches only within the subtree rooted at `rootPattern`
- `allElements` enumerates only that subtree
- `containers` and `siblings` return empty results because this scope lacks parent/sibling context

**Relationships**:
- Implements `ScopeQuery`
- Used by `para` as the default scope adapter

### 3. `ScopeDict`

**Purpose**: First-class value representation of scope behavior for dynamic composition and storage.

**Fields / state**:
- containment function
- sibling function
- identity lookup function
- all-elements enumeration value or function

**Validation rules**:
- Must be observationally equivalent to the scope provider from which it was reified
- Must preserve scope boundary semantics

**Relationships**:
- Reified from any `ScopeQuery` implementation via `toScopeDict`
- Implements `ScopeQuery` itself

### 4. `GraphViewScope` Adapter

**Purpose**: Adapter that answers generic scope queries across all classified elements present in a `GraphView` without changing `GraphQuery(..)`.

**Fields / state**:
- `baseQuery`: the existing `GraphQuery v` snapshot for graph-topology operations
- `viewElements`: the classified element list from `GraphView`
- `elementIndex`: identity-based index over all classified elements in the view

**Validation rules**:
- Must cover all classified elements in the `GraphView`, not just nodes and relationships
- Must preserve the `GraphView` snapshot boundary for the duration of a fold
- Must remain additive and not require new fields on `GraphQuery`

**Relationships**:
- Built from `GraphView`
- Supplies generic scope answers for graph-aware planning/implementation
- Delegates graph-topology queries to `baseQuery`

### 5. `Scope-Aware Fold Step`

**Purpose**: The algebra used by the unified fold contract.

**Inputs**:
- current scope provider
- current `Pattern v`
- already-computed direct child results in element order

**Validation rules**:
- Child results must be bottom-up
- Scope must remain fixed for the full fold
- `para` compatibility requires full child-result completeness for recursive tree folds
- `paraGraph` compatibility requires best-effort omission for graph cycle cases

**Relationships**:
- Executed by `paraWithScope`
- Reused conceptually by `para` and `paraGraph` wrappers

## Behavioral Relationships

### Scope Boundary

- `TrivialScope` defines scope from one subtree root.
- `GraphViewScope` defines scope from the full `GraphView` snapshot.
- `ScopeDict` preserves whichever scope boundary it reifies.

### Wrapper Preservation

- `para` = `paraWithScope` + `TrivialScope`
- `paraGraph` = graph-specific scheduling/accumulation wrapper that preserves current output while using the shared scope model

## State Transitions

This feature is pure and in-memory. The relevant transitions are conceptual:

1. A caller chooses a scope boundary.
2. That boundary is represented as a `ScopeQuery` implementation or `ScopeDict`.
3. A structure-aware fold runs with that scope fixed for the duration of the operation.
4. Existing wrappers (`para`, `paraGraph`) preserve their current externally visible outputs.

## Test-Critical Invariants

- `paraWithScope (trivialScope p)` is observationally equivalent to `para p`.
- Reifying a scope provider with `toScopeDict` does not change any generic scope answers.
- Narrower scopes never expose elements outside their declared boundary.
- `paraGraph` preserves existing `topoShapeSort` ordering and cycle soft-failure semantics.
