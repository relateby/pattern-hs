# Data Model: Gram annotation-based identifiers and labels

**Feature**: 032-gram-annotation-syntax  
**Date**: 2026-02-17

## Overview

The CST currently has a single annotation form: `Annotation { annKey, annValue }` (property-style only). This feature extends the model so that annotations can be either **property-style** (`@key(value)`) or **identified/labeled** (`@@` with optional identifier and/or labels). The parser produces a list of annotations per `AnnotatedPattern`; ordering is “at most one identified first, then zero or more property.”

## Entities and CST Changes

### Annotation (extended)

Annotations are one of two kinds:

1. **Property annotation** — key (symbol) + value (integer, symbol, string, etc.). Same as current `Annotation` content.
2. **Identified annotation** — optional identifier, optional labels (at least one of the two required in syntax). No key/value; body is the following pattern (not part of the annotation node).

Recommended Haskell representation:

- **PropertyAnnotation**: key (`Symbol`), value (`Value`). Backward-compatible with current `Annotation`.
- **IdentifiedAnnotation**: optional `Identifier`, optional labels (`Set String` or equivalent). At least one present when parsed.
- **Annotation**: sum type `PropertyAnnotation ... | IdentifiedAnnotation ...` (or equivalent so consumers can discriminate).

Existing `AnnotatedPattern` remains:

- `apAnnotations :: [Annotation]` — list of annotations in order. Parser enforces: at most one `IdentifiedAnnotation`, and it must appear first if present; rest are `PropertyAnnotation`.
- `apElements :: [PatternElement]` — the single pattern body (node, path, or subject pattern) that the annotations apply to.

### Property-style annotation (single `@`)

| Field  | Type     | Description |
|--------|----------|-------------|
| key    | Symbol   | Property key. |
| value  | Value    | Value inside parentheses (symbol, string, integer, etc.). |

Unchanged from current CST except possibly renamed to `PropertyAnnotation` or wrapped in a sum.

### Identified/labeled annotation (double `@@`)

| Field      | Type                | Description |
|------------|---------------------|-------------|
| identifier | Maybe Identifier    | Optional naming identifier (symbol, string, or integer per grammar). |
| labels     | Set String (or list)| Optional labels (`:L`, `::Label` style). At least one of identifier or labels must be present in source. |

Body is not part of this node; it is the `apElements` of the parent `AnnotatedPattern`.

## Validation Rules

- **Empty `@@`**: Parser MUST reject `@@` followed immediately by space and then pattern (e.g. `@@ (a)`). No valid CST for that input (FR-005).
- **Stack order**: When building `apAnnotations`, at most one identified annotation and it must be first. Parser structure enforces this.
- **Property form**: `@` must be followed by key (symbol) and `(` value `)`. Invalid or empty value should result in parse error.
- **Label syntax**: In `@@` header, labels use the same syntax as elsewhere (`:L`, `::Label`); reuse existing label parsing.

## State Transitions

N/A — annotations are syntactic; no state machine.

## Relationships

- **AnnotatedPattern** → `apAnnotations` (list of Annotation: zero or one identified then zero or more property), `apElements` (single pattern body).
- **Annotation** (sum) → either PropertyAnnotation (key, value) or IdentifiedAnnotation (optional identifier, optional labels).
- **IdentifiedAnnotation** → identifier and labels are direct fields; body is the sibling `apElements` of the parent `AnnotatedPattern`.

## Backward Compatibility

- Existing `@key(value)` continues to parse as property annotation. Call sites that only handle key/value can match on the property variant; new code can handle both.
- If we keep the name `Annotation` for the sum type, existing type references still hold; only the constructors or pattern-matching change at use sites that need to distinguish.
