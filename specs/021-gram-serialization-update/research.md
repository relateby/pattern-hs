# Research: Gram Serialization Update

## Objective
Determine how to support the required serialization changes for `tree-sitter-gram` 0.2.7, specifically "flat top-level elements" and "annotations without commas".

## Key Questions

1.  **How to represent Annotations in `Pattern Subject`?**
    *   Current `Subject` does not have an `annotations` field.
    *   `Gram.Transform` drops CST annotations.
    *   *Option A*: Add `annotations` to `Subject`. (Big change, affects `libs/subject`).
    *   *Option B*: Map annotations to a special label/property convention (e.g., label `Gram.Annotation`, property `value`).
    *   *Option C*: Do not support annotations in `Pattern Subject`, but support them in `Gram.CST` serialization. (But `toGram` works on `Pattern`).

2.  **What does "flattening" imply for the internal model?**
    *   Currently, `Gram.Transform` wraps everything in a `Gram.Root` subject if there are multiple patterns.
    *   If the output needs to be flat, `toGram` needs to detect this "wrapper" and unwrap it, outputting children separated by newlines (or just whitespace).

## Findings

### Annotation Representation

*   **Current State**: `Gram.Parse` parses `@key(val)` into `Annotation` type. `Gram.Transform` ignores them.
*   **Recommendation**: To avoid breaking `libs/subject` (which is a core library potentially used by others), we should investigate if we can map Annotations to a `Subject` with a specific reserved label, e.g., `Gram.Annotation`.
    *   Structure: `Subject { identity = "key", labels = ["Gram.Annotation"], properties = { "value": val } }`.
    *   Serialization: `toGram` checks for `Gram.Annotation` label. If present, it outputs `@key(val)`.
    *   Parsing/Transform: Update `Gram.Transform` to convert `CST.Annotation` into this special `Subject`.

### Flattening Strategy

*   **Current State**: `Gram.Root` label is used to identify the file root.
*   **Recommendation**:
    *   Continue using `Gram.Root`.
    *   Update `toGram` to ensure that when it encounters `Gram.Root`, it outputs its elements as a flat list, without wrapping them in `[]` or `()`.
    *   Ensure that the elements can be any top-level construct: `Subject`, `Node`, `Relationship` (path), `Annotation`.

## Plan

1.  **Prototype Annotation Mapping**: Test if `Gram.Annotation` label strategy works for round-tripping.
2.  **Prototype Flattening**: Verify `serializeImplicitElements` logic covers the "flatness" requirement.

