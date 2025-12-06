# Data Model: Gram Serialization

## Core Concepts

The data model relies on `Pattern Subject` as the internal representation. 
To support `tree-sitter-gram` 0.2.7 features (annotations, flat structure), we use a **semantic mapping** strategy that integrates annotations directly into the Subject's property record.

## Semantic Mappings

### 1. Annotations (`@key(value)`)

Annotations are semantically equivalent to properties on the wrapping subject pattern.
Transformation Rule:
`@k("v") @k2("v2") (a)`  ⟹  `[ {k:"v", k2:"V2"} | (a)]`

- **Input**: Annotated Pattern in CST.
- **Transformation**: 
  - Collect all annotations as key-value pairs.
  - Create (or update) the Subject of the pattern.
  - If the pattern was a simple node `(a)`, it becomes a subject pattern `[...]` containing `(a)` as an element, with the annotations as properties of the outer subject.
  - **Subject State**:
    - **Identity**: Empty (unless the annotated construct itself acts as the subject, but the equivalence implies a wrapper).
    - **Labels**: Empty.
    - **Properties**: Union of all annotation key-values.
  - **Elements**: The original pattern element(s) become the children of this new Subject Pattern.

### 2. Root (`Gram.Root`)

Represented as a `Pattern` with:
- **Subject**: `Gram.Root` (Identity="", Labels={"Gram.Root"}, Props={...file-level-props...}).
- **Elements**: List of top-level patterns.

## Serialization Rules

### Top-Level Flattening
When serializing a `Gram.Root` pattern:
1.  Serialize each element in `elements`.
2.  Join with newlines.
3.  Do **not** wrap in `[]` or `()`.

### Annotated Pattern Serialization
Since annotations are transformed into properties of a wrapper Subject Pattern, they are serialized as a standard **Subject Pattern**:
- `[ {k:"v", k2:"V2"} | (a) ]`

This conforms to the `tree-sitter-gram` 0.2.7 grammar which allows `subject_pattern` at the top level. The `@` syntax is "compiled away" during transformation and serialized as canonical Gram properties.

## Parsing/Transformation Updates

- `Gram.Transform.transformGram` must be updated to:
    - Handle `CST.AnnotatedPattern`.
    - Extract annotations and convert them to a `Map String Value`.
    - Wrap the content elements in a new `Pattern` with a `Subject` containing these properties.
    - If no annotations exist, process elements as before (preserving existing behavior).

## Validation

- **SC-001**: Serialization output matches `tree-sitter-gram` 0.2.7 (using `subject_pattern` syntax).
- **SC-002**: Flat structure for top-level.
- **SC-003**: No commas (properties use commas, but annotation lists in `@` syntax are avoided).
