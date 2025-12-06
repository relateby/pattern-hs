# Implementation Plan - Update Gram Serialization for Tree-Sitter 0.2.7

The goal of this feature is to update the gram serialization logic to conform with the latest `tree-sitter-gram` 0.2.7 specification. This involves flattening the top-level pattern structure and ensuring annotations are serialized (via transformation to properties).

## Technical Context

- **Gram Library**: The `libs/gram` package handles parsing and serialization.
- **Serialization**: `Gram.Serialize.toGram` converts `Pattern Subject` to a string.
- **Parsing**: `Gram.Parse` parses strings into `Gram.CST`, which is then transformed into `Pattern Subject`.
- **Transformation**: `Gram.Transform` converts `Gram.CST` to `Pattern Subject`.
- **Tree-Sitter Gram 0.2.7**: The target grammar. Key changes:
    - Flattened top-level structure.
    - Annotations are mapped to properties of a wrapper subject pattern.

## Constitution Check

- **Code Quality**: Changes must be clean and documented.
- **Testing**: New serialization logic must be covered by tests.
- **Conceptual Consistency**: Serialization must reflect the logical structure.

## Proposed Changes

### Phase 1: Design & Research (Completed)

- Semantic Mapping: Annotations `@k(v)` mapped to properties `{k:v}` on a wrapper subject.

### Phase 2: Implementation

1.  **Update `Gram.Transform`**:
    - Modify `transformPattern` to handle `AnnotatedPattern` with annotations.
    - If annotations exist:
        - Convert `[Annotation]` to `Map String Value`.
        - Create a new wrapper `Pattern` with `Subject` (no id, no labels, props from annotations).
        - Set `elements` to the transformed content of the pattern.
    - If no annotations, pass through (preserving existing structure).
2.  **Update `Gram.Serialize`**:
    - Verify `serializeImplicitElements` handles the flat list correctly (it appears to already do so, but verify `Gram.Root` label handling).
    - Ensure `toGram` outputs `Gram.Root` elements separated by newlines without wrapping brackets.

### Phase 3: Validation

1.  **Test Cases**:
    - `tests/Gram/SerializeSpec.hs`: Add tests for top-level flattening.
    - Add tests for annotated patterns (verifying they become `[{props} | content]`).
2.  **Round-Trip**: Verify `parse "@a(1) (n)" -> transform -> serialize` produces `[{a:1} | (n)]` (or equivalent).

## Gates

- [ ] **Tests Passing**: All new serialization tests pass.
