# Contract: Gram.Serialize

## Module: `Gram.Serialize`

### `toGram`

```haskell
toGram :: Pattern Subject -> String
```

**Description**:
Serializes a `Pattern Subject` into a gram notation string adhering to `tree-sitter-gram` 0.2.7.

**Behavior Changes**:
- **Flattening**: If the pattern is `Gram.Root`, its elements are serialized as top-level siblings separated by newlines, without enclosing brackets.
- **Annotations**:
    - If a pattern is `Gram.Annotated`, its `Gram.Annotation` elements are serialized as `@key(value)` separated by spaces (no commas), followed by the content pattern.
    - If a subject is `Gram.Annotation`, it is serialized as `@key(value)`.
- **Top-Level Types**: Supports mixed top-level elements including nodes `()`, subjects `[]`, paths `()->()`, and annotations `@()`.

## Module: `Gram.Transform` (Internal)

### `transformGram`

```haskell
transformGram :: CST.Gram -> Pattern Subject
```

**Behavior Changes**:
- Maps `CST.Annotation` to `Gram.Annotation` subject.
- Wraps annotated patterns in `Gram.Annotated` container.

