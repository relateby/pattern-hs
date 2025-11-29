# API Contract: Subject Serialization

## Module: `Gram.Serialize`

```haskell
-- | Serialize a Pattern Subject to gram notation string.
-- Handles escaping, quoting, and structure.
toGram :: Pattern Subject -> String
```

## Module: `Gram.Parse`

```haskell
-- | Parse a gram notation string into a Pattern Subject.
-- Automatically assigns unique IDs (e.g., #1) to anonymous subjects.
fromGram :: String -> Either ParseError (Pattern Subject)
```

## Invariants

1.  **Round-Trip**: `fromGram (toGram s) == Right s` (modulo potential ID generation for previously anonymous subjects, which become named).
2.  **Identity**: `toGram` output for a Subject with ID `#1` is `(#1)` (or similar valid syntax).

