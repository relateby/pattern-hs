# Data Model: Gram Documents

This feature refines the representation of a "Gram Document" as distinct from a "Gram Pattern".

## Entities

### Gram Document

A document is the top-level container for Gram notation.

- **Header Metadata** (Optional): A `PropertyRecord` (Map from String to Value) appearing at the very beginning of the document.
- **Content**: A sequence (list) of `Pattern Subject` elements.

### Mapping to Pattern Subject

While the new API returns lists, the legacy "Single Root" model is still supported through a mapping:

| Logical Part | Single-Root Representation |
|--------------|--------------------------|
| Header Metadata | `properties` of the root `Pattern` |
| Pattern List | `elements` of the root `Pattern` |
| Identity | Root identity is empty (`Symbol ""`) |
| Label | Root has the label `Gram.Root` |

## Validation Rules

1.  **Header Uniqueness**: Only one bare record at the start of the document is treated as a header.
2.  **Bare Records as Patterns**: A bare record appearing anywhere else in the document is treated as an anonymous pattern with properties (as before).
3.  **Empty Documents**: An empty document results in an empty list of patterns and `Nothing` for the header.

## API Signatures (Haskell)

```haskell
-- | Parse a Gram document into a list of patterns.
fromGramList :: String -> Either ParseError [Pattern Subject]

-- | Parse a Gram document into an optional header and a list of patterns.
fromGramWithHeader :: String -> Either ParseError (Maybe PropertyRecord, [Pattern Subject])

-- | Serialize a list of patterns to Gram notation.
toGramList :: [Pattern Subject] -> String

-- | Serialize a header and a list of patterns to Gram notation.
toGramWithHeader :: PropertyRecord -> [Pattern Subject] -> String
```
