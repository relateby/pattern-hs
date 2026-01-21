# Research: Separation of Container and Content Parsing

## Decision: Separate Container API from Content API

The project will move away from implicitly wrapping multiple patterns in a `Gram.Root` pattern during parsing. Instead, the API will explicitly support documents as collections of patterns.

### Rationale

1.  **Cross-Language Consistency**: The `gram-rs` implementation has adopted this model, returning `Vec<Pattern<Subject>>` from its primary `parse_gram` function. Aligning `gram-hs` ensures that developers moving between languages encounter the same mental model.
2.  **Metadata Clarity**: Using a "magic label" (`Gram.Root`) to store file-level metadata is fragile and makes it harder for users to distinguish between intentional pattern labels and parser-injected metadata.
3.  **Performance and Scaling**: For large documents (SC-001), treating patterns as a stream or list is more efficient than building a single massive nested data structure.

### Alternatives Considered

1.  **Status Quo (Magic Label)**:
    - *Why evaluated*: Least amount of change.
    - *Why rejected*: Confusing for end-users, inconsistent with `gram-rs`, and mixes document-level concerns with pattern-level concerns.
2.  **New `GramDoc` Data Type**:
    - *Why evaluated*: Provides a formal structure for `(Maybe Header, [Pattern])`.
    - *Why rejected*: `gram-rs` opted for simple tuples/vectors for the raw API to keep it lightweight. We will follow that for the basic API, but we might use a helper type if needed for the CLI.

### Implementation Details

#### API Alignment

| Feature | `gram-rs` | `gram-hs` (Proposed) |
|---------|-----------|----------------------|
| Parse multiple | `parse_gram(&str) -> Vec<Pattern>` | `fromGramList :: String -> Either ParseError [Pattern Subject]` |
| Parse w/ Header | `parse_gram_with_header(&str) -> (Option<Record>, Vec<Pattern>)` | `fromGramWithHeader :: String -> Either ParseError (Maybe PropertyRecord, [Pattern Subject])` |
| Serialize multiple | `to_gram(&[Pattern]) -> String` | `toGramList :: [Pattern Subject] -> String` |
| Serialize w/ Header | `to_gram_with_header(Record, &[Pattern]) -> String` | `toGramWithHeader :: PropertyRecord -> [Pattern Subject] -> String` |

#### Backward Compatibility

`fromGram` and `toGram` will remain but will be implemented as wrappers around the new functionality, preserving the `Gram.Root` wrapping for existing callers to avoid breaking changes in the 0.x series.

## Best Practices for Megaparsec Streams

The current parser in `Gram.Parse` is built on `megaparsec`. The refactor will involve:
1.  Modifying the top-level `parseGram` parser to return a structure that clearly separates the leading record from the list of patterns.
2.  Updating `Gram.Transform` to handle the new `Gram` CST structure correctly.
