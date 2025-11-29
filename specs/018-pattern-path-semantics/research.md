# Research: Gram Pattern and Path Semantics

## Decisions

### Validation Strategy
- **Decision**: Implement a separate `Gram.Validate` module that traverses the `Gram.CST` before transformation.
- **Rationale**: Separation of concerns. The parser handles syntax, the validator handles semantics (definitions, references, cycles), and the transformer builds the final core structure. This allows catching semantic errors without polluting the core `Pattern` construction logic.
- **Alternatives Considered**: 
  - *Validation during Parsing*: Too complex; semantic rules like "forward references" require full AST visibility.
  - *Validation during Transformation*: Mixing validation logic with structural transformation makes the code harder to read and maintain.

### Symbol Table Structure
- **Decision**: Use a `Map Identifier SymbolInfo` where `SymbolInfo` captures:
  - Definition status (Defined, ForwardReferenced)
  - Type hint (Pattern, Node, Relationship)
  - Structural signature (for consistency checks)
- **Rationale**: Needed to enforce single-definition and consistency rules.

### Error Handling
- **Decision**: Return a list of `ValidationError`s rather than failing on the first one.
- **Rationale**: Provides a better developer experience to see multiple issues at once.

### Path Notation Consistency
- **Decision**: Decompose paths into equivalent pattern structures for validation.
  - `(a)-[r]->(b)` becomes `Definition(a)`, `Definition(b)`, `Definition(r, elements=[a,b])`.
- **Rationale**: Unified validation logic. Both notations map to the same underlying constraints.

### Cycle Detection
- **Decision**: Track the "visiting" path during recursive validation to detect direct cycles.
- **Rationale**: Standard approach for graph cycle detection.

## Technical Unknowns Resolved

- **Parsers**: `Gram.Parse` exists and produces `Gram.CST`.
- **AST**: `Gram.CST` is available but lacks source positions. Validation errors will reference identifiers but not line numbers for now.
- **Dependencies**: `containers` and `mtl` (for State/Except monads) are available/standard.

## Future Considerations
- Adding source spans to `Gram.CST` for better error reporting.
- Optimizing validation for very large files (incremental validation).

