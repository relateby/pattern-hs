# Type Signatures: Gram Serialization Library

**Feature**: 014-gram-serialization  
**Date**: 2025-01-27

## Module: Gram.Serialize

### toGram

**Type Signature**:
```haskell
toGram :: Pattern Subject -> String
```

**Description**: Serializes a Pattern Subject data structure to gram notation string format.

**Parameters**:
- `Pattern Subject`: The pattern to serialize

**Returns**:
- `String`: Gram notation representation of the Pattern Subject

**Examples**:
```haskell
-- Simple subject
toGram (Pattern (Subject (Symbol "n") (Set.fromList ["Person"]) empty) [])
-- Result: "(n:Person)"

-- Subject with properties
toGram (Pattern (Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])) [])
-- Result: "(n:Person {name:\"Alice\"})"

-- Anonymous subject (empty Symbol)
toGram (Pattern (Subject (Symbol "") (Set.fromList ["Person"]) empty) [])
-- Result: "(:Person)" or "()" depending on implementation
```

**Requirements**: FR-001, FR-003, FR-004, FR-005, FR-006, FR-007, FR-008

**Error Handling**: N/A (always succeeds, produces String)

**Notes**:
- Handles all value types (standard and extended)
- Supports nested patterns
- Supports relationship patterns
- Handles anonymous subjects (empty Symbol)

## Module: Gram.Parse

### fromGram

**Type Signature**:
```haskell
fromGram :: String -> Either ParseError (Pattern Subject)
```

**Description**: Parses a gram notation string into a Pattern Subject data structure.

**Parameters**:
- `String`: Gram notation string to parse

**Returns**:
- `Either ParseError (Pattern Subject)`: Either a parse error or the parsed Pattern Subject

**Examples**:
```haskell
-- Simple subject
fromGram "(n:Person)"
-- Result: Right (Pattern (Subject (Symbol "n") (Set.fromList ["Person"]) empty) [])

-- Subject with properties
fromGram "(n:Person {name:\"Alice\"})"
-- Result: Right (Pattern (Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])) [])

-- Invalid syntax
fromGram "(invalid"
-- Result: Left (ParseError "Unexpected end of input: expected ')' at position 8")
```

**Requirements**: FR-002, FR-009, FR-010, FR-011, FR-012, FR-013, FR-014, FR-015

**Error Handling**: Returns `Left ParseError` on parsing failure with clear error message

**Notes**:
- Handles all value types (standard and extended)
- Supports nested patterns
- Supports relationship patterns
- Handles anonymous subjects (assigns empty Symbol)
- Provides clear error messages for invalid syntax

### ParseError

**Type Signature**:
```haskell
data ParseError = ParseError String
  deriving (Eq, Show)
```

**Description**: Error type representing parsing failures during gram notation parsing.

**Fields**:
- `String`: Error message containing error description, position information (if available), and context

**Examples**:
```haskell
ParseError "Unexpected token '}' at position 15: expected property value"
ParseError "Unclosed bracket '(' at position 5"
ParseError "Invalid value syntax at position 20: expected integer, decimal, boolean, string, or symbol"
```

**Requirements**: FR-015

**Error Handling**: N/A (error type)

**Notes**:
- Error messages should be clear and actionable
- Should include position information if available from parser
- Should include context (what was expected, what was found)

## Module: Gram

**Re-exports**: All public exports from `Gram.Serialize` and `Gram.Parse`:
- `toGram` from `Gram.Serialize`
- `fromGram` from `Gram.Parse`
- `ParseError` from `Gram.Parse`

**Type Signatures**: See individual modules above.

## Internal Helper Functions

The following functions are internal implementation details and not exported:

### Serialization Helpers

- `serializeSubject :: Subject -> String`: Serializes a Subject to gram notation
- `serializeValue :: Value -> String`: Serializes a Value to gram notation
- `serializePropertyRecord :: PropertyRecord -> String`: Serializes a property record to gram notation
- `serializePatternElements :: [Pattern Subject] -> String`: Serializes pattern elements to gram notation
- `escapeString :: String -> String`: Escapes special characters in strings
- `quoteSymbol :: Symbol -> String`: Formats a Symbol for gram notation

### Parsing Helpers

- `parseSubject :: Parser Subject`: Parses a Subject from gram notation (parser library specific)
- `parseValue :: Parser Value`: Parses a Value from gram notation
- `parsePropertyRecord :: Parser PropertyRecord`: Parses a property record from gram notation
- `parsePatternElements :: Parser [Pattern Subject]`: Parses pattern elements from gram notation
- `unescapeString :: String -> String`: Unescapes special characters in strings
- `parseSymbol :: Parser Symbol`: Parses a Symbol from gram notation

**Note**: Actual parser types depend on selected parsing library (tree-sitter-gram, Parsec, Megaparsec).

## Type Constraints

### Pattern Subject Constraints

- All elements in a Pattern Subject must be `Pattern Subject` (type consistency)
- Subject identity is always `Symbol` (empty Symbol "" represents anonymous subjects)
- Labels are `Set String` (no duplicates, order doesn't matter)
- Properties are `PropertyRecord` (Map String Value)

### Value Type Constraints

- Standard types: `VInteger Integer`, `VDecimal Double`, `VBoolean Bool`, `VString String`, `VSymbol String`
- Extended types: `VTaggedString String String`, `VArray [Value]`, `VMap (Map String Value)`, `VRange RangeValue`, `VMeasurement String Double`
- Values can be nested (arrays and maps contain other values)

### Gram Notation Constraints

- Must be valid gram notation syntax (parsable by tree-sitter-gram)
- Must match syntax features supported by tree-sitter-gram
- Comments may be present but are stripped during parsing

## Round-Trip Conversion Properties

The following property should hold for all valid Pattern Subject structures:

```haskell
forall (p :: Pattern Subject) . 
  case fromGram (toGram p) of
    Right p' -> p == p'  -- Structure and values preserved
    Left _ -> False       -- Should never happen for valid input
```

**Note**: Formatting (whitespace, comments) may vary, but structure and values must be preserved.

## Error Handling

### Serialization Errors

Serialization (`toGram`) always succeeds and produces a String. No error handling needed.

### Parsing Errors

Parsing (`fromGram`) returns `Either ParseError (Pattern Subject)`:
- `Right (Pattern Subject)`: Successful parsing
- `Left ParseError`: Parsing failure with error message

Error messages should include:
- Error type (syntax error, unexpected token, incomplete input)
- Position information (line, column if available)
- Context (what was expected, what was found)
- Clear, actionable guidance

## Performance Considerations

- Serialization should complete in under 1 second for patterns with 10,000+ nodes
- Parsing should complete in under 1 second for gram notation files up to 1MB
- Memory usage should be reasonable for large patterns (avoid excessive copying)

## Testing Requirements

All public functions must have:
- Unit tests covering normal cases, edge cases, and error conditions
- Property-based tests for round-trip conversion
- Integration tests using tree-sitter-gram test corpus

See `quickstart.md` for testing examples and patterns.

