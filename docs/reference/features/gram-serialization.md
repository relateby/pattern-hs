# Gram Serialization Feature Specification

**Status**: ✅ Implemented  
**Location**: `libs/gram/src/Gram/`  
**Features**: Serialization, Parsing, Validation, JSON Support, Schema Generation (Features 14, 16, 20, 029)

## Overview

Gram serialization provides comprehensive support for converting `Pattern Subject` structures between multiple formats:
- **Gram notation** (text format)
- **Canonical JSON** with bidirectional conversion
- **JSON Schema** for validation and documentation
- **TypeScript** and **Rust** type definitions for downstream ports

The library handles all value types, nested patterns, relationships, and anonymous subjects across all formats.

## Serialization

### `toGram` - Serialize to Gram Notation

```haskell
toGram :: Pattern Subject -> String
```

Converts a `Pattern Subject` to gram notation string.

### `toGramList` - Serialize multiple patterns

```haskell
toGramList :: [Pattern Subject] -> String
```

### `toGramWithHeader` - Serialize header and patterns

```haskell
toGramWithHeader :: PropertyRecord -> [Pattern Subject] -> String
```

## Parsing

### `fromGram` - Parse from Gram Notation

```haskell
fromGram :: String -> Either ParseError (Pattern Subject)
```

Parses gram notation to a single `Pattern Subject`. If the document contains multiple patterns or a leading record, they are wrapped in a pattern with the label `Gram.Root`.

### `fromGramList` - Parse to list of patterns

```haskell
fromGramList :: String -> Either ParseError [Pattern Subject]
```

### `fromGramWithHeader` - Parse to header and patterns

```haskell
fromGramWithHeader :: String -> Either ParseError (Maybe PropertyRecord, [Pattern Subject])
```

## JSON Serialization

### `Gram.JSON` Module

Provides canonical JSON representation with bidirectional conversion:

```haskell
-- ToJSON/FromJSON instances for Pattern, Subject, Value
encode :: Pattern Subject -> ByteString
decode :: ByteString -> Maybe (Pattern Subject)
```

**Features**:
- Canonical JSON format with deterministic output
- Key sorting for byte-for-byte comparison
- All 10 value types supported (integer, decimal, boolean, string, symbol, tagged, array, map, range, measurement)
- Complex value types use discriminators (`type` field)
- Metadata wrapper support (version, timestamp, hash)

**CLI Support**:
```bash
gramref parse input.gram --format json --value-only --canonical
gramref convert input.json --from json --to gram
```

### `Gram.Schema` Module

Generates formal specifications for downstream ports:

```haskell
-- JSON Schema Draft 2020-12
generatePatternSchema :: Value

-- TypeScript type definitions
generateTypeScriptTypes :: Text

-- Rust struct definitions  
generateRustTypes :: Text
```

**CLI Support**:
```bash
gramref schema --format json-schema > pattern-schema.json
gramref schema --format typescript > pattern.ts
gramref schema --format rust > pattern.rs
```

## Validation

### `Gram.Validate` Module

```haskell
validate :: Gram -> Either ValidationError Gram
```

**Checks**:
- Duplicate definition checking
- Undefined reference checking
- Arity consistency checking

## Round-Trip Verification

Validated structural equality after serialization/deserialization cycles against the full test corpus (Feature 20).

## Test Coverage

- **Parsing Conformance**: 100% pass rate against `tree-sitter-gram` corpus (Feature 16)
- **Gram Round-Trip Tests**: Validated against full test corpus
- **JSON Round-Trip Tests**: 35+ unit tests + 200 QuickCheck properties
- **Schema Generation**: 17 tests validating JSON Schema, TypeScript, and Rust output
- **Validation Tests**: Comprehensive validation rule tests

## See Also

- **[Canonical JSON Format](canonical-json-format.md)** - JSON format specification
- **[Gram Semantics](../semantics/gram-semantics.md)** - Semantic specification
- **[Subject Library](subject.md)** - Subject type specification
- **[Implementation](../IMPLEMENTATION.md)** - Implementation patterns

