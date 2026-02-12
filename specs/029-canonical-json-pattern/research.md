# Research: Canonical JSON Pattern Representation

**Feature**: 029-canonical-json-pattern  
**Date**: 2026-01-10  
**Status**: Complete

## Research Questions

### 1. JSON Schema Generation Approach

**Question**: Should we use a Haskell library for JSON Schema generation, or construct schemas manually?

**Research**: 
- Evaluated `aeson-schema` - Not actively maintained, last update 2017
- Evaluated `json-schema` - Lightweight but only provides validation, not schema generation
- Examined existing code: `Gramref.CLI.JSON` uses plain `aeson` with `ToJSON` instances
- Considered `servant-jsonschema` - Designed for servant APIs, not general purpose

**Decision**: Manual JSON Schema construction using `aeson` Value builders

**Rationale**:
- Full control over schema structure and metadata
- No external dependencies beyond existing `aeson`
- Clear, maintainable code that generates exact desired schema format
- Can generate documentation inline with schema
- Easy to version and evolve schema independently
- Haskell libraries for schema generation are either unmaintained or designed for specific use cases

**Alternatives Considered**:
- **Library-based generation**: Rejected due to lack of mature, maintained libraries
- **Template Haskell derivation**: Rejected as it would couple schema to implementation details and reduce control
- **External tool (e.g., Rust/TypeScript schema generator)**: Rejected as gramref should be self-contained

**Implementation Approach**:
```haskell
-- In Gram.Schema.JSONSchema module
generatePatternSchema :: Value
generatePatternSchema = object
  [ "$schema" .= ("http://json-schema.org/draft/2020-12/schema#" :: Text)
  , "title" .= ("Pattern<Subject>" :: Text)
  , "version" .= ("0.1.0" :: Text)
  , "definitions" .= object [ ... ]
  , "type" .= ("object" :: Text)
  , "properties" .= ...
  ]
```

---

### 2. JSON Schema Version

**Question**: Which JSON Schema draft version should we target?

**Research**:
- Draft 2020-12 (latest stable as of 2026)
- Draft-07 (2018, widely supported)
- Examined tool support: Most validators support both versions
- Checked TypeScript/Rust schema tools: Both support 2020-12

**Decision**: JSON Schema Draft 2020-12

**Rationale**:
- Latest stable specification with best modern features
- Better support for vocabularies and dynamic schemas
- Improved validation keywords (`prefixItems`, `$dynamicRef`)
- Future-proof choice as it's the current standard
- Adequate tool support across ecosystems

**Alternatives Considered**:
- **Draft-07**: More widely deployed but lacks modern features
- **Draft-04**: Too old, lacks many useful validation features

---

### 3. TypeScript Type Generation

**Question**: How should we generate TypeScript type definitions from JSON Schema?

**Research**:
- Examined `json-schema-to-typescript` npm package - Most popular, actively maintained
- Considered inline Haskell code generation - Would require maintaining TypeScript syntax knowledge
- Evaluated quicktype CLI - Supports many languages but external dependency
- Checked existing gramref approach: Currently doesn't generate types

**Decision**: Use external post-processing with `json-schema-to-typescript` (recommended), but provide capability for manual template-based generation in Haskell

**Rationale**:
- `json-schema-to-typescript` is the de-facto standard with excellent output quality
- gramref can output the JSON Schema, users can pipe to external tool
- For self-contained generation, we'll create template-based TypeScript generator in Haskell
- Template approach is maintainable and produces clean, readable TypeScript
- Provides flexibility: users can use external tool for advanced features, or use built-in for simplicity

**Implementation Approach**:
- **Primary**: `gramref schema --format json-schema | json-schema-to-typescript`
- **Built-in**: `gramref schema --format typescript` - template-based generation in `Gram.Schema.TypeScript`

**Alternatives Considered**:
- **Pure Haskell generation with full TypeScript AST**: Rejected as overengineering, hard to maintain
- **Only external tool**: Rejected as it creates external dependency for basic use case
- **Only built-in template**: Rejected as external tools offer more sophisticated features

---

### 4. Rust Type Generation with Serde

**Question**: How should we generate Rust structs with serde annotations?

**Research**:
- Examined `quicktype` - Supports Rust + serde, good quality output
- Investigated `schemafy` - Rust tool, generates from JSON Schema but less mature
- Evaluated `typeshare` - Designed for sharing types between Rust/TypeScript/Kotlin
- Checked manual template generation feasibility - Simple for basic cases

**Decision**: Template-based Rust generation in Haskell for built-in support

**Rationale**:
- Rust serde code generation is straightforward for our use case
- Our types are relatively simple: structs with fields, enums for value types
- Template-based approach gives full control over output format
- No external dependencies required for basic generation
- Users can still use quicktype for more advanced scenarios

**Implementation Approach**:
```haskell
-- In Gram.Schema.Rust module
generateRustTypes :: Text
generateRustTypes = T.unlines
  [ "#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]"
  , "pub struct Pattern {"
  , "    pub value: Subject,"
  , "    pub elements: Vec<Pattern>,"
  , "}"
  , ...
  ]
```

**Alternatives Considered**:
- **Quicktype only**: Rejected as it's an external dependency, though users can still use it
- **schemafy integration**: Rejected as it's less mature and would complicate build
- **typeshare**: Rejected as it's designed for a different workflow (multi-language type sharing)

---

### 5. Roundtrip Testing Strategy

**Question**: What constitutes "semantic equivalence" for roundtrip testing? How should we integrate with tree-sitter-gram corpus?

**Research**:
- Examined existing gram test corpus: 130+ files in various test categories
- Reviewed current test structure: Organized by gram notation features
- Investigated Pattern equality: Already has Eq instance
- Considered format variations: Whitespace, property order, etc.

**Decision**: Semantic equivalence means structural and data equality (using Eq instance), allowing format differences

**Rationale**:
- `Pattern Subject` already has `Eq` instance that compares structure
- Format differences (whitespace, property ordering) don't affect semantics
- Tree-sitter-gram corpus provides comprehensive coverage
- Roundtrip test: `fromGram (toGram p) == p` is clear and testable

**Test Corpus Integration**:
- Reference existing corpus via symbolic link or direct path
- Create `test-data/roundtrip/corpus/` directory
- Add custom test cases for JSON-specific scenarios in `test-data/roundtrip/custom/`

**Equivalence Rules**:
1. Pattern structure must match exactly (value + elements)
2. Subject identity, labels, properties must match
3. Value types must preserve data precisely
4. Element order must be preserved
5. Empty collections ([], {}) equivalent to absent optional fields (for JSON compatibility)

**Implementation Approach**:
```haskell
-- In Gram.JSON module
parseJSON :: Value -> Parser (Pattern Subject)

-- In tests/Spec/Gram/RoundtripSpec.hs
roundtripProperty :: Property
roundtripProperty = property $ do
  pat <- forAll genPattern
  let json = toJSON pat
  let pat' = fromJSON json
  pat === pat'
```

**Alternatives Considered**:
- **Textual equivalence** (gram notation strings must match): Rejected as too strict, format variations are acceptable
- **Custom equivalence checker**: Rejected as `Eq` instance is sufficient
- **Manual test case creation**: Rejected as corpus provides better coverage

---

### 6. Value Type JSON Representation

**Question**: Are the current JSON representations for complex value types optimal for schema generation?

**Research**:
- Reviewed `Gramref.CLI.JSON.valueToJSON` implementation
- Examined complex types: VSymbol, VTaggedString, VRange, VMeasurement
- Checked consistency: All complex types use `{"type": "...", ...}` discriminator pattern
- Evaluated schema compatibility: Current structure maps well to JSON Schema `oneOf`

**Decision**: Keep existing JSON representation format with minor documentation enhancements

**Rationale**:
- Current format is already consistent and well-designed
- Uses type discriminator pattern: `{"type": "symbol", "value": "..."}`
- Maps cleanly to JSON Schema using `oneOf` with type-based discrimination
- Simple types (integer, boolean, string) use native JSON types
- Complex types use object representation with clear structure
- No breaking changes needed

**Current Format (Confirmed Optimal)**:
- **Simple types**: `VInteger 42` → `42`, `VString "hello"` → `"hello"`
- **Symbol**: `VSymbol "x"` → `{"type": "symbol", "value": "x"}`
- **Tagged String**: `VTaggedString "json" "{}"` → `{"type": "tagged", "tag": "json", "content": "{}"}`
- **Range**: `VRange (RangeValue 1 10)` → `{"type": "range", "lower": 1, "upper": 10}`
- **Measurement**: `VMeasurement "kg" 5.5` → `{"type": "measurement", "unit": "kg", "value": 5.5}`
- **Array**: `VArray [...]` → `[...]` (recursive)
- **Map**: `VMap {...}` → `{...}` (recursive)

**Schema Representation**:
```json
{
  "oneOf": [
    {"type": "integer"},
    {"type": "number"},
    {"type": "boolean"},
    {"type": "string"},
    {
      "type": "object",
      "required": ["type", "value"],
      "properties": {
        "type": {"const": "symbol"},
        "value": {"type": "string"}
      }
    },
    ...
  ]
}
```

**Alternatives Considered**:
- **Untagged union**: Rejected as it makes schema validation ambiguous
- **Different discriminator**: Rejected as "type" is the standard convention
- **Simplified representation**: Rejected as it would lose information

---

### 7. Deserialization (fromJSON) Requirements

**Question**: Do we need full deserialization from JSON back to Pattern<Subject>, or just serialization?

**Research**:
- Examined requirements: US2 requires bidirectional conversion
- Checked existing code: Only `toJSON` implemented, no `FromJSON` instance
- Considered roundtrip testing: Requires deserialization
- Evaluated validation use case: Needs to parse JSON for validation

**Decision**: Implement full deserialization with `FromJSON` instances

**Rationale**:
- Required for roundtrip testing (FR-003)
- Required for validation command (FR-010)
- Enables JSON as a first-class input format (not just output)
- Completes the bidirectional conversion story
- Relatively straightforward given existing structure

**Implementation Approach**:
```haskell
instance FromJSON (Pattern Subject) where
  parseJSON = withObject "Pattern" $ \obj -> do
    val <- obj .: "value"
    elems <- obj .: "elements"
    return $ Pattern val elems

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \obj -> do
    sym <- obj .: "identity"
    labels <- obj .: "labels"
    props <- obj .: "properties"
    return $ Subject sym (Set.fromList labels) props

-- Similarly for Value types with discrimination
```

**Alternatives Considered**:
- **Serialization only**: Rejected as it doesn't meet requirements
- **Validation-only parsing** (no full Pattern construction): Rejected as roundtrip testing needs full construction

---

## Summary of Decisions

1. **JSON Schema Generation**: Manual construction using `aeson` Value builders
2. **JSON Schema Version**: Draft 2020-12
3. **TypeScript Generation**: Template-based in Haskell + recommend external tool for advanced use
4. **Rust Generation**: Template-based in Haskell
5. **Roundtrip Testing**: Semantic equivalence via `Eq`, integrated with tree-sitter-gram corpus
6. **Value Type Representation**: Keep existing format (confirmed optimal)
7. **Deserialization**: Implement full `FromJSON` instances

## Next Steps

Proceed to Phase 1: Data Model & Contracts
- Define formal data model with JSON mappings
- Create example contract files (schema, TypeScript, Rust)
- Write quickstart guide with usage examples
