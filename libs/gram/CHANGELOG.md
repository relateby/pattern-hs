# Changelog

All notable changes to the gram library will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.1] - Unreleased

### Fixed
- **parseAnnotations**: Corrected parsing so identified annotations are recognized in all contexts (removed unnecessary lookAhead that could skip annotation parsing).
- **Empty @@ header**: Error message now includes examples (e.g. `@@p`, `@@:L`, `@@p:L`).

## [0.3.0] - 2026-01-21

### Breaking
- **fromGram**: Now returns `Either ParseError [Pattern Subject]` instead of `Either ParseError (Pattern Subject)`.
- **toGram**: Now takes `[Pattern Subject]` instead of `Pattern Subject`.
- **Gram.CST**: `Gram` renamed to `GramDoc`.
- **Gram**: Re-exports are now explicit; some previously re-exported symbols may need to be imported from submodules.
- **Leading bare record**: In `fromGram`, a leading bare record is no longer dropped; it is represented as an anonymous, no-elements pattern and placed first in the result. In `toGram`, when the first pattern is anonymous, has no labels, and has no elements, it is serialized as a bare root record `{k:v}` or `{}`; only the first pattern is considered.

### Added
- **fromGramWithHeader**: `String -> Either ParseError (Maybe PropertyRecord, [Pattern Subject])`; header kept separate from the pattern list.
- **toGramWithHeader**: `PropertyRecord -> [Pattern Subject] -> String`; serializes an explicit header and then the patterns.
- **serializePattern**: `Pattern Subject -> String`; serializes a single pattern (useful when you already have one pattern).

## [0.2.0] - 2026-01-10

### Added
- **Gram.JSON module**: Canonical JSON serialization/deserialization for Pattern<Subject>
  - ToJSON/FromJSON instances for Pattern, Subject, and all Value types
  - Bidirectional conversion between Gram notation and JSON
  - Canonical form with deterministic output (sorted keys)
  - Support for all 10 value types (integer, decimal, boolean, string, symbol, tagged, array, map, range, measurement)
  - Metadata wrapper support (version, timestamp, hash)
- **Gram.Schema module**: Schema and type definition generation
  - Gram.Schema.JSONSchema: JSON Schema Draft 2020-12 generation
  - Gram.Schema.TypeScript: TypeScript type definitions with interfaces and type guards
  - Gram.Schema.Rust: Rust struct definitions with serde derives
- **Comprehensive test suite**:
  - 35+ JSON roundtrip unit tests
  - 200 QuickCheck property-based tests
  - 17 schema generation tests
  - Semantic equivalence checking for integer/decimal ambiguity

### Changed
- Gram.Serialize and Gram.Parse remain backward compatible

## [0.1.2.0] - Previous

### Added
- Initial library structure
- Gram.Serialize module for serializing Pattern Subject to gram notation
- Gram.Parse module for parsing gram notation to Pattern Subject

