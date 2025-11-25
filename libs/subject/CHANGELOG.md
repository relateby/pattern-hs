# Changelog

All notable changes to the Subject library will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2025-01-28

### Added

- Core Subject data type with identity, labels, and property records
- Identifier type supporting Symbol, StringLiteral, and Integer
- Value type system supporting standard types (Integer, Decimal, Boolean, String, Symbol) and extended types (TaggedString, Array, Map, Range, Measurement)
- PropertyRecord type as Map String Value
- Typeclass instances: Show, Eq, Ord, Hashable, Semigroup, Monoid
- Constructor functions: `subject`, `subjectWith`
- Property manipulation functions: `addProperty`, `updateProperty`, `removeProperty`, `hasProperty`
- Comprehensive test suite covering all functionality
- Haddock documentation for all public APIs
- Examples demonstrating gram notation mappings and Pattern integration

### Design

- Subject designed as self-descriptive object matching gram notation attributes
- All three components (identity, labels, properties) are optional/flexible
- Labels are lists to support multiple labels
- Property records use Map for efficient lookups
- Value type system supports all gram notation value types including nested structures
- Subject works seamlessly as Pattern value type (Pattern Subject)

