# Subject Library Implementation Plan

## Overview

Create a new `subject` library following the structure of the `pattern` library. The Subject type will be a self-descriptive object with:
- **Identity**: Optional identifier (String, Integer, or Symbol)
- **Labels**: List of label strings (can be empty)
- **Property Record**: Key-value map supporting standard types plus extended types (tagged strings, arrays, maps, ranges)

Subject will serve as the primary content type for patterns (i.e., `Pattern Subject` will be the common use case) and will serialize to gram notation.

## Library Structure

Following the `pattern` library structure:

```
libs/subject/
├── src/
│   ├── Subject.hs              # Main module (re-exports)
│   └── Subject/
│       ├── Core.hs             # Core Subject type and basic operations
│       ├── Value.hs            # Value type system (standard + extended types)
│       └── Construction.hs      # Constructor functions (future)
├── tests/
│   ├── Test.hs                 # Test runner
│   └── Spec/
│       └── Subject/
│           ├── CoreSpec.hs     # Core tests
│           └── ValueSpec.hs    # Value type tests
├── examples/
│   └── examples.md             # Usage examples
├── subject.cabal               # Build configuration
└── CHANGELOG.md                # Changelog
```

## Core Data Structure

### Subject Type

Based on the tree-sitter-gram grammar, a Subject has three optional components that can appear in any combination:

```haskell
data Subject = Subject
  { identity  :: Maybe Identifier  -- Optional identifier (symbol, string, or integer)
  , labels    :: [String]          -- List of labels (can be empty)
  , properties :: PropertyRecord    -- Key-value property map
  }
```

Where:
- `Identifier` is a sum type: `Symbol String | StringLiteral String | Integer Int`
- `PropertyRecord` is `Map String Value` (using `Data.Map`)
- `Value` supports standard types plus extended types (see Value Type System below)

### Value Type System

The `Value` type must support all gram notation value types:

```haskell
data Value
  = VInteger Integer
  | VDecimal Double
  | VBoolean Bool
  | VString String
  | VSymbol String
  | VTaggedString String String  -- tag and content
  | VArray [Value]
  | VMap (Map String Value)
  | VRange RangeValue
  | VMeasurement String Double   -- e.g., "5kg" -> ("kg", 5.0)
```

This enables property values to include:
- Standard types: integers, decimals, booleans, strings, symbols
- Extended types: tagged strings (`tag`content`), arrays, maps, ranges, measurements

## Implementation Phases

### Phase 1: Core Subject Type (Initial Implementation)

**Files**: `src/Subject/Core.hs`, `src/Subject/Value.hs`

**Tasks**:
1. Define `Identifier` type (Symbol | StringLiteral | Integer)
2. Define `Value` type with all standard and extended types
3. Define `PropertyRecord` as `Map String Value`
4. Define `Subject` data type with record syntax
5. Implement field accessors (automatic via record syntax)
6. Add comprehensive Haddock documentation
7. Implement `Show` instance for Subject
8. Implement `Eq` instance for Subject

**Type Signatures**:
```haskell
data Identifier = Symbol String | StringLiteral String | Integer Int
data Value = VInteger Integer | VDecimal Double | ... (see above)
type PropertyRecord = Map String Value
data Subject = Subject { identity :: Maybe Identifier, labels :: [String], properties :: PropertyRecord }
```

### Phase 2: Typeclass Instances

**Files**: `src/Subject/Core.hs`

**Tasks**:
1. Implement `Ord` instance for Subject (lexicographic ordering)
2. Implement `Hashable` instance for Subject
3. Implement `Semigroup` instance (combine properties, merge labels)
4. Implement `Monoid` instance (empty subject as identity)

### Phase 3: Constructor Functions

**Files**: `src/Subject/Construction.hs`

**Tasks**:
1. Implement `subject` - create subject with minimal required fields
2. Implement `subjectWith` - create subject with all fields
3. Implement helper functions for common construction patterns
4. Implement property manipulation functions (add, update, remove)

### Phase 4: Integration with Pattern

**Files**: `src/Subject/Core.hs` (or new integration module)

**Tasks**:
1. Ensure Subject works as Pattern value type (`Pattern Subject`)
2. Document integration patterns
3. Add examples showing `Pattern Subject` usage

## Test Structure

Following `pattern` library test structure:

**Files**: `tests/Spec/Subject/CoreSpec.hs`, `tests/Spec/Subject/ValueSpec.hs`

**Test Coverage**:
- Subject construction (with/without identity, labels, properties)
- Field accessors
- Value type system (all variants)
- Typeclass instances (Eq, Ord, Show, Hashable, Semigroup, Monoid)
- Property record operations
- Edge cases (empty subject, subject with only identity, etc.)

## Build Configuration

**File**: `libs/subject/subject.cabal`

Following `pattern.cabal` structure:
- Library section with exposed modules
- Test suite section with dependencies
- Build dependencies: `base`, `containers` (for Map), `hashable`, `unordered-containers`
- Test dependencies: `hspec`, `QuickCheck`, `pattern` (for integration tests)

**Update**: `cabal.project` to include `libs/subject/`

## Documentation

**Files**: 
- `src/Subject/Core.hs` - Comprehensive Haddock documentation
- `examples/examples.md` - Usage examples
- `CHANGELOG.md` - Version history

**Documentation Requirements**:
- Explain Subject as "self-descriptive object" concept
- Document identity, labels, and properties fields
- Explain Value type system (standard + extended types)
- Show gram notation examples mapping to Subject
- Document integration with Pattern library

## Gram Notation Mapping

Examples from grammar:
- `(n:Person {name:"ABK"})` → `Subject (Just (Symbol "n")) ["Person"] (fromList [("name", VString "ABK")])`
- `(a)-[r:KNOWS {since: 2024}]->(b)` → `Subject (Just (Symbol "r")) ["KNOWS"] (fromList [("since", VInteger 2024)])`
- `[pat:Pattern {k:"v"}]` → `Subject (Just (Symbol "pat")) ["Pattern"] (fromList [("k", VString "v")])`

## Key Design Decisions

1. **Optional Identity**: All three components (identity, labels, properties) are optional to match grammar flexibility
2. **Labels as List**: Grammar allows multiple labels (`repeat1($._label)`), so use `[String]`
3. **PropertyRecord as Map**: Use `Data.Map` for efficient key-value lookups
4. **Value Type System**: Comprehensive sum type covering all gram value types
5. **Integration First**: Design Subject to work seamlessly as `Pattern Subject`

## Dependencies

- `base >=4.17.0.0 && <5`
- `containers ^>=0.6` (for Map)
- `hashable ^>=1.4` (for Hashable instance)
- `unordered-containers ^>=0.2` (for HashSet/HashMap if needed)

## Success Criteria

1. Subject type can represent all gram notation attribute structures
2. Subject works as Pattern value type (`Pattern Subject`)
3. All typeclass instances satisfy their laws
4. Comprehensive test coverage (similar to pattern library)
5. Clear documentation with gram notation examples
6. Library structure mirrors pattern library for consistency

## Implementation Todos

1. **subject-core-type**: Define core Subject data type with identity, labels, and properties fields in src/Subject/Core.hs
2. **subject-value-type**: Define Value type system supporting standard types (Int, Double, Bool, String) and extended types (tagged strings, arrays, maps, ranges) in src/Subject/Value.hs
3. **subject-identifier**: Define Identifier type (Symbol | StringLiteral | Integer) in src/Subject/Core.hs
4. **subject-typeclass-instances**: Implement Show, Eq, Ord, Hashable, Semigroup, and Monoid instances for Subject (depends on: subject-core-type, subject-value-type)
5. **subject-constructor-functions**: Implement constructor functions (subject, subjectWith) and property manipulation functions in src/Subject/Construction.hs (depends on: subject-core-type)
6. **subject-test-suite**: Create comprehensive test suite in tests/Spec/Subject/ covering construction, field accessors, typeclass instances, and edge cases (depends on: subject-core-type, subject-value-type)
7. **subject-cabal-config**: Create subject.cabal build configuration following pattern library structure and update cabal.project
8. **subject-documentation**: Add comprehensive Haddock documentation and examples/examples.md with gram notation mappings (depends on: subject-core-type, subject-value-type)

