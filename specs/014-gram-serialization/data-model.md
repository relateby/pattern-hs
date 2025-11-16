# Data Model: Gram Serialization Library

**Feature**: 014-gram-serialization  
**Date**: 2025-01-27

## Core Entities

### Pattern Subject

**Definition**: A Pattern data structure with Subject values, representing the primary data type for serialization and parsing operations.

**Structure**: 
- Inherited from Pattern library: `Pattern Subject` where `Pattern v = Pattern { value :: v, elements :: [Pattern v] }`
- The `value` field contains a `Subject` (identity, labels, properties)
- The `elements` field contains a list of `Pattern Subject` (enabling nested patterns)

**Key Attributes**:
- **value**: Subject decoration containing identity (Symbol), labels (Set String), and properties (PropertyRecord)
- **elements**: List of Pattern Subject elements forming the pattern sequence

**Validation Rules**:
- All elements in a Pattern Subject must also be Pattern Subject (type consistency)
- Subject identity is always required (empty Symbol "" represents anonymous subjects)
- Labels are a set (no duplicates, order doesn't matter)
- Properties are a map (key-value pairs, keys are strings)

**State Transitions**: N/A (immutable data structure)

**Relationships**:
- Contains Subject (value field)
- Contains list of Pattern Subject (elements field, recursive)
- Serializes to Gram Notation (via toGram function)
- Parses from Gram Notation (via fromGram function)

### Gram Notation

**Definition**: Text format representation of Pattern Subject structures, following the gram notation specification.

**Structure**: String representation with syntax for:
- Subjects: `(identity:labels {properties})` or `(identity)` or `(:labels)` or `()` (anonymous)
- Properties: `{key: value, key2: value2}`
- Values: Standard types (integers, decimals, booleans, strings, symbols) and extended types (tagged strings, arrays, maps, ranges, measurements)
- Patterns: Nested structures, relationships (e.g., `(a)-[r:KNOWS]->(b)`)

**Key Attributes**:
- Text format (String)
- Syntax follows gram notation specification
- Supports comments (`//` line comments, end-of-line comments)

**Validation Rules**:
- Must be valid gram notation syntax (parsable by tree-sitter-gram)
- Must match syntax features supported by tree-sitter-gram

**State Transitions**: N/A (text format, immutable)

**Relationships**:
- Serialized from Pattern Subject (via toGram function)
- Parsed to Pattern Subject (via fromGram function)

### ParseError

**Definition**: Error type representing parsing failures during gram notation parsing.

**Structure**:
```haskell
data ParseError = ParseError String
  deriving (Eq, Show)
```

**Key Attributes**:
- **message**: String containing error description, position information (if available), and context

**Validation Rules**:
- Error messages must be clear and actionable
- Error messages should include context (what was expected, what was found)

**State Transitions**: N/A (error type, immutable)

**Relationships**:
- Returned by fromGram function when parsing fails
- Used in error handling and reporting

### Value Types

**Definition**: Rich value system supporting standard and extended types for property records.

**Structure**: Inherited from Subject library, `Value` type with constructors:
- Standard types: `VInteger Integer`, `VDecimal Double`, `VBoolean Bool`, `VString String`, `VSymbol String`
- Extended types: `VTaggedString String String`, `VArray [Value]`, `VMap (Map String Value)`, `VRange RangeValue`, `VMeasurement String Double`

**Key Attributes**:
- Each value type has specific serialization format in gram notation
- Values can be nested (arrays and maps contain other values)

**Validation Rules**:
- Values must match their type constraints (e.g., integers are Integer, decimals are Double)
- Nested values must be valid (arrays contain valid Values, maps have valid keys and values)

**State Transitions**: N/A (immutable value types)

**Relationships**:
- Used in Subject properties (PropertyRecord = Map String Value)
- Serialized to gram notation value syntax
- Parsed from gram notation value syntax

## Data Flow

### Serialization Flow

```
Pattern Subject
  → toGram function
  → Gram Notation (String)
```

**Process**:
1. Extract Subject from Pattern value field
2. Serialize Subject (identity, labels, properties) to gram notation
3. Serialize Pattern elements (recursively) to gram notation
4. Combine into complete gram notation string

### Parsing Flow

```
Gram Notation (String)
  → fromGram function
  → Either ParseError (Pattern Subject)
```

**Process**:
1. Parse gram notation string using selected parser library
2. Build Pattern Subject structure (Subject value, Pattern elements)
3. Return Either ParseError (Pattern Subject)
4. Handle errors with clear error messages

### Round-Trip Flow

```
Pattern Subject
  → toGram
  → Gram Notation
  → fromGram
  → Pattern Subject (should match original)
```

**Verification**:
- Structure preserved (Pattern elements, Subject components)
- Values preserved (all value types correctly preserved)
- Formatting may vary (whitespace, comments not preserved)

## Validation Rules

### Serialization Validation

- All Pattern Subject structures must serialize to valid gram notation
- All value types must serialize to correct gram notation syntax
- Anonymous subjects (empty Symbol) must serialize to anonymous gram notation syntax
- Nested patterns must serialize with correct nesting structure
- Relationship patterns must serialize with correct relationship syntax

### Parsing Validation

- All valid gram notation strings must parse to Pattern Subject
- Invalid gram notation must return ParseError with clear message
- Anonymous subjects in gram notation must parse to empty Symbol
- All value types must parse to correct Value constructors
- Nested patterns must parse to correct Pattern element structure

### Round-Trip Validation

- Serialize then parse must preserve Pattern Subject structure
- Serialize then parse must preserve all value types
- Structure and values must be preserved (formatting may vary)
- Edge cases (deep nesting, large property records) must handle correctly

## Edge Cases

### Deep Nesting

- **Scenario**: Pattern Subject with 100+ levels of nesting
- **Handling**: Recursive serialization/parsing must handle deep nesting without stack overflow
- **Validation**: Test with maximum nesting depth

### Large Property Records

- **Scenario**: Pattern Subject with 1000+ properties
- **Handling**: Serialization/parsing must handle large property records efficiently
- **Validation**: Test with maximum property count

### Special Characters

- **Scenario**: Strings, symbols, or property keys with special characters (quotes, escapes, Unicode)
- **Handling**: Proper escaping and quoting in serialization, correct unescaping in parsing
- **Validation**: Test with various special character combinations

### Anonymous Subjects

- **Scenario**: Gram notation with anonymous subjects (no identity)
- **Handling**: Parse to empty Symbol, serialize empty Symbol to anonymous syntax
- **Validation**: Test round-trip conversion for anonymous subjects

### Empty Structures

- **Scenario**: Empty patterns, empty property records, empty label sets
- **Handling**: Serialize/parse empty structures correctly
- **Validation**: Test all empty structure combinations

