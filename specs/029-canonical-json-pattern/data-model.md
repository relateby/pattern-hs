# Data Model: Canonical JSON Pattern Representation

**Feature**: 029-canonical-json-pattern  
**Date**: 2026-01-10

## Core Entities

### Canonical JSON Pattern

**Definition**: A standardized JSON representation of Pattern<Subject> that preserves all structural information and enables bidirectional conversion with gram notation.

**JSON Structure**:
```json
{
  "value": {
    "identity": "string",
    "labels": ["string"],
    "properties": {
      "key": <Value>
    }
  },
  "elements": [<Pattern>]
}
```

**Key Attributes**:
- **value**: Subject object containing the pattern's head/subject
- **elements**: Array of nested Pattern objects (recursive structure)

**Validation Rules**:
- `value` field is REQUIRED
- `elements` field is REQUIRED (may be empty array)
- All elements must conform to Pattern structure recursively
- Symbol may be empty string (anonymous subject)
- Labels array may be empty
- Properties object may be empty

**State Transitions**: N/A (immutable data structure)

**Relationships**:
- Contains Subject (value field)
- Contains array of Patterns (elements field, recursive)
- Serializes from Haskell `Pattern Subject` type
- Deserializes to Haskell `Pattern Subject` type

---

### Subject

**Definition**: Represents a graph node or relationship with identity (symbol), labels (types), and properties (attributes).

**JSON Structure**:
```json
{
  "identity": "string",
  "labels": ["string"],
  "properties": {
    "key": <Value>
  }
}
```

**Key Attributes**:
- **identity**: String identifier (may be empty for anonymous subjects)
- **labels**: Array of label strings (types/categories)
- **properties**: Object mapping property names to values

**Validation Rules**:
- `identity` is REQUIRED (string, may be empty)
- `labels` is REQUIRED (array, may be empty)
- `properties` is REQUIRED (object, may be empty)
- Labels must be unique within array
- Property keys must be unique within object

**Haskell Mapping**:
- `Subject { identity :: Symbol, labels :: Set String, properties :: Map String Value }`
- Set serializes to sorted array
- Map serializes to object with sorted keys (canonical form)

---

### Value (Discriminated Union)

**Definition**: Represents a property value with support for multiple types including primitives and complex structures.

**Simple Types** (native JSON):

```typescript
// Integer
42

// Decimal
3.14

// Boolean
true | false

// String
"hello world"
```

**Complex Types** (object with type discriminator):

```typescript
// Symbol
{
  "type": "symbol",
  "value": "identifier"
}

// Tagged String
{
  "type": "tagged",
  "tag": "json",
  "content": "{\"key\": \"value\"}"
}

// Range
{
  "type": "range",
  "lower": 1,
  "upper": 10
}

// Measurement
{
  "type": "measurement",
  "unit": "kg",
  "value": 5.5
}

// Array (recursive)
[<Value>, <Value>, ...]

// Map (recursive)
{
  "key1": <Value>,
  "key2": <Value>
}
```

**Validation Rules**:
- Simple types use native JSON validation
- Complex types REQUIRE `type` field for discrimination
- Symbol: `type` = "symbol", `value` = string
- Tagged String: `type` = "tagged", `tag` = string, `content` = string
- Range: `type` = "range", `lower` = number, `upper` = number, lower <= upper
- Measurement: `type` = "measurement", `unit` = string, `value` = number
- Arrays and Maps are recursive and may contain any Value type

**Haskell Mapping**:
```haskell
data Value
  = VInteger Integer
  | VDecimal Double
  | VBoolean Bool
  | VString String
  | VSymbol String
  | VTaggedString String String  -- tag, content
  | VArray [Value]
  | VMap (Map String Value)
  | VRange RangeValue
  | VMeasurement String Double   -- unit, value
```

---

### JSON Schema Document

**Definition**: A formal specification in JSON Schema Draft 2020-12 format that defines the structure and validation rules for Canonical JSON Pattern.

**Structure**:
```json
{
  "$schema": "http://json-schema.org/draft/2020-12/schema#",
  "$id": "https://gram.data/schemas/pattern/v0.1.0/pattern.json",
  "title": "Pattern<Subject>",
  "description": "Canonical JSON representation of Pattern with Subject",
  "version": "0.1.0",
  "definitions": {
    "Pattern": { ... },
    "Subject": { ... },
    "Value": { ... }
  },
  "$ref": "#/definitions/Pattern"
}
```

**Key Attributes**:
- **$schema**: JSON Schema version identifier
- **$id**: Unique URI for this schema
- **title**: Human-readable schema name
- **version**: Semantic version matching gramref version
- **definitions**: Reusable schema components
- **$ref**: Root schema reference

**Validation Rules**:
- Must conform to JSON Schema Draft 2020-12
- All definitions must be self-contained
- Recursive structures use `$ref` for circularity
- Version must follow semantic versioning

---

### TypeScript Type Definitions

**Definition**: Generated TypeScript interface definitions that provide type-safe access to Pattern<Subject> structure.

**Structure**:
```typescript
export interface Pattern {
  value: Subject;
  elements: Pattern[];
}

export interface Subject {
  identity: string;
  labels: string[];
  properties: { [key: string]: Value };
}

export type Value =
  | number
  | boolean
  | string
  | ValueSymbol
  | ValueTaggedString
  | ValueRange
  | ValueMeasurement
  | Value[]
  | { [key: string]: Value };

export interface ValueSymbol {
  type: "symbol";
  value: string;
}

export interface ValueTaggedString {
  type: "tagged";
  tag: string;
  content: string;
}

export interface ValueRange {
  type: "range";
  lower: number;
  upper: number;
}

export interface ValueMeasurement {
  type: "measurement";
  unit: string;
  value: number;
}
```

**Key Attributes**:
- Interfaces for structured types (Pattern, Subject)
- Discriminated union for Value type
- Type guards can be generated from discriminators
- Fully type-safe with TypeScript compiler

---

### Rust Type Definitions

**Definition**: Generated Rust struct definitions with serde derive macros for serialization/deserialization.

**Structure**:
```rust
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Pattern {
    pub value: Subject,
    pub elements: Vec<Pattern>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Subject {
    pub identity: String,
    pub labels: Vec<String>,
    pub properties: HashMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Value {
    Integer(i64),
    Decimal(f64),
    Boolean(bool),
    String(String),
    Symbol(ValueSymbol),
    TaggedString(ValueTaggedString),
    Range(ValueRange),
    Measurement(ValueMeasurement),
    Array(Vec<Value>),
    Map(HashMap<String, Value>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueSymbol {
    #[serde(rename = "type")]
    pub type_: String,  // Always "symbol"
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueTaggedString {
    #[serde(rename = "type")]
    pub type_: String,  // Always "tagged"
    pub tag: String,
    pub content: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueRange {
    #[serde(rename = "type")]
    pub type_: String,  // Always "range"
    pub lower: f64,
    pub upper: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueMeasurement {
    #[serde(rename = "type")]
    pub type_: String,  // Always "measurement"
    pub unit: String,
    pub value: f64,
}
```

**Key Attributes**:
- Derive macros for Debug, Clone, PartialEq, Serialize, Deserialize
- Untagged enum for Value (serde handles discrimination)
- HashMap for properties and map values
- Type field renamed to type_ (reserved keyword workaround)

---

## Semantic Equivalence Rules

**Definition**: Rules for determining when two patterns are semantically equivalent after roundtrip conversion.

**Equivalence Criteria**:

1. **Structural Equivalence**:
   - Pattern value must be equivalent
   - Pattern elements array must have same length
   - Each element must be equivalent at same position

2. **Subject Equivalence**:
   - Symbols must be equal (string comparison)
   - Labels sets must contain same elements (order independent)
   - Properties maps must have same keys and equivalent values

3. **Value Equivalence**:
   - Simple types: Direct equality
   - Symbols: Value strings must be equal
   - Tagged strings: Tag and content must be equal
   - Ranges: Lower and upper bounds must be equal
   - Measurements: Unit and value must be equal
   - Arrays: Same length, each element equivalent at same position
   - Maps: Same keys, each value equivalent for same key

4. **Format Independence**:
   - Whitespace in gram notation doesn't affect equivalence
   - Property order in JSON objects doesn't affect equivalence
   - Label order in JSON arrays doesn't affect equivalence (treated as sets)
   - Empty vs absent optional fields: Both valid (empty = absent)

**Test Expression**:
```haskell
roundtripEquivalent :: Pattern Subject -> Bool
roundtripEquivalent p = 
  case fromJSON (toJSON p) of
    Success p' -> p == p'  -- Uses Eq instance
    Error _ -> False
```

---

## Schema Versioning

**Definition**: Versioning scheme for JSON Schema and type definitions to support evolution.

**Version Format**: Semantic versioning (MAJOR.MINOR.PATCH)

**Version Rules**:
- **MAJOR**: Breaking changes (incompatible structure changes)
- **MINOR**: Backward-compatible additions (new optional fields, new value types)
- **PATCH**: Documentation updates, clarifications (no structure change)

**Schema Version Embedding**:
```json
{
  "version": "0.1.0",
  "$id": "https://gram.data/schemas/pattern/v0.1.0/pattern.json"
}
```

**Compatibility**:
- Old JSON validates against new schema (backward compatible)
- New JSON may not validate against old schema (forward compatible discouraged)
- Validation must specify schema version for reproducibility

---

## Examples

### Complete Pattern Example

**Gram Notation**:
```
(person:Person {name: "Alice", age: 30})
```

**Canonical JSON**:
```json
{
  "value": {
    "identity": "person",
    "labels": ["Person"],
    "properties": {
      "age": 30,
      "name": "Alice"
    }
  },
  "elements": []
}
```

### Nested Pattern Example

**Gram Notation**:
```
[graph:Graph {created: 2024-01-10} | (a:Node), (b:Node)]
```

**Canonical JSON**:
```json
{
  "value": {
    "identity": "graph",
    "labels": ["Graph"],
    "properties": {
      "created": "2024-01-10"
    }
  },
  "elements": [
    {
      "value": {
        "identity": "a",
        "labels": ["Node"],
        "properties": {}
      },
      "elements": []
    },
    {
      "value": {
        "identity": "b",
        "labels": ["Node"],
        "properties": {}
      },
      "elements": []
    }
  ]
}
```

### Complex Value Types Example

**Gram Notation**:
```
(sensor:Sensor {
  temp: 23.5°C,
  range: 10..20,
  type: `sensor`,
  config: json`{"interval": 1000}`
})
```

**Canonical JSON**:
```json
{
  "value": {
    "identity": "sensor",
    "labels": ["Sensor"],
    "properties": {
      "config": {
        "type": "tagged",
        "tag": "json",
        "content": "{\"interval\": 1000}"
      },
      "range": {
        "type": "range",
        "lower": 10,
        "upper": 20
      },
      "temp": {
        "type": "measurement",
        "unit": "°C",
        "value": 23.5
      },
      "type": {
        "type": "symbol",
        "value": "sensor"
      }
    }
  },
  "elements": []
}
```
