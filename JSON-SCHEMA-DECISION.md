# JSON Schema Decision Analysis: pattern-hs vs gram-rs

**Date**: 2026-01-29
**Status**: ✅ ALIGNED - Documentation Bug Found
**Priority**: P0 - Blocks Interoperability

---

## Executive Summary

**CRITICAL FINDING**: pattern-hs documentation and implementation are **misaligned**!

- **pattern-hs documentation** (Gram/JSON.hs lines 14-18) says: `"value"` and `"symbol"`
- **pattern-hs implementation** (Gram/JSON.hs lines 52-63) uses: `"subject"` and `"identity"`
- **gram-rs implementation** uses: `"subject"` and `"identity"`

**Result**: gram-rs is **already aligned** with pattern-hs's actual implementation. The issue is a documentation bug in pattern-hs, not a format mismatch.

---

## Actual Current State

### pattern-hs Implementation (ACTUAL)
```haskell
-- From Gram/JSON.hs lines 52-55
patternToValue (Pattern.Pattern v es) = object
  [ "subject" .= subjectToValue v
  , "elements" .= map patternToValue es
  ]

-- From Gram/JSON.hs lines 59-63
subjectToValue (Subject.Subject ident labels props) = object
  [ "identity" .= symbolToValue ident
  , "labels" .= toJSON (Set.toList labels)
  , "properties" .= propsToValue props
  ]
```

### pattern-hs Documentation (INCORRECT)
```haskell
-- From Gram/JSON.hs lines 14-18
-- Patterns are represented as:
-- > { "value": Subject, "elements": [Pattern] }
-- Subjects are represented as:
-- > { "symbol": String, "labels": [String], "properties": {...} }
```

### gram-rs Implementation (ALIGNED)
```rust
// Already uses "subject" and "identity" - matches pattern-hs implementation!
```

---

## Historical Context

### 1. Pattern Field Name: `value` vs `subject`

#### Documentation Says: `"value"`
```json
{
  "value": {
    "symbol": "alice",
    "labels": ["Person"],
    "properties": {}
  },
  "elements": []
}
```

#### Both Implementations Use: `"subject"`
```json
{
  "subject": {
    "identity": "alice",
    "labels": ["Person"],
    "properties": {}
  },
  "elements": []
}
```

#### Analysis

**Consideration 1: Least Surprising Mapping from Gram Notation**

Gram notation: `(alice:Person {name: "Alice"})`

- ✅ **pattern-hs (`value`)**: Aligns with Pattern conceptual model where "value provides decoration about the elements"
- ❌ **gram-rs (`subject`)**: Introduces graph-specific terminology not present in gram notation
- **Winner**: pattern-hs - `value` is more neutral and matches the Pattern abstraction

**Consideration 2: Least Surprising Mapping to User Code**

In Haskell: `Pattern { value :: Subject, elements :: [Pattern] }`  
In Rust: `Pattern<V> { value: V, elements: Vec<Pattern<V>> }`

- ✅ **pattern-hs (`value`)**: Direct 1:1 mapping to code field names
- ❌ **gram-rs (`subject`)**: Introduces semantic mismatch - code says `value`, JSON says `subject`
- **Winner**: pattern-hs - field names match code structure exactly

**Verdict**: ✅ **pattern-hs approach is superior**

---

### 2. Subject Identity Field: `symbol` vs `identity`

#### pattern-hs Approach: `"symbol"`
```json
{
  "symbol": "alice",
  "labels": ["Person"],
  "properties": {}
}
```

#### gram-rs Approach: `"identity"`
```json
{
  "identity": "alice",
  "labels": ["Person"],
  "properties": {}
}
```

#### Analysis

**Consideration 1: Least Surprising Mapping from Gram Notation**

Gram notation: `(alice:Person)`

- ⚠️ **pattern-hs (`symbol`)**: Exposes type name rather than semantic meaning
- ✅ **gram-rs (`identity`)**: Reflects semantic meaning - this is the identity of the subject
- **Winner**: gram-rs - field names should reflect semantics, not types

**Consideration 2: Least Surprising Mapping to User Code**

In Haskell: `Subject { identity :: Symbol, ... }`
In Rust: `Subject { identity: Symbol, ... }`

- ❌ **pattern-hs (`symbol`)**: Exposes internal type name rather than field name
- ✅ **gram-rs (`identity`)**: Matches actual field name in both implementations
- **Winner**: gram-rs - direct field name mapping

**Verdict**: ✅ **gram-rs approach is superior - use `identity`**

**Action**: pattern-hs should change `symbol` → `identity` (see pattern-hs-JSON-ALIGNMENT.todo.md)

---

### 3. Number Serialization: Native vs Tagged

#### pattern-hs Approach: Native JSON Numbers
```json
{
  "properties": {
    "age": 30,
    "temperature": 98.6
  }
}
```

#### gram-rs Approach: Tagged Objects
```json
{
  "properties": {
    "age": {"type": "Integer", "value": 30},
    "temperature": {"type": "Decimal", "value": 98.6}
  }
}
```

#### Analysis

**Consideration 1: Least Surprising Mapping from Gram Notation**

Gram notation: `{age: 30, temperature: 98.6}`

- ✅ **pattern-hs (native)**: Direct mapping - what you see is what you get
- ❌ **gram-rs (tagged)**: Adds ceremony not present in gram notation
- **Winner**: pattern-hs - simpler, more intuitive

**Consideration 2: Least Surprising Mapping to User Code**

When working with JSON in code:
```javascript
// pattern-hs approach
const age = pattern.value.properties.age;  // 30

// gram-rs approach  
const age = pattern.subject.properties.age.value;  // 30
```

- ✅ **pattern-hs (native)**: Natural property access, works with standard JSON tools
- ❌ **gram-rs (tagged)**: Requires unwrapping, breaks JSON tooling expectations
- **Winner**: pattern-hs - much better developer experience

**Technical Consideration: Type Preservation**

- **pattern-hs**: Loses integer/decimal distinction (JSON limitation)
  - `2.0` serializes as `2`
  - Handles via semantic equivalence during parsing
- **gram-rs**: Preserves exact type information
  - Can distinguish `Integer(2)` from `Decimal(2.0)`
  - But adds complexity for marginal benefit

**Verdict**: ✅ **pattern-hs approach is strongly superior**

The type preservation benefit of gram-rs doesn't justify the complexity cost. JSON's number ambiguity is well-understood and handled via semantic equivalence.

---

### 4. Type Discriminator Case: Lowercase vs Capitalized

#### pattern-hs Approach: Lowercase
```json
{
  "type": "symbol",
  "value": "identifier"
}
```

#### gram-rs Approach: Capitalized
```json
{
  "type": "Symbol",
  "value": "identifier"
}
```

#### Analysis

**Consideration 1: Least Surprising Mapping from Gram Notation**

Gram notation doesn't expose type discriminators directly.

- ✅ **pattern-hs (lowercase)**: Follows JSON/JavaScript conventions
- ❌ **gram-rs (capitalized)**: Follows Rust enum conventions
- **Winner**: pattern-hs - JSON is the target format

**Consideration 2: Least Surprising Mapping to User Code**

JSON consumers expect lowercase discriminators:
```typescript
// Standard JSON convention
if (value.type === "symbol") { ... }

// Rust-specific convention (unusual in JSON)
if (value.type === "Symbol") { ... }
```

- ✅ **pattern-hs (lowercase)**: Matches JSON/TypeScript/JavaScript conventions
- ❌ **gram-rs (capitalized)**: Leaks Rust implementation detail
- **Winner**: pattern-hs - better cross-language compatibility

**Verdict**: ✅ **pattern-hs approach is superior**

---

## Additional Considerations

### pattern-hs Documentation Quality

The pattern-hs canonical JSON format has:
- ✅ Comprehensive specification document
- ✅ Formal JSON Schema (Draft 2020-12)
- ✅ Generated TypeScript types
- ✅ Generated Rust types
- ✅ 35+ unit tests + 200 QuickCheck properties
- ✅ Corpus validation

This level of documentation and tooling makes it the **de facto standard**.

### Ecosystem Impact

Choosing pattern-hs format means:
- ✅ Compatibility with existing pattern-hs tooling
- ✅ Compatibility with future language ports
- ✅ Leveraging existing JSON Schema for validation
- ✅ Using proven, battle-tested format

---

## Detailed Comparison Matrix

| Aspect | pattern-hs | gram-rs | Winner | Rationale |
|--------|---------|---------|--------|-----------|
| **Pattern field name** | `value` | `subject` | pattern-hs | Matches code structure, neutral terminology |
| **Subject identity field** | `symbol` | `identity` | **gram-rs** | Semantic meaning over type name, matches field names |
| **Number serialization** | Native JSON | Tagged objects | pattern-hs | Simpler, better DX, standard JSON |
| **Type discriminators** | lowercase | Capitalized | pattern-hs | JSON conventions, cross-language |
| **Documentation** | Comprehensive | Minimal | pattern-hs | Formal spec, schema, types |
| **Tooling** | Schema + generators | None | pattern-hs | JSON Schema, TS/Rust types |
| **Testing** | 235+ tests | Basic | pattern-hs | Extensive validation |
| **Ecosystem** | Reference impl | Port | pattern-hs | Established standard |

---

## Recommendation

### ✅ Hybrid Approach (Co-Design Decision)

**Rationale**:

1. **Mostly pattern-hs**: Reference implementation with proven tooling
2. **Better Mapping from Gram**: Simpler, more intuitive (native JSON numbers)
3. **Better Developer Experience**: Standard JSON conventions
4. **Semantic Field Names**: Use `identity` not `symbol` (gram-rs is correct here)
5. **Ecosystem Compatibility**: Coordinate changes across both implementations

### Migration Path

#### For gram-rs (P0):
1. Rename `subject` → `value` in AstPattern
2. **Keep** `identity` field name (gram-rs is correct)
3. Change to native JSON numbers (remove Integer/Decimal tagging)
4. Lowercase type discriminators

#### For pattern-hs (P0):
1. Rename `symbol` → `identity` in Subject JSON serialization
2. Update JSON Schema, TypeScript types, Rust types
3. Update all documentation and examples
4. See **pattern-hs-JSON-ALIGNMENT.todo.md** for detailed tasks

#### Coordination (P0):
1. Complete pattern-hs changes first
2. Then align gram-rs to updated canonical format
3. Cross-validate with shared test suite
4. Update alignment documentation

---

## Final Decision

**DECISION**: Hybrid approach - mostly pattern-hs, but use `identity` field name

**Justification**:
- pattern-hs approach is superior for: pattern field name, number serialization, type discriminators
- gram-rs approach is superior for: subject identity field name (semantic over type)
- Both implementations should align to this hybrid canonical format
- Co-design allows us to fix issues in either project

**Action Items**:
1. ✅ Created pattern-hs-JSON-ALIGNMENT.todo.md for pattern-hs changes
2. ✅ Updated ALIGNMENT.todo.md Phase 0 for gram-rs changes
3. Next: Implement pattern-hs changes first
4. Then: Align gram-rs to updated canonical format
5. Finally: Cross-validate with shared test suite

---

**Decision Date**: 2026-01-29
**Decision Maker**: Co-design (pattern-hs + gram-rs teams)
**Status**: ✅ Approved
