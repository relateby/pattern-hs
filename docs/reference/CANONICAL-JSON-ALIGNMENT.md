# Canonical JSON Alignment: gram-rs vs pattern-hs

**Date**: January 9, 2026  
**Status**: ⚠️ **MISALIGNMENTS FOUND**  
**Priority**: P1 - Must align before Phase 7 completion

---

## 🎯 Goal

Ensure gram-rs AST output matches the **canonical JSON format** defined by pattern-hs (spec 029-canonical-json-pattern) to enable interoperability across the gram ecosystem.

---

## 📊 Comparison: pattern-hs Canonical Format vs gram-rs AST

### Pattern Structure

| Field | pattern-hs (Canonical) | gram-rs (Current) | Status |
|-------|---------------------|-------------------|--------|
| Pattern value | `value` | `subject` | ❌ **MISMATCH** |
| Elements | `elements` | `elements` | ✅ Match |

### Subject Structure

| Field | pattern-hs (Canonical) | gram-rs (Current) | Status |
|-------|---------------------|-------------------|--------|
| Identity | `symbol` | `identity` | ❌ **MISMATCH** |
| Labels | `labels` | `labels` | ✅ Match |
| Properties | `properties` | `properties` | ✅ Match |

### Value Type Discriminators

| Type | pattern-hs (Canonical) | gram-rs (Current) | Status |
|------|---------------------|-------------------|--------|
| Symbol | `{"type": "symbol", "value": "..."}` | `{"type": "Symbol", "value": "..."}` | ❌ **MISMATCH** (case) |
| Tagged String | `{"type": "tagged", "tag": "...", "content": "..."}` | `{"type": "Tagged", "tag": "...", "content": "..."}` | ❌ **MISMATCH** (case) |
| Range | `{"type": "range", "lower": n, "upper": n}` | `{"type": "Range", "lower": n, "upper": n}` | ❌ **MISMATCH** (case) |
| Measurement | `{"type": "measurement", "unit": "...", "value": n}` | `{"type": "Measurement", "unit": "...", "value": n}` | ❌ **MISMATCH** (case) |

### Simple Types

| Type | pattern-hs | gram-rs | Status |
|------|---------|---------|--------|
| Integer | Native JSON `number` | Tagged `{"type": "Integer", "value": n}` | ⚠️ **DIFFERENT APPROACH** |
| Decimal | Native JSON `number` | Tagged `{"type": "Decimal", "value": n}` | ⚠️ **DIFFERENT APPROACH** |
| Boolean | Native JSON `boolean` | Native JSON `boolean` | ✅ Match |
| String | Native JSON `string` | Native JSON `string` | ✅ Match |
| Array | Native JSON `array` | Native JSON `array` | ✅ Match |
| Map | Native JSON `object` | Native JSON `object` | ✅ Match |

---

## 🔍 Detailed Differences

### 1. Pattern Field Name: `value` vs `subject`

**pattern-hs**:
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

**gram-rs (current)**:
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

**Impact**: High - This is a structural difference that breaks interoperability.

---

### 2. Subject Identity Field: `symbol` vs `identity`

**pattern-hs (CURRENT - CORRECT)**:
```json
{
  "identity": "alice",
  "labels": ["Person"],
  "properties": {}
}
```

**gram-rs (current)**:
```json
{
  "identity": "alice",
  "labels": ["Person"],
  "properties": {}
}
```

**Status**: ✅ **ALIGNED** - Both implementations now use `"identity"`

---

### 3. Value Type Discriminator Case: Lowercase vs Capitalized

**pattern-hs**:
```json
{
  "type": "symbol",
  "value": "user123"
}
```

**gram-rs (current)**:
```json
{
  "type": "Symbol",
  "value": "user123"
}
```

**Impact**: Medium - Type discrimination will fail if consumers check for lowercase.

---

### 4. Integer/Decimal Serialization: Native vs Tagged

**pattern-hs** (from schema):
- Integer: Native JSON number (e.g., `42`)
- Decimal: Native JSON number (e.g., `3.14`)

**gram-rs (current)**:
- Integer: Tagged `{"type": "Integer", "value": 42}`
- Decimal: Tagged `{"type": "Decimal", "value": 3.14}`

**Impact**: High - This is a fundamental difference in approach. pattern-hs uses native JSON for numbers, gram-rs tags them.

**Note**: Our design decision document says "mixed approach" but pattern-hs uses pure native for numbers. Need to align.

---

## ✅ What We Got Right

1. ✅ **Tagged String Structure**: `tag` and `content` fields match perfectly
2. ✅ **Range Structure**: `lower` and `upper` fields match
3. ✅ **Measurement Structure**: `unit` and `value` fields match
4. ✅ **Array/Map Recursion**: Both use native JSON arrays/objects
5. ✅ **Boolean/String**: Both use native JSON types

---

## 📋 Required Changes

### High Priority (Breaking Changes)

1. **Rename `subject` → `value`** in `AstPattern`
   - Update struct field
   - Update all references
   - Update documentation

2. ~~**Rename `identity` → `symbol`** in `AstSubject`~~ **ALREADY CORRECT**
   - ✅ Both pattern-hs and gram-rs use `"identity"`
   - No changes needed

3. **Change Integer/Decimal to native JSON**
   - Remove tagging for integers and decimals
   - Use native JSON numbers
   - Update value conversion logic

### Medium Priority (Type Discriminator Case)

4. **Lowercase type discriminators**
   - `"Symbol"` → `"symbol"`
   - `"Tagged"` → `"tagged"`
   - `"Range"` → `"range"`
   - `"Measurement"` → `"measurement"`

### Low Priority (Documentation)

5. **Update examples** to show canonical format
6. **Update READMEs** to reference canonical format
7. **Add validation** against pattern-hs JSON schema

---

## 🧪 Testing Strategy

After changes:

1. **Round-trip test**: gram-rs JSON → pattern-hs parser → gram-rs parser
2. **Schema validation**: Validate gram-rs JSON against pattern-hs schema
3. **Example comparison**: Compare outputs for same gram input

---

## 📚 References

- **pattern-hs JSON Schema**: `../pattern-hs/specs/029-canonical-json-pattern/contracts/json-schema.json`
- **pattern-hs Spec**: `../pattern-hs/specs/029-canonical-json-pattern/spec.md`
- **pattern-hs Data Model**: `../pattern-hs/specs/029-canonical-json-pattern/data-model.md`
- **pattern-hs TypeScript Types**: `../pattern-hs/specs/029-canonical-json-pattern/contracts/typescript-types.ts`

---

## 🎯 Recommendation

**Action**: Align gram-rs AST output with pattern-hs canonical format **before** Phase 7 completion.

**Rationale**:
- Interoperability is a core goal
- pattern-hs is the reference implementation
- Breaking changes now are easier than later
- Downstream projects (gram-js, gram-py) will depend on canonical format

**Estimated Effort**: 2-3 hours
- Field renames: 30 minutes
- Value serialization changes: 1 hour
- Type discriminator case: 30 minutes
- Testing and validation: 1 hour

---

**Status**: ⚠️ **PARTIAL ALIGNMENT - pattern-hs contracts updated**
**Priority**: P1
**Blocks**: Phase 7 completion, gram-js/gram-py development

**Update (2026-01-29)**: pattern-hs implementation and contracts now consistently use `"identity"`. The original analysis was based on outdated JSON Schema. gram-rs should align with this correct format.
