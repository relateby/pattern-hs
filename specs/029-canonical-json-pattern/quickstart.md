# Quickstart: Canonical JSON Pattern Representation

**Feature**: 029-canonical-json-pattern  
**Date**: 2026-01-10

## Overview

This guide provides quick examples of using the canonical JSON representation of Pattern<Subject> with the gramref CLI tool.

## Prerequisites

- gramref CLI installed (version 0.2.0+)
- Basic familiarity with gram notation

## Basic Usage

### Convert Gram Notation to JSON

```bash
# Convert a simple pattern
echo "(node:Person {name: \"Alice\"})" | gramref parse --format json

# Output (formatted):
{
  "Meta": {
    "Command": "parse",
    "Hash": "...",
    "Timestamp": "2026-01-10T...",
    "Version": "0.1.0"
  },
  "Result": {
    "Type": "Pattern",
    "Value": {
      "value": {
        "symbol": "node",
        "labels": ["Person"],
        "properties": {
          "name": "Alice"
        }
      },
      "elements": []
    }
  }
}
```

### Get Clean JSON Output (No Metadata)

```bash
# Use --value-only flag for just the pattern data
echo "(node:Person {name: \"Alice\"})" | gramref parse --format json --value-only

# Output:
{
  "value": {
    "identity": "node",
    "labels": ["Person"],
    "properties": {
      "name": "Alice"
    }
  },
  "elements": []
}
```

### Canonical JSON Output

```bash
# Use --canonical flag for deterministic, sorted output
echo "(node:Person {name: \"Alice\"})" | gramref parse --format json --canonical --value-only

# Output will have sorted keys at all nesting levels
```

## Schema Generation

### Generate JSON Schema

```bash
# Generate JSON Schema for Pattern<Subject>
gramref schema --format json-schema

# Save to file
gramref schema --format json-schema > pattern-schema.json
```

### Generate TypeScript Types

```bash
# Generate TypeScript interface definitions
gramref schema --format typescript > pattern.ts

# The output includes type guards and helper functions
```

### Generate Rust Types

```bash
# Generate Rust structs with serde annotations
gramref schema --format rust > pattern.rs

# Ready to use in your Rust project with serde
```

## Roundtrip Conversion

### Gram → JSON → Gram

```bash
# Original gram notation
cat > input.gram << 'EOF'
(person:Person {name: "Alice", age: 30})
EOF

# Convert to JSON
gramref parse input.gram --format json --value-only > pattern.json

# Convert back to gram notation
gramref convert pattern.json --from json --to gram

# Output: (person:Person {age: 30, name: "Alice"})
# Note: Property order may change, but semantics are preserved
```

### Verify Roundtrip Integrity

```bash
# Generate test data
echo "(a)-[:KNOWS]->(b)" > test.gram

# Roundtrip and compare
gramref parse test.gram --format json --value-only | \
  gramref convert --from json --to gram > roundtrip.gram

# Check semantic equivalence (implementation-specific)
diff <(gramref parse test.gram --value-only --canonical) \
     <(gramref parse roundtrip.gram --value-only --canonical)
```

## Working with Complex Patterns

### Nested Patterns

```bash
# Pattern with nested elements
cat > nested.gram << 'EOF'
[graph:Graph {created: "2026-01-10"} | 
  (a:Node), 
  (b:Node), 
  (a)-[:CONNECTED]->(b)
]
EOF

gramref parse nested.gram --format json --value-only
```

### Complex Value Types

```bash
# Pattern with various value types
cat > complex.gram << 'EOF'
(sensor:Sensor {
  temp: 23.5°C,
  range: 10..20,
  type: `sensor`,
  active: true,
  readings: [1, 2, 3],
  metadata: json`{"interval": 1000}`
})
EOF

gramref parse complex.gram --format json --value-only
```

Output shows different value representations:
- Numbers: `23.5`
- Booleans: `true`
- Strings: `"value"`
- Symbols: `{"type": "symbol", "value": "..."}`
- Tagged strings: `{"type": "tagged", "tag": "json", "content": "..."}`
- Ranges: `{"type": "range", "lower": 10, "upper": 20}`
- Measurements: `{"type": "measurement", "unit": "°C", "value": 23.5}`
- Arrays: `[1, 2, 3]`

## Validation

### Validate JSON Against Schema

```bash
# Generate schema
gramref schema --format json-schema > schema.json

# Parse pattern to JSON
echo "(node:Person)" | gramref parse --value-only > pattern.json

# Validate using external JSON Schema validator
jsonschema --instance pattern.json --schema schema.json

# Or use gramref's built-in validation (if implemented)
gramref validate pattern.json --schema schema.json
```

### Validate Port Implementation Output

```bash
# Compare your implementation's output with gramref
your-gram-tool parse input.gram --json > your-output.json
gramref parse input.gram --value-only --canonical > reference-output.json

# Exact comparison (if using canonical format)
diff your-output.json reference-output.json

# Or validate against schema
gramref validate your-output.json --schema schema.json
```

## Using Generated Types

### TypeScript Example

```typescript
import { Pattern, Subject, Value } from './pattern';

// Type-safe pattern construction
const pattern: Pattern = {
  value: {
    identity: "person",
    labels: ["Person"],
    properties: {
      name: "Alice",
      age: 30
    }
  },
  elements: []
};

// Parse JSON with type safety
const json = '{"value": {"identity": "test", "labels": [], "properties": {}}, "elements": []}';
const parsed: Pattern = JSON.parse(json);
```

### Rust Example

```rust
use serde_json;
mod pattern; // From generated pattern.rs

fn main() {
    // Parse JSON into Pattern
    let json = r#"{
        "value": {
            "identity": "person",
            "labels": ["Person"],
            "properties": {}
        },
        "elements": []
    }"#;
    
    let pattern: pattern::Pattern = serde_json::from_str(json).unwrap();
    println!("Identity: {}", pattern.value.identity);
    
    // Serialize Pattern to JSON
    let json_out = serde_json::to_string_pretty(&pattern).unwrap();
    println!("{}", json_out);
}
```

## Common Patterns

### Batch Processing

```bash
# Process multiple files
for file in *.gram; do
    gramref parse "$file" --format json --value-only > "${file%.gram}.json"
done
```

### Streaming JSON

```bash
# Generate test data and convert
gramref generate --type pattern --count 10 | \
  while read -r pattern; do
    echo "$pattern" | gramref parse --format json --value-only
  done
```

### Schema Validation Pipeline

```bash
# Complete validation workflow
gramref schema --format json-schema > schema.json

# Validate all JSON files
for json in *.json; do
    echo "Validating $json..."
    if gramref validate "$json" --schema schema.json; then
        echo "✓ Valid"
    else
        echo "✗ Invalid"
    fi
done
```

## Performance Tips

1. **Use --value-only** when you don't need metadata (faster, smaller output)
2. **Use --canonical** only when needed for comparison (slightly slower due to sorting)
3. **Batch operations** when possible to amortize startup costs
4. **Stream processing** for large datasets instead of loading all into memory

## Troubleshooting

### Issue: JSON output has changing timestamps

**Solution**: Use `--deterministic` flag for fixed timestamps and hashes

```bash
gramref parse input.gram --deterministic
```

### Issue: Property order differs between runs

**Solution**: Use `--canonical` flag to sort keys consistently

```bash
gramref parse input.gram --canonical
```

### Issue: Roundtrip changes format

**Solution**: This is expected. Semantic content is preserved, formatting may change. Use semantic comparison:

```bash
# Compare canonical JSON instead of gram text
diff <(gramref parse input1.gram --canonical --value-only) \
     <(gramref parse input2.gram --canonical --value-only)
```

### Issue: Schema validation fails

**Solution**: Ensure you're using the correct schema version and format

```bash
# Check schema version matches your gramref version
gramref --version
grep "version" schema.json
```

## Next Steps

- Read the full [data model documentation](./data-model.md) for detailed format specification
- Review [example contracts](./contracts/) for complete schema and type definitions
- Explore the [implementation plan](./plan.md) for technical details
- Check the [specification](./spec.md) for user stories and requirements

## Getting Help

- CLI help: `gramref help` or `gramref schema --help`
- Report issues: [GitHub Issues](https://github.com/gram-data/gram-hs/issues)
- Documentation: [gram.data](https://gram.data)
