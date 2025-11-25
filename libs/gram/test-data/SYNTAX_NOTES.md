# Gram Notation Syntax Notes

Based on tree-sitter-gram grammar.js and test corpus.

## Top-Level Structure

```
gram: optional(record) + repeat(pattern)
```

- Optional top-level record: `{ k: v }`
- Zero or more patterns following

## Four Primary Structures

### 1. Top-Level Record (Module-Level Pattern Record)
- Optional record at root: `{ k: v }`
- Uses `:` or `::` as separator
- Example: `{ s : "a" }`

### 2. Subject (Square Brackets) - ONLY structure with nested elements
- Syntax: `[attributes | sub_pattern]`
- Attributes: identifier, labels, record (all optional)
- Sub-pattern after `|`: comma-separated list of subjects, paths (nodes/relationships), or references
- Examples:
  - `[ ]` - empty subject
  - `[ a ]` - named subject
  - `[ a:Subject ]` - named and labeled
  - `[ a:Subject { title: "Generic Subject" } ]` - with record
  - `[ devrel:Team {name : "Developer Relations"} | abk, adam, alex, alexy ]` - with nested elements

### 3. Node (Round Parentheses) - Atomic patterns
- Syntax: `(identifier? labels? record?)`
- NO nested elements inside parentheses
- Examples:
  - `()` - empty node
  - `({})` - node with empty record
  - `({ k : "v" })` - node with record
  - `(player1 { named : "it" })` - identified with record
  - `(player1:Player { named : "it" })` - identified, labeled, with record

### 4. Relationship (Connecting Nodes)
- Syntax: `(node)-[attributes?]->(path)`
- Relationship attributes in square brackets (optional)
- Can chain: `()-->()-->()` is a single relationship element
- Arrow types:
  - `-->` right arrow
  - `<--` left arrow
  - `<-->` bidirectional
  - `--` undirected
  - Also: `=>`, `<=`, `<=>`, `==`, `~>`, `<~`, `<~>`, `~~`
- Examples:
  - `()-->()` - simple relationship
  - `()-->()-->()` - two-hop path (nested relationship)
  - `(a)-[r:Rel {k:"v"}]->(b)` - relationship with attributes

## Value Types

### Standard Values
- Integer: `1`, `-5`
- Decimal: `3.14`, `-0.5`
- Boolean: `true`, `false`
- String: `"text"`, `'text'`, `` `text` ``, ` ```fenced``` `
- Symbol: `symbol_name`

### Extended Values
- Tagged string: `tag`content`` or ` ```tag\ncontent\n``` `
- Array: `[value1, value2]` - contains scalar values only
- Map: `{k1: v1, k2: v2}` - contains scalar values only (no nested maps/arrays)
- Range: `1..10`, `1...`, `...100`
- Measurement: `70.5kg`, `-10m`

## Important Constraints

1. **Nodes cannot have nested elements**: `(g (a:Person))` is INVALID
   - Use `[g | (a:Person)]` for nested structure
   - Use `(g), (a:Person)` for separate nodes

2. **Only subjects can have nested elements**: After the `|` in `[attributes | sub_pattern]`

3. **Maps cannot be nested**: Maps only accept scalar values

4. **Arrays cannot contain maps**: Arrays only accept scalar values

5. **Records use `:` or `::`**: Property separator
6. **Maps use `:`**: Mapping separator

## Pattern Structure

- Patterns contain comma-separated elements
- Elements can be: subjects, nodes, relationships
- Relationships can chain (nested relationships)
- Example: `(),(), ()-->(), ()-->()-->()` - pattern with multiple elements

