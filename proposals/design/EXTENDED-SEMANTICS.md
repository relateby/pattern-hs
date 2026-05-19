# Gram Extended Semantics: Pattern and Path Notation

**Status**: ✅ Implemented  
**Implementation**: Gram serialization and parsing support both pattern and path notation  
**Reference**: See `docs/reference/semantics/gram-semantics.md` for current specification

## Overview

Gram notation supports two complementary syntaxes:
- **Pattern notation**: Declarative nested structures using `[...]`
- **Path notation**: Sequential graph traversals using `(nodes)` and relationships

Both syntaxes can be mixed, with clear rules for definition and reference.

## Core Concepts

### Identity vs Type
- **Identifiers** (e.g., `a`, `k`) denote specific instances - must be unique
- **Labels** (e.g., `:Person`, `:knows`) denote types - can be reused freely
- **Anonymous** elements have no identifier and are always unique

### Anonymous Elements
Every anonymous element is a unique instance:
- Pattern notation: `[]`, `[{k:"v"}]`, `[:Label]`
- Path notation: `(alice)-[:knows]->(bob)` (anonymous relationship)

Each anonymous element is distinct, even if structurally identical.

## Pattern Notation

### Definition Rules
**Brackets create definitions, bare identifiers create references**

```
[a]                     // Defines pattern 'a'
[a {k:"v"}]            // Defines 'a' with properties
[b | a]                // Defines 'b', references 'a'
[b | [a]]              // Defines both 'b' and 'a'
```

### Single Definition Constraint
Each identified pattern can only be defined once:

```
[a {k:"v"}]            // Defines 'a'
[b | a, a]             // OK: references 'a' twice
[a {k2:"v2"}]          // ERROR: 'a' already defined
[c | [a]]              // ERROR: attempts to redefine 'a'
```

### Immutability
Once defined, patterns cannot be modified:

```
[a]                    // Defines 'a'
[a:Label]              // ERROR: cannot add label to existing pattern
[a | b]                // ERROR: cannot add elements to existing pattern
```

## Path Notation

### Basic Syntax
Paths consist of nodes connected by relationships:

```
(a)                    // Node 'a'
(a:Person)             // Node 'a' with label 'Person'
(a)-[r]->(b)           // Relationship 'r' from 'a' to 'b'
(a)-[:knows]->(b)      // Anonymous relationship with label 'knows'
(a)-[k:knows]->(b)     // Relationship 'k' with label 'knows'
```

**Important**: Relationships only exist between nodes, never standalone.

### First-Appearance Definition
In path notation, the first appearance defines an element:

```
(a)-[r1]->(b)          // Defines: a, r1, b
(b)-[r2]->(c)          // Defines: r2, c (b already defined)
(a)-[r3]->(c)          // Defines: r3 (a and c already defined)
```

### Direction Matters
Path relationships map to pattern notation's left-to-right ordering:

```
(a)-[r]->(b)           // Forward: translates to [r | a, b]
(a)<-[r]-(b)           // Reverse: translates to [r | b, a]

// Mixed directions
(a)-[r1]->(b)<-[r2]-(c)  
// Translates to: [ | [r1 | a, b], [r2 | c, b]]
```

### Cyclic Paths
Relationships can be referenced when traversing cycles:

```
(a)-[r1]->(b)-[r2]->(a)-[r1]->(b)
```
- First `r1`: defines `[r1 | a, b]`
- Second `r1`: references the same relationship (valid - same endpoints)

## Pattern-Path Integration

### Path to Pattern Translation
Paths translate to anonymous patterns containing relationship patterns:

```
(a)-[r]->(b)                      
// Translates to: [ | [r | a, b]]

(a)-[r1]->(b)-[r2]->(c)           
// Translates to: [ | [r1 | a, b], [r2 | b, c]]

[p | (a)-[r1]->(b)-[r2]->(c)]    
// Named path translates to: [p | [r1 | a, b], [r2 | b, c]]
```

### Patterns as Nodes
Any pattern can serve as a node in path notation:

```
[team | alice, bob, charlie]
[project | frontend, backend]
(team)-[:works_on]->(project)     // Entire team relates to entire project

[a | x, y]
(a)-[r]->(b)                       // Pattern 'a' has relationship to 'b'
```

This enables multi-level modeling where composites and atoms can freely interconnect.

### Meta-Relationships
Relationship instances can become nodes:

```
(alice)-[k:knows]->(bob)           // Defines relationship 'k'
(k)-[:type_of]->(social)           // 'k' used as a node
```

## Semantic Constraints

### Cross-Notation Consistency
Definitions must be consistent across notations:

```
[k | a, b]                         // Defines 'k' as pattern
(a)-[k]->(c)                       // ERROR: different content [a,c] vs [a,b]

(a)-[k]->(b)                       // Defines 'k' as relationship  
[k | a, b]                         // OK if after: same definition
[k | x, y]                         // ERROR: different content
```

### Anonymous Relationships
Each anonymous relationship is unique:

```
(alice)-[:knows]->(bob)
(alice)-[:knows]->(bob)            // Two different relationships, not one
```

### No Self-Reference
Patterns cannot directly contain themselves:

```
[a | a]                            // ERROR: self-reference
[a | [b | a]]                      // OK: indirect reference through 'b'
```

## Examples

### Building a Graph
```
// Define initial relationships
(alice)-[:knows]->(bob)
(bob)-[:knows]->(charlie)
(alice)-[friendship:knows]->(charlie)

// Reference existing nodes
(alice)-[:manages]->(project)
(bob)-[:contributes_to]->(project)

// Meta-relationship
(friendship)-[:strength]->(strong)
```

### Hierarchical Structures
```
// Define team structure
[eng_team | alice, bob]
[product_team | charlie, dana]
[company | eng_team, product_team]

// Teams interact
(eng_team)-[:collaborates_with]->(product_team)

// Company-level relationship
(company)-[:partners_with]->(other_company)

// Individual can relate to team
(alice)-[:leads]->(eng_team)
```

### Named Paths
```
// Define a path pattern
[review_cycle | 
  (author)-[:writes]->(code)-[:reviewed_by]->(reviewer)
]

// Reference the path pattern
[process | review_cycle, deployment]
```

## Error Examples

```
// Multiple definitions
(a)-[k]->(b)
(a)-[k]->(c)                       // ERROR: k already defined with [a,b]

// Inconsistent redefinition  
(alice)-[k:knows]->(bob)
(alice)-[k:friend]->(bob)          // ERROR: cannot change label

// Undefined reference in pattern notation
[a | b]                            // ERROR if 'b' never defined

// Adding content after definition
(a)-[r]->(b)                       // Defines 'r'
[r:NewLabel]                       // ERROR: cannot modify 'r'
```

## Best Practices

1. **Use labels for types, identifiers for instances** - Anonymous relationships with labels for common types, identified relationships for specific instances needing reference
2. **Define before reference in pattern notation** - Though order-independent, defining first improves readability  
3. **Let path notation build naturally** - First-appearance definition allows organic graph construction
4. **Use pattern-as-node thoughtfully** - Powerful feature best used when relationships truly apply to entire structures
5. **Maintain consistent granularity** - While level-crossing is allowed, consistency within a domain improves clarity