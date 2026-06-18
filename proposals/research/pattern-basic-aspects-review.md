# Pattern Basic Aspects Review: Depth, Size, and Length

**Date**: 2025-01-27  
**Purpose**: Review and illustrate basic pattern aspects using gram notation

## Current Implementation

### 1. `length :: Pattern v -> Int`

**Definition**: Returns the number of direct elements in a pattern's sequence.

**Implementation**:
```haskell
length :: Pattern v -> Int
length (Pattern _ es) = Prelude.length es
```

**Properties**:
- O(1) operation
- Counts only direct children, not nested descendants
- Atomic patterns (no elements) return 0

### 2. `size :: Pattern v -> Int`

**Definition**: Returns the total number of nodes in a pattern structure.

**Implementation**:
```haskell
size :: Pattern v -> Int
size (Pattern _ es) = 1 + sum (map size es)
```

**Properties**:
- O(n) operation where n is the total number of nodes
- Recursively counts all nodes at all nesting levels
- Includes the root node
- Atomic patterns return 1

### 3. `depth :: Pattern v -> Int`

**Definition**: Returns the maximum nesting depth of a pattern structure.

**Implementation**:
```haskell
depth :: Pattern v -> Int
depth (Pattern _ []) = 0
depth (Pattern _ es) = 1 + maximum (map depth es)
```

**Properties**:
- O(n) operation where n is the total number of nodes
- Atomic patterns return 0 (zero-based depth, root only, no nesting)
- Pattern with elements has depth 1 + max depth of elements
- Returns maximum depth across all branches
- Follows standard tree depth conventions (root at depth 0)

## Gram Notation Examples

Gram notation uses subject pattern syntax `[ subject | elements ]` to represent patterns. The following examples illustrate how depth, size, and length work with various pattern structures.

### Example 1: Atomic Pattern

**Gram Notation**:
```gram
[atom]
```

**Structure**: A single pattern with no elements
- `value`: `atom`
- `elements`: `[]`

**Metrics**:
- `length`: 0 (no direct elements)
- `size`: 1 (just the root node)
- `depth`: 0 (atomic pattern, zero-based depth)

**Visualization**:
```
[atom]
 └─ (no elements)
```

### Example 2: Simple Sequence

**Gram Notation**:
```gram
[sequence | a, b, c]
```

**Structure**: A pattern with three direct elements
- `value`: `sequence`
- `elements`: `[a, b, c]` (each is an atomic pattern)

**Metrics**:
- `length`: 3 (three direct elements: a, b, c)
- `size`: 4 (root + 3 atomic elements = 1 + 1 + 1 + 1)
- `depth`: 1 (root has elements, each element has depth 0, so root depth = 1 + max(0,0,0) = 1)

**Visualization**:
```
[sequence]
 ├─ [a]
 ├─ [b]
 └─ [c]
```

### Example 3: Nested Pattern (One Level)

**Gram Notation**:
```gram
[root | [inner | x, y]]
```

**Structure**: A pattern containing one nested pattern
- `value`: `root`
- `elements`: `[[inner | x, y]]` (one element which is itself a pattern)

**Metrics**:
- `length`: 1 (one direct element: the inner pattern)
- `size`: 4 (root + inner + x + y = 1 + 1 + 1 + 1)
- `depth`: 2 (root depth = 1 + max(inner depth) where inner depth = 1 + max(0,0) = 1, so root = 1 + 1 = 2)

**Visualization**:
```
[root]
 └─ [inner]
     ├─ [x]
     └─ [y]
```

### Example 4: Multiple Nested Patterns

**Gram Notation**:
```gram
[container | [left | a, b], [right | c, d]]
```

**Structure**: A pattern with two nested patterns
- `value`: `container`
- `elements`: `[[left | a, b], [right | c, d]]`

**Metrics**:
- `length`: 2 (two direct elements: left and right patterns)
- `size`: 6 (root + left + a + b + right + c + d = 1 + 1 + 1 + 1 + 1 + 1 + 1)
- `depth`: 2 (root depth = 1 + max(left depth, right depth) where both left and right have depth = 1 + max(0,0) = 1, so root = 1 + 1 = 2)

**Visualization**:
```
[container]
 ├─ [left]
 │   ├─ [a]
 │   └─ [b]
 └─ [right]
     ├─ [c]
     └─ [d]
```

### Example 5: Deeply Nested Pattern

**Gram Notation**:
```gram
[level1 | [level2 | [level3 | [level4 | leaf]]]]
```

**Structure**: A linear chain of nested patterns
- `value`: `level1`
- `elements`: `[[level2 | [level3 | [level4 | leaf]]]]`

**Metrics**:
- `length`: 1 (one direct element: level2)
- `size`: 5 (level1 + level2 + level3 + level4 + leaf = 5 nodes)
- `depth`: 4 (calculated as: leaf=0, level4=1+0=1, level3=1+1=2, level2=1+2=3, level1=1+3=4)

**Visualization**:
```
[level1]
 └─ [level2]
     └─ [level3]
         └─ [level4]
             └─ [leaf]
```

### Example 6: Asymmetric Branching

**Gram Notation**:
```gram
[root | [shallow | a], [deep | [deeper | [deepest | x]]]]
```

**Structure**: A pattern with branches of different depths
- `value`: `root`
- `elements`: `[[shallow | a], [deep | [deeper | [deepest | x]]]]`

**Metrics**:
- `length`: 2 (two direct elements: shallow and deep)
- `size`: 7 (root + shallow + a + deep + deeper + deepest + x = 7 nodes)
- `depth`: 4 (calculated as: [x]=0, [deepest|x]=1+0=1, [deeper|...]=1+1=2, [deep|...]=1+2=3, [shallow|a]=1+0=1, [root|...]=1+max(1,3)=4)

**Visualization**:
```
[root]
 ├─ [shallow]
 │   └─ [a]
 └─ [deep]
     └─ [deeper]
         └─ [deepest]
             └─ [x]
```

### Example 7: Graph Node Pattern (Atomic)

**Gram Notation**:
```gram
(a:Person {name: "Alice"})
```

**Structure**: A node pattern (syntactic sugar for atomic pattern)
- `value`: Subject with identifier `a`, label `Person`, and properties
- `elements`: `[]` (nodes are atomic)

**Metrics**:
- `length`: 0 (no elements)
- `size`: 1 (just the node)
- `depth`: 0 (atomic pattern, zero-based depth)

### Example 8: Relationship Pattern

**Gram Notation**:
```gram
(a)-[r:KNOWS]->(b)
```

**Structure**: A relationship pattern (syntactic sugar)
- This desugars to: `[r:KNOWS | (a), (b)]`
- `value`: Subject with identifier `r` and label `KNOWS`
- `elements`: `[(a), (b)]` (two node patterns)

**Metrics**:
- `length`: 2 (two direct elements: nodes a and b)
- `size`: 3 (relationship + node a + node b)
- `depth`: 1 (relationship has elements, each node has depth 0, so relationship depth = 1 + max(0,0) = 1)

### Example 9: Subject with Members

**Gram Notation**:
```gram
[team:Team {name: "DevRel"} | abk, adam, alex]
```

**Structure**: A subject pattern with identifier, label, properties, and elements
- `value`: Subject with identifier `team`, label `Team`, and properties
- `elements`: `[abk, adam, alex]` (three atomic patterns)

**Metrics**:
- `length`: 3 (three direct elements: abk, adam, alex)
- `size`: 4 (team + abk + adam + alex)
- `depth`: 1 (team has elements, each member has depth 0, so team depth = 1 + max(0,0,0) = 1)

### Example 10: Complex Nested Structure

**Gram Notation**:
```gram
[organization | 
  [engineering:Team | 
    [backend:Group | alice, bob],
    [frontend:Group | charlie, diana]
  ],
  [product:Team | 
    [design:Group | eve],
    [research:Group | frank, grace]
  ]
]
```

**Structure**: Multi-level nested organization
- `value`: `organization`
- `elements`: Two team patterns, each containing group patterns

**Metrics**:
- `length`: 2 (two direct elements: engineering and product teams)
- `size`: 13 (organization + engineering + backend + alice + bob + frontend + charlie + diana + product + design + eve + research + frank + grace)
- `depth`: 3 (calculated as: members=0, groups=1+max(0,0)=1, teams=1+max(1,1)=2, organization=1+max(2,2)=3)

**Visualization**:
```
[organization]
 ├─ [engineering:Team]
 │   ├─ [backend:Group]
 │   │   ├─ [alice]
 │   │   └─ [bob]
 │   └─ [frontend:Group]
 │       ├─ [charlie]
 │       └─ [diana]
 └─ [product:Team]
     ├─ [design:Group]
     │   └─ [eve]
     └─ [research:Group]
         ├─ [frank]
         └─ [grace]
```

## Summary Table

| Example | Gram Pattern | length | size | depth |
|---------|-------------|--------|------|-------|
| Atomic | `[atom]` | 0 | 1 | 0 |
| Simple sequence | `[seq \| a, b, c]` | 3 | 4 | 1 |
| One level nested | `[root \| [inner \| x, y]]` | 1 | 4 | 2 |
| Multiple nested | `[container \| [left \| a, b], [right \| c, d]]` | 2 | 6 | 2 |
| Deep nesting | `[l1 \| [l2 \| [l3 \| [l4 \| leaf]]]]` | 1 | 5 | 4 |
| Asymmetric | `[root \| [shallow \| a], [deep \| [deeper \| [deepest \| x]]]]` | 2 | 7 | 4 |
| Node | `(a:Person {name: "Alice"})` | 0 | 1 | 0 |
| Relationship | `(a)-[r:KNOWS]->(b)` | 2 | 3 | 1 |
| Team | `[team:Team {name: "DevRel"} \| abk, adam, alex]` | 3 | 4 | 1 |
| Complex org | Multi-level structure | 2 | 13 | 3 |

## Key Insights

1. **Length** measures only direct children, making it useful for understanding immediate structure.

2. **Size** measures total nodes, making it useful for understanding overall complexity and memory usage.

3. **Depth** measures maximum nesting, making it useful for understanding structural complexity and recursion limits.

4. **Relationship between metrics**:
   - `size >= length + 1` (size includes root, length doesn't)
   - `depth >= 0` (all patterns have at least depth 0, atomic patterns have depth 0)
   - For atomic patterns: `length = 0`, `size = 1`, `depth = 0`

5. **Gram notation mapping**:
   - Subject patterns `[subject \| elements]` directly map to Pattern structure
   - Node patterns `(subject)` are atomic patterns (length=0)
   - Relationship patterns `(a)-[r]->(b)` desugar to patterns with 2 elements

