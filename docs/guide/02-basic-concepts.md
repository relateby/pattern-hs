# Basic Concepts: Patterns as Decorated Sequences

## Introduction

Patterns are **decorated sequences**: the elements form the pattern itself, and the value provides decoration about that pattern. This simple but powerful structure enables you to represent patterns from everyday life in a way that makes patterns explicit and usable.

Understanding this fundamental concept is essential before diving into how to create and manipulate Patterns. This section will help you grasp what Patterns are, how they relate to familiar concepts, and how they differ from other data structures.

## Patterns as Decorated Sequences

The core idea of Patterns is that **elements form the pattern**, and **values provide decoration**.

### Conceptual Model

Think of a pattern like a musical rhythm or a poetic meter:

```gram
["Enclosed rhyme" | A, B, B, A]
```

In this example:
- The **elements** (`A, B, B, A`) **are** the pattern—they form the sequence that defines the pattern
- The **value** (`"Enclosed rhyme"`) provides decoration—it describes what kind of pattern this is

The elements are not subordinate to the value; rather, the value describes the pattern formed by the elements.

### Why This Matters

This structure enables Patterns to:
- Make patterns **explicit** rather than implicit
- Support **composition** and **comparison** of patterns
- Represent patterns at **multiple levels of abstraction**

## Intuitive Examples from Everyday Life

Patterns show up everywhere in our world. Let's explore some familiar examples to build intuition, progressing from simple sequences with implicit relationships to complex graph structures with explicit relationships.

### Design Patterns

In software design, patterns like "Observer" or "Factory" describe common solutions to recurring problems. A design pattern is like a little category or graph—it shows how entities relate to each other through relationships and interactions.

A Pattern can represent this structure:

```gram
["Observer Pattern" | [Notify | Subject, Observer]]
```

The elements form a **sequence of relationships among objects**. Here:
- `Subject` and `Observer` are **entities** (nodes in the graph)
- `Notify` is a **relationship** (an edge connecting them)
- The relationship `Notify` connects `Subject` to `Observer`

The pattern structure makes explicit that this is a graph: entities (`Subject`, `Observer`) connected by relationships (`Notify`). The sequence of elements represents the sequence of relationships that define the pattern structure.

### Architectural Patterns

In building architecture, patterns like "Courtyard" or "Atrium" describe spatial arrangements with **implicit spatial relationships**:

```gram
["Courtyard Pattern" | Building, Courtyard, Building]
```

The elements represent the spatial sequence, where spatial relationships (adjacency, containment) are implicit in the sequence order. The value describes the architectural pattern type.

### Musical Patterns

Musical patterns like rhythms or chord progressions are sequences with **implicit "follows" relationships**:

```gram
["I-V-vi-IV" | I, V, vi, IV]
```

The elements form the chord sequence, where each chord implicitly "follows" the previous one. The value names the progression pattern. The relationships between chords are implicit in the sequence order.

### Literary Patterns

Poetic meters and rhyme schemes are patterns with **implicit structural relationships**:

```gram
["Sonnet" | Quatrain, Quatrain, Quatrain, Couplet]
```

The elements represent the structural sequence, where structural relationships (rhyme scheme, meter) are implicit in the sequence order. The value identifies the form.

### Behavioral Patterns

In psychology, behavioral patterns describe sequences of actions with **implicit "next" relationships**:

```gram
["Habit Loop" | Cue, Routine, Reward]
```

The elements form the behavioral sequence, where each action implicitly follows the previous one. The value describes the pattern type. The relationships between actions are implicit in the sequence order.

## Patterns: From Implicit to Explicit Relationships

Patterns can represent relationships in different ways, from simple sequences with implicit "next" relationships to complex graph structures with explicit relationships. Understanding this progression helps clarify how Patterns differ from knowledge graphs and other data structures.

### Progression: Implicit to Explicit Relationships

1. **Implicit "next" relationships**: Sequences where objects connect via implicit "next" or "follows" relationships
   - **Musical progression**: `["I-V-vi-IV" | I, V, vi, IV]` — implicit "follows"
   - **Workflow steps**: `["Process Order" | ValidatePayment, UpdateInventory, ShipItem]` — implicit "next step"
   - **Literary structure**: `["Sonnet" | Quatrain, Quatrain, Quatrain, Couplet]` — implicit "follows"

2. **Single explicit relationship**: One explicit relationship connecting entities
   - **Observer Pattern**: `["Observer Pattern" | [Notify | Subject, Observer]]` — explicit `Notify` relationship

3. **Multiple explicit relationships**: Multiple different relationship types
   - **Factory Pattern**: `["Factory Pattern" | [Creates | Creator, Product], [Implements | ConcreteCreator, Creator]]` — multiple relationship types

4. **Explicit relationships with properties**: Relationships that carry their own properties
   - **Route 66**: `["Route 66" | [Segment1 {length: 300, speed: 65} | Chicago, StLouis], ...]` — each segment is an explicit relationship with properties

## How Patterns Differ from Knowledge Graphs

Knowledge graphs are often used to encode pattern concepts, but they represent pattern concepts **implicitly** as traversals of the graph structure. The Pattern data structure makes pattern concepts **explicit**.

### The Route 66 Example

Consider "Route 66" in the USA. It's both:
- A **sequence** of road segments (Chicago → St. Louis → Oklahoma City → ... → Los Angeles)
- **Information** about the entire route (history, pop culture, significance)

#### In a Knowledge Graph

In a property graph, Route 66 might be represented as:
- A **Route66 node**: `Route66 { history: "...", popCulture: "..." }` — the concept itself
- **City nodes**: `Chicago`, `St. Louis`, `Oklahoma City`, ..., `Los Angeles`
- **Segment relationships**: `(Chicago)-[:SEGMENT {length: 300, speed: 65}]->(St. Louis)`, etc.

But how do you connect the `Route66` node to all the segments? You might use a convention like:
- All segments have a property: `[:SEGMENT {route: "Route66", ...}]`
- Or segments connect to Route66: `(Route66)-[:CONTAINS]->(:SEGMENT)`

The **pattern concept** (the sequence of segments) is **implicit**—you have to traverse the graph following these conventions to discover it. The pattern exists only as an implicit traversal path, not as an explicit structure. The connection between Route66 and its segments requires a convention, which is the "lossy" part—the pattern structure is not directly represented.

#### As a Pattern Data Structure

As a Pattern data structure, Route 66 would be **explicit** as a sequence of road segments, where each segment is a relationship connecting two cities:

```gram
["Route 66" | [Segment1 {length: 300, speed: 65} | Chicago, StLouis], [Segment2 {length: 250, speed: 70} | StLouis, OklahomaCity], ..., [SegmentN {length: 200, speed: 75} | Flagstaff, LosAngeles]]
```

Each segment is an explicit relationship pattern connecting two cities, with properties like length, speed, and cost. The sequence of segments is explicit in the elements. The decoration (history, pop culture) would be in the value or properties.

This makes the pattern concept:
- **Explicit** rather than implicit
- **Comparable** with other route pattern concepts
- **Composable** with other pattern structures (e.g., Route 66 as part of a "Vacation Plan")

### Why This Matters

Making pattern concepts explicit enables:
- **Comparison**: Compare Route 66 with Route 40 to see similarities and differences
- **Composition**: Compose Route 66 with other pattern structures (vacation plans, travel guides)
- **Factoring**: Extract common elements from multiple route pattern structures

## How the Pattern Data Structure Differs from Other Data Structures

The Pattern data structure is distinct from common data structures like lists, trees, and graphs.

### Patterns vs. Lists

**Lists** are sequences of values:
```haskell
[1, 2, 3]  -- Just values, no decoration
```

**Pattern data structures** are decorated sequences:
```gram
["sequence" | 1, 2, 3]
```

The key difference: Pattern data structures have **decoration** (the value) that describes the pattern concept formed by the elements. Lists are just sequences.

### Patterns vs. Trees

**Trees** have a hierarchical structure where nodes contain children:

```
    Root
   /    \
Child1  Child2
```

**Patterns** are sequences with decoration:
```gram
["pattern" | elem1, elem2]
```

The key difference: In trees, children are **subordinate** to their parent. In Pattern data structures, elements **are** the pattern concept—they're not subordinate to the value. The value describes the pattern concept formed by the elements.

### Patterns vs. Graphs

**Graphs** have nodes and edges:
```
(A) --[edge]--> (B)
```

**Patterns** are sequences with decoration:
```gram
["path" | A, B]
```

The key difference: Graphs represent relationships between entities. Pattern data structures represent **sequences** with decoration. While Patterns can represent graph-like structures (through views), their primary semantic is as decorated sequences.

### When to Use the Pattern Data Structure

Use the Pattern data structure when:
- You need to represent **sequences** with **decoration** about those sequences
- You want pattern concepts to be **explicit** rather than implicit
- You need to **compare**, **compose**, or **factor** pattern structures
- You're working with pattern concepts from everyday life (design, architecture, music, literature, behavior)

Use other data structures when:
- You need simple sequences without decoration → **Lists**
- You need hierarchical structures with parent-child relationships → **Trees**
- You need to represent arbitrary relationships between entities → **Graphs**

## Summary

- **Pattern data structures are decorated sequences**: Elements form the pattern concept, values provide decoration
- **The Pattern data structure makes pattern concepts explicit**: Unlike knowledge graphs where pattern concepts are implicit traversals
- **Pattern data structures are distinct from lists, trees, and graphs**: They represent sequences with decoration, not just sequences or hierarchical structures
- **Pattern concepts are intuitive**: They align with how we naturally think about patterns in design, architecture, music, literature, and behavior

## Next Steps

Now that you understand what Patterns are, learn how to create them in the [Construction](03-construction.md) section.

