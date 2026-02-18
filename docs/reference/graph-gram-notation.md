# .graph.gram Notation Reference

**Purpose**: Define the subset of gram notation allowed in `.graph.gram` files and describe the resulting data structures inside PatternGraph.

**Flow**: *gram → parse → PatternGraph → explained with pattern notation.*

## Allowed syntax in .graph.gram files

A `.graph.gram` file is restricted to **graph-only** notation:

- **Annotations** (e.g. `@@id` or `@key(value)` on a pattern)
- **Nodes** (e.g. `(a)`, `(n:Person)`, `(n:Person {name:"Alice"})`)
- **Relationships** (e.g. `(a)-[r:KNOWS]->(b)`, path notation)
- **Paths** (sequences of relationships, e.g. `(a)-[r1]->(b)-[r2]->(c)`)

**Not allowed** in the file:

- Square-bracket pattern notation for arbitrary nested patterns (e.g. `[label | elem1, elem2]` as a general pattern). The file format uses only node, relationship, path, and annotation syntax so that the result maps cleanly onto PatternGraph.

## Resulting structures in PatternGraph

After parsing a `.graph.gram` document with `Gram.Parse.fromGram` and loading with `fromPatterns`:

1. **Nodes** → entries in `pgNodes`. Each node pattern (0 elements) is keyed by its identity (e.g. `Symbol "a"`).
2. **Relationships** → entries in `pgRelationships`. Each relationship (2 node elements) is keyed by its identity. Endpoint nodes are also merged into `pgNodes`.
3. **Paths / walks** → entries in `pgWalks`. A path becomes a walk pattern (n relationship elements). Component relationships and their nodes are merged into `pgRelationships` and `pgNodes`.
4. **Annotations** → entries in `pgAnnotations`. The annotated inner element is merged into its category (node or relationship, etc.).

In pattern notation, a node is a pattern with no elements: `[value | ]`. A relationship is a pattern with two node elements: `[value | node1, node2]`. A walk is a pattern with n relationship elements: `[value | rel1, rel2, ...]`. Classification and storage follow the data model in `specs/033-pattern-graph/data-model.md`.

## Unrecognized patterns

If the parser produces a pattern that does not classify as Node, Annotation, Relationship, or Walk (e.g. a malformed or non-graph pattern), it is **not** stored in the graph and appears in `MergeResult.unrecognized`. Callers can log or error on unrecognized as needed.

## See also

- **PatternGraph usage**: `docs/guide/pattern-graph-usage.md`
- **Data model**: `specs/033-pattern-graph/data-model.md`
- **Quick start**: `specs/033-pattern-graph/quickstart.md`
- **Gram notation (full)**: `docs/guide/08-gram-notation-reference.md`
