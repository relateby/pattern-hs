# Phase 0: Outline & Research

## Extensible Storage Shape
- **Decision:** Store the custom category fallback as `Map (Id v) (extra, Pattern v)` inside `pgOther`.
- **Rationale:** Preserves the specific `extra` classification tag paired with the pattern without modifying `Pattern v` itself.
- **Alternatives considered:** `Map (Id v) (Pattern v)`, which would discard the specific classification tag and require re-classification on retrieval.

## GraphLens Internal Implementation
- **Decision:** Re-derive `GraphLens` operations to use a two-category `GraphClassifier`.
- **Rationale:** Unifies the parallel type hierarchies while maintaining the exact public API of `GraphLens`. The two categories will be `GNode` (when `testNode` holds) and `GOther Void` (when it doesn't).
- **Alternatives considered:** Keeping `GraphLens` separate (creates duplicate work for graph queries).

## Default Classification Logic
- **Decision:** `classifyByShape` is extracted from the `GraphValue` typeclass and packaged into `canonicalClassifier :: GraphClassifier Void Subject`. Crucially, Walk classification is upgraded: walks must be validated as a contiguous sequence of relationships that "chain" by sharing nodes between consecutive relationships (ignoring relationship direction).
- **Rationale:** Separates categorization policy from the value type identity mechanism. Additionally, GraphLens requires walks to be truly connected. If `classifyByShape` only checks arity, invalid structures (like disconnected subgraphs, or "star patterns" where relationships share a center node but consecutive relationships don't chain end-to-end) would be incorrectly categorized as walks. Such invalid chains will correctly fall back to `GOther`.
- **Alternatives considered:** Leaving it in the typeclass. Leaving Walk classification as a simple arity check (rejected because it violates the rule that patterns must be structurally valid for their target category, and causes failures in graph traversal algorithms).
