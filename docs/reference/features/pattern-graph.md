# PatternGraph Feature Reference

**Status**: Implemented (Feature 33)  
**Module**: `Pattern.PatternGraph`  
**Spec**: `specs/033-pattern-graph/`

## Types

### PatternGraph

```haskell
data PatternGraph v = PatternGraph
  { pgNodes         :: Map (Id v) (Pattern v)
  , pgRelationships :: Map (Id v) (Pattern v)
  , pgWalks         :: Map (Id v) (Pattern v)
  , pgAnnotations   :: Map (Id v) (Pattern v)
  }
```

Container holding four categories of graph elements keyed by identity. All stored elements are `Pattern v`; classification is via the `GraphValue` typeclass.

### PatternClass

```haskell
data PatternClass = Node | Annotation | Relationship | Walk | Unrecognized
```

Classification of a pattern for dispatch into PatternGraph categories. Unrecognized patterns are not stored; they appear in `MergeResult.unrecognized`.

### GraphValue

```haskell
class Ord (Id v) => GraphValue v where
  type Id v
  identify :: v -> Id v
  classify :: Pattern v -> PatternClass
```

Provides identity and classification for the value type `v`. The primary instance is `Subject` with `Id Subject = Symbol`; classification is by arity (0 = Node, 1 = Annotation, 2 = Relationship, n = Walk, else Unrecognized).

### MergeResult

```haskell
data MergeResult v = MergeResult
  { mergedGraph   :: PatternGraph v
  , unrecognized :: [Pattern v]
  }
```

Result of `merge` or `fromPatterns`. Unrecognized patterns are never stored; they appear only in `unrecognized`.

## Operations

| Function | Description |
|----------|-------------|
| `empty` | Empty graph (all four maps empty). |
| `merge` | Merge one pattern into the graph (default: LastWriteWins). Returns `MergeResult`. |
| `mergeWithPolicy` | Merge with explicit `ReconciliationPolicy`. |
| `fromPatterns` | Build graph from a list (fold of merge). Returns `MergeResult`. |
| `fromPatternsWithPolicy` | Same with explicit policy. |
| `toGraphLens` | Convert PatternGraph to `Maybe (GraphLens v)`; `Nothing` when empty. |
| `toGraphLensWithScope` | Total conversion using given scope value (works for empty graphs). |

## Round-trip and .graph.gram

- **Round-trip**: Parse (e.g. `Gram.Parse.fromGram`) → `fromPatterns` → modify → serialize via `Gram.Serialize.toGram` on flattened graph contents.
- **.graph.gram**: Files restricted to annotations, nodes, relationships, and paths (no square-bracket pattern notation). See `docs/reference/graph-gram-notation.md`.

## Related

- **Graph Lens**: `docs/reference/features/graph-lens.md`
- **Reconciliation**: `Pattern.Reconcile` (policies, duplicate identity)
