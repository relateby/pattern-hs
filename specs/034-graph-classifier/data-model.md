# Data Model

## Entities

### `GraphClass extra`
```haskell
data GraphClass extra
  = GNode
  | GRelationship
  | GAnnotation
  | GWalk
  | GOther extra
  deriving (Eq, Show, Functor, Traversable, Foldable)
```
- **Description**: Represents the five structural categories of graph elements. `GOther` allows open extension.
- **Validation Constraints**: Elements classified as `GWalk` MUST consist of a sequence of valid relationships that properly "chain" (consecutive relationships must share at least one node, ignoring the direction of the relationships). Patterns failing this structural sequence constraint MUST NOT be classified as `GWalk`, but should instead fall back to `GOther`. Note: "Star patterns" where all relationships share a center node but are listed such that consecutive relationships do *not* chain end-to-end must be explicitly rejected as a walk.

### `GraphClassifier extra v`
```haskell
data GraphClassifier extra v = GraphClassifier
  { classify :: Pattern v -> GraphClass extra
  }
```
- **Description**: Pluggable record of functions defining the categorization logic.
- **Validation**: Every `Pattern v` must be assigned to exactly one `GraphClass extra`.

### `PatternGraph extra v` (Updated)
```haskell
data PatternGraph extra v = PatternGraph
  { pgNodes         :: Map (Id v) (Pattern v)
  , pgRelationships :: Map (Id v) (Pattern v)
  , pgWalks         :: Map (Id v) (Pattern v)
  , pgAnnotations   :: Map (Id v) (Pattern v)
  , pgOther         :: Map (Id v) (extra, Pattern v)
  }
```
- **Description**: Holds categorized graph elements eagerly. Now parameterized by `extra` (usually `()` for the canonical graph).
- **Transitions**: Built from `fromPatterns classifier patterns`.

### `GraphValue v` (Updated)
```haskell
class Ord (Id v) => GraphValue v where
  type Id v
  identify :: v -> Id v
```
- **Description**: Simplified to only handle identity extraction. `classify` is moved to `GraphClassifier`.
