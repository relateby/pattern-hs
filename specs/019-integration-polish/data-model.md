# Data Model: Public API Surface

The "Data Model" for this feature is the set of exported types and functions that constitute the public API of the `pattern-hs` library (specifically the `pattern` package).

## Core Types

### `Pattern v`
The fundamental recursive data structure.
- **Constructors**: `Pattern { value :: v, elements :: [Pattern v] }`
- **Exports**: Type and constructors MUST be exported.

## Core Functions

### Construction
- `pattern :: v -> Pattern v`
- `patternWith :: v -> [Pattern v] -> Pattern v`
- `fromList :: v -> [v] -> Pattern v`

### Query
- `length :: Pattern v -> Int`
- `size :: Pattern v -> Int`
- `depth :: Pattern v -> Int`
- `values :: Pattern v -> [v]`

### Context (Comonad)
- `extract :: Pattern v -> v`
- `duplicate :: Pattern v -> Pattern (Pattern v)`
- `extend :: (Pattern v -> w) -> Pattern v -> Pattern w`
- `depthAt :: Pattern v -> Pattern Int`
- `sizeAt :: Pattern v -> Pattern Int`
- `indicesAt :: Eq v => Pattern v -> Pattern [Int]`

### Predicates
- `anyValue :: (v -> Bool) -> Pattern v -> Bool`
- `allValues :: (v -> Bool) -> Pattern v -> Bool`
- `filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]`
- `findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)`
- `findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]`
- `matches :: Eq v => Pattern v -> Pattern v -> Bool`
- `contains :: Eq v => Pattern v -> Pattern v -> Bool`

### Foldable/Traversable Extras
- `flatten :: Pattern v -> [v]`
- `toTuple :: Pattern v -> (v, [Pattern v])`

## Typeclass Instances
The following instances MUST be exported:
- `Show v => Show (Pattern v)`
- `Eq v => Eq (Pattern v)`
- `Ord v => Ord (Pattern v)`
- `Functor Pattern`
- `Foldable Pattern`
- `Traversable Pattern`
- `Semigroup v => Semigroup (Pattern v)`
- `Monoid v => Monoid (Pattern v)`
- `Hashable v => Hashable (Pattern v)`
- `Applicative Pattern`
- `Comonad Pattern`
