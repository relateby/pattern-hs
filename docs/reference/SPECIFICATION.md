# Feature Specification

**Audience**: Language porters  
**Purpose**: Authoritative, current feature specification with implementation status

## Implementation Status Overview

### Pattern Library (`libs/pattern`)

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| Core Pattern Type | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | `Pattern v` with `value` and `elements` |
| Show Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | Manual instance |
| Eq Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | `deriving Eq` |
| Construction Functions | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | `pattern`, `patternWith`, `fromList` |
| Functor Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | Structure-preserving `fmap` |
| Foldable Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | `foldr`, `foldl`, `toList`, `flatten` |
| Traversable Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | `traverse`, `sequenceA` |
| Query Functions | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | `length`, `size`, `depth`, `values` |
| Ord Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | Lexicographic ordering |
| Semigroup Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | Pattern combination |
| Monoid Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | Identity pattern |
| Hashable Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | Structure-preserving hashing |
| Applicative Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | Zip-like application |
| Predicate Matching | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | `anyValue`, `allValues`, `filterPatterns`, `findPattern`, `matches`, `contains` |
| Comonad Instance | ✅ Implemented | `libs/pattern/src/Pattern/Core.hs` | `extract`, `duplicate`, `extend`, `depthAt`, `sizeAt`, `indicesAt` |
| Graph Lens | ✅ Implemented | `libs/pattern/src/Pattern/Graph.hs` | Feature 23: Interpretive graph views |

### Subject Library (`libs/subject`)

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| Core Subject Type | ✅ Implemented | `libs/subject/src/Subject/Core.hs` | `Subject` with identity, labels, properties |
| Value Types | ✅ Implemented | `libs/subject/src/Subject/Value.hs` | Standard and extended value types |
| Identity Management | ✅ Implemented | `libs/subject/src/Subject/Core.hs` | Sequential ID generation for anonymous subjects |

### Gram Library (`libs/gram`)

| Feature | Status | Location | Notes |
|---------|--------|----------|-------|
| Parsing (`fromGram`) | ✅ Implemented | `libs/gram/src/Gram/Parse.hs` | Supports single-root wrapping, list parsing, and header separation |
| Serialization (`toGram`) | ✅ Implemented | `libs/gram/src/Gram/Serialize.hs` | Supports single pattern, list serialization, and header serialization |
| Validation | ✅ Implemented | `libs/gram/src/Gram/Validate.hs` | Duplicate definition, undefined reference, arity checking |
| Round-Trip Verification | ✅ Implemented | Tests | Validated against full test corpus |

### Deferred Features

| Feature | Status | Reference | Notes |
|---------|--------|-----------|-------|
| Pattern Morphisms | ❌ Deferred | `design/DESIGN.md` | `Functor` provides similar functionality |
| Zipper | ❌ Deferred | `design/DESIGN.md` | Comonad provides context-aware operations |
| Pattern Matching DSL | ❌ Deferred | `design/pattern-matching-dsl-design.md` | Predicate matching (Feature 9) provides similar functionality |

## Feature Specifications

See `docs/reference/features/` for detailed feature-by-feature specifications:
- `core-pattern.md` - Core Pattern type
- `typeclass-instances.md` - Typeclass instances
- `graph-lens.md` - Graph Lens (Feature 23)
- `gram-serialization.md` - Gram serialization

## API Contracts

### Core Pattern Type

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
```

### Construction Functions

```haskell
pattern :: v -> Pattern v
patternWith :: v -> [Pattern v] -> Pattern v
fromList :: [Pattern v] -> Pattern v
```

### Query Functions

```haskell
length :: Pattern v -> Int
size :: Pattern v -> Int
depth :: Pattern v -> Int
values :: Pattern v -> [v]
```

### Typeclass Instances

- `Show`, `Eq`, `Ord` (value type must have instance)
- `Functor`, `Foldable`, `Traversable`
- `Semigroup`, `Monoid` (value type must have instance)
- `Hashable`, `Applicative`, `Comonad`

See `docs/reference/features/typeclass-instances.md` for complete specifications.

## Behavioral Specifications

### Pattern Equality

Two patterns are equal if and only if:
1. Their values are equal (using value type's `Eq` instance)
2. Their elements lists have the same length
3. Corresponding elements are equal (recursive)

### Functor Laws

- **Identity**: `fmap id = id`
- **Composition**: `fmap (f . g) = fmap f . fmap g`

### Monoid Laws

- **Associativity**: `(a <> b) <> c = a <> (b <> c)`
- **Identity**: `mempty <> a = a = a <> mempty`

## Test Coverage

All features have comprehensive test coverage. Tests serve as executable specifications:
- Unit tests for each function
- Property-based tests for typeclass laws
- Integration tests for cross-component functionality

See test files in `libs/*/tests/` for complete test specifications.

## See Also

- **[Architecture](ARCHITECTURE.md)** - Design principles
- **[Implementation](IMPLEMENTATION.md)** - Implementation patterns
- **[Porting Guide](PORTING-GUIDE.md)** - Implementation roadmap
- **[Feature Specifications](features/)** - Detailed feature specs

