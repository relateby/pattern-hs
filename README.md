# Pattern HS

A Haskell library providing a generalized representation of graph elements using category theory. Pattern is a recursive data structure that can be interpreted as different graph elements (nodes, relationships, subgraphs, paths) through categorical views.

## Design Goals

### Primary Objectives

1. **Reference Implementation**: This codebase serves as a canonical reference design that can be accurately translated to other programming languages while maintaining category-theoretic correctness.

2. **Category-Theoretic Foundation**: Patterns are built on solid mathematical foundations from category theory, ensuring:
   - Functor instances that satisfy functor laws
   - Natural transformations that satisfy naturality conditions
   - Morphisms that preserve relevant structure
   - Composition that is associative with identity elements

3. **Schema-Lazy Design**: Patterns don't commit to specific graph semantics at creation time. Interpretation happens through views, enabling:
   - Multiple interpretations of the same pattern structure
   - View composition and stacking
   - Open-ended extensions for new graph-like interpretations

4. **Sequence-Based Semantics**: Conceptually, patterns are sequences of elements with associated metadata. While implemented using a recursive tree structure, the primary semantic is sequence-based, enabling intuitive pattern matching and manipulation.

5. **Multi-Language Translation**: The design prioritizes clarity and mathematical correctness over language-specific optimizations, making it easier to translate to other languages while preserving conceptual consistency.

### Key Principles

- **Code Quality (NON-NEGOTIABLE)**: All code must be clear, maintainable, and well-documented
- **Testing Standards (NON-NEGOTIABLE)**: Comprehensive testing serves as both verification and executable specifications
- **Conceptual Consistency**: All implementations align with category theory formalisms
- **Mathematical Clarity**: Formal definitions and mathematical properties are explicitly stated
- **Cross-Language Alignment**: Core design is language-agnostic and translatable

## Core Concept: Patterns as Sequences

A Pattern is conceptually a **sequence of elements with associated metadata**. For example, the pattern "3 1 4 1 5 9" is a sequence of 6 elements, that could be described as some digits of Pi.

```haskell
data Pattern v = Pattern 
  { value    :: v        -- Metadata about the sequence
  , elements :: [Pattern v]  -- The sequence itself
  }
```

The `value` field stores metadata about the sequence (e.g., sequence name, type, or properties), while `elements` contains the sequence of pattern elements. Each element in the sequence is itself a Pattern, enabling recursive nesting while maintaining the sequence semantic.

While implemented using a recursive tree structure, the primary semantic is sequence-based. This enables:
- Intuitive pattern matching
- Clear length semantics (number of elements in the sequence)
- Natural composition operations
- Graph element interpretation through views

## Project Structure

```
pattern-hs/
├── src/
│   ├── Pattern.hs          # Main module (re-exports)
│   └── Pattern/
│       ├── Core.hs         # Core Pattern type ✅
│       ├── Views.hs        # GraphView typeclass (planned)
│       ├── Graph.hs        # Graph operations (planned)
│       └── Morphisms.hs    # Pattern morphisms (planned)
├── tests/
│   ├── Test.hs             # Test runner
│   └── Spec/
│       └── Pattern/
│           ├── CoreSpec.hs     # Core tests ✅
│           ├── ViewsSpec.hs    # Views tests (planned)
│           ├── GraphSpec.hs    # Graph tests (planned)
│           └── Properties.hs    # Property-based tests (planned)
├── specs/                   # Feature specifications
│   ├── 001-pattern-data-structure/
│   └── 002-basic-pattern-type/
├── DESIGN.md                 # Design documentation
├── TODO.md                   # Implementation roadmap
└── pattern.cabal            # Cabal build configuration
```

**Status**: ✅ Core Pattern type implemented | 🔄 Views and Graph operations (planned)

## Developer Section

### Prerequisites

- **GHC**: 9.8.4 (specified in `cabal.project`)
- **Cabal**: 3.6.2.0 or later (recommended: 3.12.1.0+)
- **Haskell Language Server (HLS)**: Optional but recommended for IDE support

#### Installing Prerequisites

**Using ghcup (recommended)**:
```bash
# Install GHC 9.8.4
ghcup install ghc 9.8.4
ghcup set ghc 9.8.4

# Install Cabal (if needed)
ghcup install cabal
ghcup set cabal

# Install HLS (optional)
ghcup install hls
```

**Verify installation**:
```bash
ghc --version    # Should show 9.8.4
cabal --version  # Should show 3.6.2.0 or later
```

### Setup

1. **Clone the repository**:
   ```bash
   git clone <repository-url>
   cd pattern-hs
   ```

2. **Update Cabal package index** (if needed):
   ```bash
   cabal update
   ```

3. **Verify project structure**:
   ```bash
   ls -la src/Pattern/Core.hs    # Should exist
   ls -la tests/Spec/Pattern/CoreSpec.hs  # Should exist
   ```

### Build

**Build the library**:
```bash
cabal build
```

**Build with verbose output**:
```bash
cabal build --verbose
```

**Build only the library (skip tests)**:
```bash
cabal build lib:pattern
```

**Build with specific GHC version**:
```bash
cabal build --with-compiler=ghc-9.8.4
```

### Test

**Run all tests**:
```bash
cabal test
```

**Run tests with verbose output**:
```bash
cabal test --test-show-details=always
```

**Run specific test suite**:
```bash
cabal test pattern-test
```

**Run tests in watch mode** (if using `ghcid`):
```bash
ghcid --test=':main' --command='cabal repl'
```

### Development Workflow

**Build and test in one command**:
```bash
cabal build && cabal test
```

**Generate Haddock documentation**:
```bash
cabal haddock
```

**Open documentation**:
```bash
# After generating with haddock
open dist-newstyle/build/*/ghc-9.8.4/pattern-0.1.0.0/doc/html/pattern/index.html
```

**Clean build artifacts**:
```bash
cabal clean
```

**Format code** (if using `ormolu` or `brittany`):
```bash
# Example with ormolu
find src tests -name "*.hs" -exec ormolu -m inplace {} \;
```

### Troubleshooting

**Build errors**:
- Ensure GHC 9.8.4 is installed: `ghc --version`
- Update package index: `cabal update`
- Clean and rebuild: `cabal clean && cabal build`

**Test failures**:
- Check test output: `cabal test --test-show-details=always`
- Verify test files compile: `cabal build tests`

**Dependency issues**:
- Update package index: `cabal update`
- Check `pattern.cabal` for version constraints
- Try `cabal build --dry-run` to see dependency resolution

## Quick Start

### Basic Usage

```haskell
import Pattern.Core (Pattern(..))

-- Create a leaf pattern (sequence with no elements)
nodeA :: Pattern String
nodeA = Pattern { value = "A", elements = [] }

-- Create a pattern with elements
nodeB = Pattern { value = "B", elements = [] }
relationship = Pattern { value = "knows", elements = [nodeA, nodeB] }

-- Inspect pattern structure
main = do
  putStrLn $ "Node value: " ++ value nodeA
  putStrLn $ "Elements count: " ++ show (length (elements relationship))
```

### More Examples

See `specs/002-basic-pattern-type/quickstart.md` for comprehensive examples including:
- Creating leaf patterns with different value types
- Creating patterns with multiple elements
- Building nested pattern structures
- Type safety examples

## Documentation

- **Design Documentation**: `DESIGN.md` - Category-theoretic framework and design principles
- **Implementation Roadmap**: `TODO.md` - Planned features and implementation phases
- **Feature Specifications**: `specs/` - Detailed specifications for each feature
- **API Documentation**: Generate with `cabal haddock` or see `specs/*/contracts/type-signatures.md`
- **Quickstart Guide**: `specs/002-basic-pattern-type/quickstart.md`

## Current Status

### ✅ Completed (Feature 002)

- **Pattern Type Definition**: Core `Pattern v` data type with record syntax
- **Field Accessors**: `value` and `elements` accessors
- **Comprehensive Documentation**: Haddock documentation at all levels
- **Test Suite**: 25 test cases covering leaf patterns and patterns with elements

### 🔄 In Progress / Planned

- **Typeclass Instances**: Show, Eq, Functor, Foldable, Traversable (Phase 2-6)
- **Constructor Functions**: `pattern` and `patternWith` (Phase 3)
- **Classification Functions**: `isNode`, `isRelationship`, `isSubgraph`, `isPath` (Phase 8-11)
- **Navigation Functions**: `source`, `target`, `nodes`, `relationships` (Phase 12)
- **Graph Views**: `GraphView` typeclass and implementations (Phase 15)
- **Query Functions**: `length`, `size`, `depth` (Phase 7)

See `TODO.md` for the complete implementation roadmap.

## Contributing

This is a reference implementation with strict quality standards. See `.specify/memory/constitution.md` for development principles and requirements.

**Key Requirements**:
- All code must be well-documented with Haddock
- All public APIs must have tests
- Category-theoretic properties must be verified
- Code must be translatable to other languages

## License

BSD-3-Clause (see `LICENSE` file)

## Author

Pattern HS

