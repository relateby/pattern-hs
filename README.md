# Gram HS

A multi-library Haskell mono-repo providing tools for working with patterns, subjects, and graph structures. The primary library, `pattern`, offers a generalized representation of graph elements using category theory. Pattern is a recursive data structure that can be interpreted as different graph elements (nodes, relationships, subgraphs, paths) through categorical views.

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

4. **Decorated Sequence Semantics**: Conceptually, patterns are decorated sequences where the elements form the pattern itself, and the value provides decoration about that pattern. While implemented using a recursive tree structure, the primary semantic is that elements form the pattern sequence itself, enabling intuitive pattern matching and manipulation.

5. **Multi-Language Translation**: The design prioritizes clarity and mathematical correctness over language-specific optimizations, making it easier to translate to other languages while preserving conceptual consistency.

### Key Principles

- **Code Quality (NON-NEGOTIABLE)**: All code must be clear, maintainable, and well-documented
- **Testing Standards (NON-NEGOTIABLE)**: Comprehensive testing serves as both verification and executable specifications
- **Conceptual Consistency**: All implementations align with category theory formalisms
- **Mathematical Clarity**: Formal definitions and mathematical properties are explicitly stated
- **Cross-Language Alignment**: Core design is language-agnostic and translatable

## Core Concept: Patterns as Decorated Sequences

A Pattern is conceptually a **decorated sequence**: the elements form the pattern itself, and the value provides decoration about that pattern. For example, the pattern "A B B A" with decoration "Enclosed rhyme" represents a specific sequence pattern (A B B A) that is classified as an "Enclosed rhyme".

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
```

**Key Insight**: The `elements` field IS the pattern - it contains the sequence that defines the pattern. The `value` field provides decoration about what kind of pattern it is. Each element in the sequence is itself a Pattern, enabling recursive nesting while maintaining the decorated sequence semantic.

### Sequence vs Tree: Two Complementary Views

Patterns have two complementary views that work together:

**Primary Semantic (Conceptual)**: Patterns are decorated sequences where elements form the pattern itself. The sequence order is essential - patterns like "A B B A" depend on element order.

**Implementation Detail**: Patterns are implemented as recursive trees in memory. Each tree node stores a decoration (value) and contains the pattern elements as a list. The tree structure supports sequence operations.

**Relationship**: The tree implementation supports the sequence semantic. Tree nodes store sequences (lists), tree traversal preserves order, and the recursive structure enables nested sequences. There's no contradiction - the tree is simply how sequences are represented in memory.

This dual view enables:
- Intuitive pattern matching (the pattern is the sequence)
- Clear length semantics (number of elements in the pattern)
- Natural composition operations
- Graph element interpretation through views
- Efficient recursive operations via tree structure

## Project Structure

This is a multi-library mono-repo containing:

- **`pattern`**: Core pattern data structure library (recursive, decorated sequences)
- **`subject`**: Special data structure with index, labels, and property record (planned)
- **`gram`**: Serialization/deserialization for "Subject Patterns" (planned)

```
gram-hs/
├── libs/
│   ├── pattern/             # Pattern library
│   │   ├── src/
│   │   │   ├── Pattern.hs          # Main module (re-exports)
│   │   │   └── Pattern/
│   │   │       ├── Core.hs         # Core Pattern type ✅
│   │   │       ├── Views.hs        # GraphView typeclass (planned)
│   │   │       ├── Graph.hs        # Graph operations (planned)
│   │   │       └── Morphisms.hs    # Pattern morphisms (planned)
│   │   ├── tests/
│   │   │   ├── Test.hs             # Test runner
│   │   │   └── Spec/
│   │   │       └── Pattern/
│   │   │           ├── CoreSpec.hs     # Core tests ✅
│   │   │           ├── ViewsSpec.hs    # Views tests (planned)
│   │   │           ├── GraphSpec.hs    # Graph tests (planned)
│   │   │           └── Properties.hs    # Property-based tests (planned)
│   │   ├── examples/        # Pattern library examples
│   │   ├── pattern.cabal    # Pattern library build configuration
│   │   └── CHANGELOG.md     # Pattern library changelog
│   ├── subject/             # Subject library (planned)
│   └── gram/                # Gram library (planned)
├── specs/                   # Shared feature specifications
│   ├── 001-pattern-data-structure/
│   └── 002-basic-pattern-type/
├── design/                  # Shared design documentation
│   ├── DESIGN.md
│   └── pattern-matching-dsl-design.md
├── cabal.project            # Root-level build configuration
├── README.md                # This file
├── LICENSE                   # Project license
└── TODO.md                   # Implementation roadmap
```

**Status**: ✅ Core Pattern type implemented | 🔄 Views and Graph operations (planned) | 📋 Subject and Gram libraries (planned)

## Developer Section

### Development Workflow

**IMPORTANT**: All development work MUST be done in feature branches, following the Speckit workflow that adheres to the [Constitution](.specify/memory/constitution.md). **Never commit directly to the `main` branch.**

#### Speckit Workflow

This project uses the Speckit workflow for feature development:

1. **Create Feature Branch**: Use `/speckit.specify` to create a new feature branch and specification
   - Branches follow the pattern: `NNN-feature-name` (e.g., `002-basic-pattern-type`)
   - Creates a feature specification in `specs/NNN-feature-name/spec.md`

2. **Plan Implementation**: Use `/speckit.plan` to generate implementation plan
   - Creates `plan.md` with technical context and constitution checks
   - Generates design artifacts: `data-model.md`, `contracts/`, `quickstart.md`

3. **Generate Tasks**: Use `/speckit.tasks` to create actionable task list
   - Creates `tasks.md` with dependency-ordered implementation tasks

4. **Implement**: Follow the tasks, ensuring all code adheres to:
   - [Constitution principles](.specify/memory/constitution.md) (Code Quality, Testing Standards, etc.)
   - Comprehensive Haddock documentation
   - Test coverage for all public APIs

5. **Merge**: After all tests pass and code review, merge the feature branch to `main`

#### Constitution Compliance

All code MUST comply with the [Pattern HS Constitution](.specify/memory/constitution.md):

- **Code Quality (NON-NEGOTIABLE)**: Clear, maintainable, well-documented code
- **Testing Standards (NON-NEGOTIABLE)**: Comprehensive tests as executable specifications
- **Conceptual Consistency**: Align with category theory formalisms
- **Mathematical Clarity**: Explicit formal definitions
- **Multi-Language Reference Alignment**: Structure for translation to other languages

See `.specify/memory/constitution.md` for complete requirements.

### Prerequisites

- **GHC**: 9.10.3 (specified in `cabal.project`)
- **Cabal**: 3.6.2.0 or later (recommended: latest)
- **Haskell Language Server (HLS)**: Optional but recommended for IDE support

#### Installing Prerequisites

**Using ghcup (recommended)**:
```bash
# Install GHC 9.10.3
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3

# Install Cabal (if needed)
ghcup install cabal
ghcup set cabal

# Install HLS (optional)
ghcup install hls
```

**Verify installation**:
```bash
ghc --version    # Should show 9.10.3
cabal --version  # Should show 3.6.2.0 or later
```

### Setup

1. **Clone the repository**:
   ```bash
   git clone git@github.com:gram-data/gram-hs.git
   cd gram-hs
   ```

2. **Update Cabal package index** (if needed):
   ```bash
   cabal update
   ```

3. **Verify project structure**:
   ```bash
   ls -la libs/pattern/src/Pattern/Core.hs    # Should exist
   ls -la libs/pattern/tests/Spec/Pattern/CoreSpec.hs  # Should exist
   ```

4. **Create a feature branch for your work**:
   ```bash
   # Use /speckit.specify command in Cursor to create a feature branch
   # Or manually: git checkout -b NNN-feature-name
   # Never commit directly to main!
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

**Build only a specific library (skip tests)**:
```bash
cabal build lib:pattern  # Build pattern library
# Future: cabal build lib:subject  # Build subject library
# Future: cabal build lib:gram     # Build gram library
```

**Build all libraries**:
```bash
cabal build all
```

**Build with specific GHC version**:
```bash
cabal build --with-compiler=ghc-9.10.3
```

### Test

**Run all tests** (from project root):
```bash
cabal test all
```

**Run tests with verbose output**:
```bash
cabal test all --test-show-details=always
```

**Run tests for a specific library**:
```bash
cabal test libs/pattern
# Or by test suite name:
cabal test pattern-test
```

**Note**: In a multi-library project, you must specify `all` or a specific package when running `cabal test` from the root directory.

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
# Find the correct path (architecture may vary)
open $(find dist-newstyle -name "index.html" -path "*/doc/html/pattern/*" | head -1 | xargs dirname)/index.html
```

**Generate with additional options** (recommended for GitHub Pages):
```bash
cabal haddock lib:pattern --haddock-html --haddock-quickjump --haddock-hyperlink-source
```

## Publishing Documentation to GitHub Pages

The project includes a GitHub Actions workflow (`.github/workflows/haddock.yml`) that automatically builds and deploys Haddock documentation to GitHub Pages.

### Automatic Deployment

The workflow runs automatically on pushes to `main` branch when source files or `pattern.cabal` change. To enable:

1. **Enable GitHub Pages** in repository settings:
   - Go to Settings → Pages
   - Source: "GitHub Actions"

2. **Push to trigger workflow**:
   ```bash
   git add .github/workflows/haddock.yml
   git commit -m "Add Haddock documentation deployment"
   git push
   ```

3. **Documentation will be available at**:
   `https://<username>.github.io/gram-hs/` (or your repository's Pages URL)

### Manual Deployment

To manually deploy documentation:

```bash
# Generate documentation
cabal haddock lib:pattern --haddock-html --haddock-quickjump --haddock-hyperlink-source

# Find the output directory
DOC_DIR=$(find dist-newstyle -name "index.html" -path "*/doc/html/pattern/*" | head -1 | xargs dirname)

# Create/update gh-pages branch
git checkout --orphan gh-pages
git rm -rf .
cp -r "$DOC_DIR"/* .
git add .
git commit -m "Update Haddock documentation"
git push origin gh-pages
git checkout main
```

**Note**: The GitHub Actions workflow is recommended as it handles path variations and updates automatically.

**Format code** (if using `ormolu` or `brittany`):
```bash
# Example with ormolu - format all libraries
find libs -name "*.hs" -exec ormolu -m inplace {} \;
```

### Troubleshooting

**Build errors**:
- Ensure GHC 9.10.3 is installed: `ghc --version`
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

-- Create an atomic pattern (sequence with no elements)
atomA :: Pattern String
atomA = Pattern { value = "A", elements = [] }

-- Create a pattern with elements
atomB = Pattern { value = "B", elements = [] }
pair = Pattern { value = "knows", elements = [atomA, atomB] }

-- Inspect pattern structure
main = do
  putStrLn $ "Pattern value: " ++ value atomA
  putStrLn $ "Elements count: " ++ show (length (elements pair))
```

### More Examples

See `specs/002-basic-pattern-type/quickstart.md` for comprehensive examples including:
- Creating atomic patterns with different value types
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
- **Test Suite**: 25 test cases covering atomic patterns and patterns with elements

### 🔄 In Progress / Planned

- **Typeclass Instances**: Show ✅, Eq ✅, Functor ✅, Foldable ✅, Traversable (Phase 2-6)
- **Constructor Functions**: `pattern` and `patternWith` (Phase 3)
- **Classification Functions**: `isNode`, `isRelationship`, `isSubgraph`, `isPath` (Phase 8-11)
- **Navigation Functions**: `source`, `target`, `nodes`, `relationships` (Phase 12)
- **Graph Views**: `GraphView` typeclass and implementations (Phase 15)
- **Query Functions**: `length`, `size`, `depth` (Phase 7)

See `TODO.md` for the complete implementation roadmap.

## Contributing

This is a reference implementation with strict quality standards. See `.specify/memory/constitution.md` for development principles and requirements.

**Workflow Requirements**:
- **ALWAYS work in a feature branch** - Never commit directly to `main`
- Use the Speckit workflow (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- Follow the [Constitution](.specify/memory/constitution.md) principles

**Code Quality Requirements**:
- All code must be well-documented with Haddock
- All public APIs must have tests
- Category-theoretic properties must be verified
- Code must be translatable to other languages

## License

BSD-3-Clause (see `LICENSE` file)

## Author

Pattern HS

