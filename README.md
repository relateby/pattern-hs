# Pattern HS

A multi-library Haskell mono-repo providing libraries and tools for working with patterns, subjects, and graph structures. The primary library, `pattern`, offers a generalized representation of graph elements, including nodes, relationships, paths and compositions.

## Quick Start

### Installation

**Prerequisites**:
- GHC 9.12.2
- Cabal 3.6.2.0 or later

**Using ghcup**:
```bash
ghcup install ghc 9.12.2
ghcup set ghc 9.12.2
ghcup install cabal
ghcup set cabal
```

**Clone with submodules** (required for corpus tests):
```bash
# When cloning for the first time
git clone --recurse-submodules <repository-url>

# Or if already cloned, initialize submodules
git submodule update --init --recursive
```

The corpus tests require the `tree-sitter-gram` submodule to be initialized at `libs/gram/test-data/tree-sitter-gram`. Tests will skip gracefully if the submodule is not initialized.

**Add to your project**:
```yaml
build-depends:
  - pattern
```

### Basic Usage

```haskell
import Pattern.Core (pattern, patternWith)

-- Create an atomic pattern
atomA = pattern "A"

-- Create a pattern with elements
atomB = pattern "B"
relationship = patternWith "knows" [atomA, atomB]
```

### JSON and Schema Generation

```bash
# Convert Gram notation to canonical JSON
gramref parse pattern.gram --format json --value-only --canonical

# Bidirectional conversion: Gram ↔ JSON
gramref convert pattern.json --from json --to gram

# Generate JSON Schema
gramref schema --format json-schema > pattern-schema.json

# Generate TypeScript types
gramref schema --format typescript > pattern.ts

# Generate Rust types
gramref schema --format rust > pattern.rs
```

## Documentation

- **[User Documentation](docs/guide/01-introduction.md)** - Complete user guide with examples
- **[Reference Documentation](docs/reference/PORTING-GUIDE.md)** - For language porters
- **[Canonical JSON Format](docs/reference/features/canonical-json-format.md)** - JSON format specification with examples
- **User Documentation**: `docs/` - User-facing guides and API documentation
  - [Pattern Construction Functions](docs/reference/features/pattern-construction.md) - Guide to `point`, `pattern`, and `pure` with porting guidance
  - [Gram Serialization](docs/reference/features/gram-serialization.md) - Serialization, JSON, and schema generation
- **Design Documentation**: `design/DESIGN.md` - Category-theoretic framework and design principles
- **Implementation Roadmap**: `TODO.md` - Planned features and implementation phases
- **Feature Specifications**: `specs/` - Detailed specifications for each feature
- **API Documentation**: Generate with `cabal haddock` or see `specs/*/contracts/type-signatures.md`
- **Quickstart Guide**: `specs/002-basic-pattern-type/quickstart.md`

## Testing

Run the full test suite:
```bash
cabal test all
```

**Important Test Requirements**:
- **JSON Roundtrip Tests**: All patterns must successfully roundtrip through JSON serialization (`gram:test:gram-test --match=Roundtrip`)
- **Corpus Tests**: Require `tree-sitter-gram` submodule initialized (tests skip gracefully if not present)
- **Property-Based Tests**: QuickCheck validates 100+ random patterns for structural integrity

## Libraries

- **`pattern`**: Core pattern data structure library (recursive, decorated sequences)
- **`subject`**: Special data structure with index, labels, and property record
- **`gram`**: Serialization/deserialization for "Subject Patterns"
  - Gram notation (text) serialization
  - **Canonical JSON** with bidirectional conversion
  - **JSON Schema** generation (Draft 2020-12)
  - **TypeScript** and **Rust** type generation for downstream ports

## Project Structure

```
pattern-hs/
├── libs/
│   ├── pattern/             # Pattern library
│   ├── subject/             # Subject library
│   └── gram/                # Gram library
├── docs/
│   ├── guide/               # User-facing documentation
│   └── reference/           # Porter-facing documentation
└── apps/
    └── gramref-cli/         # CLI tool for testing and validation
```

## Design Goals
- **Typeclass Instances**: Show ✅, Eq ✅, Functor ✅, Foldable ✅, Traversable (Phase 2-6)
- **Constructor Functions**: `point` (atomic) and `pattern` (with elements) (Phase 3)
- **Classification Functions**: `isNode`, `isRelationship`, `isSubgraph`, `isPath` (Phase 8-11)
- **Navigation Functions**: `source`, `target`, `nodes`, `relationships` (Phase 12)
- **Graph Views**: `GraphView` typeclass and implementations (Phase 15)
- **Query Functions**: `length`, `size`, `depth` (Phase 7)

1. **Reference Implementation**: Canonical reference design for translation to other languages
2. **Category-Theoretic Foundation**: Built on solid mathematical foundations
3. **Schema-Lazy Design**: Patterns don't commit to specific graph semantics at creation time
4. **Multi-Language Translation**: Design prioritizes clarity and mathematical correctness

## Contributing

This is a reference implementation with strict quality standards. See `.specify/memory/constitution.md` for development principles.

**Workflow Requirements**:
- **ALWAYS work in a feature branch** - Never commit directly to `main`
- Use the Speckit workflow (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- Follow the [Constitution](.specify/memory/constitution.md) principles

## License

BSD-3-Clause (see `LICENSE` file)
