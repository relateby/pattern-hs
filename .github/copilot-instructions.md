# Copilot Instructions for pattern-hs

## Project Overview

This is a multi-library Haskell mono-repo providing tools for working with patterns, subjects, and graph structures. The primary library, `pattern`, offers a generalized representation of graph elements using category theory.

### Design Philosophy

1. **Reference Implementation**: This codebase serves as a canonical reference design for translation to other programming languages while maintaining category-theoretic correctness.

2. **Category-Theoretic Foundation**: Built on solid mathematical foundations from category theory with functors, natural transformations, and morphisms that preserve relevant structure.

3. **Schema-Lazy Design**: Patterns don't commit to specific graph semantics at creation time. Interpretation happens through views.

4. **Decorated Sequence Semantics**: Patterns are conceptually decorated sequences where elements form the pattern itself, and the value provides decoration about that pattern.

## Project Structure

```
pattern-hs/
├── libs/
│   ├── pattern/          # Core Pattern type and category-theoretic operations
│   ├── subject/          # Subject library (planned)
│   └── gram/             # Gram library (planned)
├── apps/
│   └── gramref-cli/      # CLI application
├── specs/                # Feature specifications (Speckit workflow)
├── .specify/
│   └── memory/
│       └── constitution.md  # Project principles and standards
└── cabal.project         # Multi-library project configuration
```

## Development Workflow

### Branch Strategy

**CRITICAL**: Never commit directly to `main`. All development MUST be done in feature branches following the Speckit workflow.

1. **Create Feature Branch**: Use pattern `NNN-feature-name` (e.g., `002-basic-pattern-type`)
2. **Plan Implementation**: Create `specs/NNN-feature-name/spec.md`
3. **Generate Tasks**: Create actionable task list with dependency order
4. **Implement**: Follow tasks ensuring constitution compliance
5. **Merge**: Submit PR after tests pass and code review

### Constitution Compliance (NON-NEGOTIABLE)

All code MUST comply with `.specify/memory/constitution.md`:

- **Code Quality**: Clear, maintainable, well-documented code
- **Testing Standards**: Comprehensive tests as executable specifications
- **Conceptual Consistency**: Align with category theory formalisms
- **Mathematical Clarity**: Explicit formal definitions
- **Multi-Language Reference Alignment**: Structure for translation to other languages

## Build and Test Commands

### Prerequisites
- GHC 9.10.3 (specified in `cabal.project`)
- Cabal 3.6.2.0 or later

### Build Commands

```bash
# Build all libraries
cabal build all

# Build specific library
cabal build lib:pattern

# Build with verbose output
cabal build --verbose
```

### Test Commands

```bash
# Run all tests
cabal test all

# Run tests with verbose output
cabal test all --test-show-details=always

# Run tests for specific library
cabal test libs/pattern
cabal test pattern-test
```

### Code Quality

```bash
# Format check (if available)
# Note: Check for specific formatting tools in use

# Type checking
cabal build --ghc-options="-Wall"
```

## Code Style and Conventions

### Haskell Standards

- Follow standard Haskell style conventions
- Use explicit type signatures for all top-level functions
- Prefer pure functions over stateful operations
- Document complex logic with inline comments explaining "why", not "what"

### Documentation Requirements

- **All public APIs** MUST have Haddock documentation
- Include usage examples for public functions
- Document category-theoretic properties (functor laws, naturality conditions)
- Explain mathematical concepts clearly before implementation

### Testing Requirements

- Every public function MUST have corresponding tests
- Use property-based testing (QuickCheck) for mathematical properties
- Test both happy paths and edge cases
- Tests MUST be independent and runnable in isolation
- Category-theoretic properties MUST be tested explicitly

## Category Theory Guidelines

### Core Concepts

When working with category-theoretic structures:

- **Functors** MUST satisfy functor laws (identity and composition preservation)
- **Natural transformations** MUST satisfy naturality conditions
- **Morphisms** MUST preserve relevant structure
- **Composition** MUST be associative with identity elements

### Naming Conventions

- Use standard category theory nomenclature
- Identify functors, morphisms, and natural transformations explicitly
- Document categorical interpretation in code comments

## Pattern Library Specifics

### Core Data Type

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
```

**Key Insight**: The `elements` field IS the pattern - it contains the sequence that defines the pattern. The `value` field provides decoration about what kind of pattern it is.

### Design Principles

- Elements form the pattern sequence
- Values provide decoration/classification
- Recursive nesting enables complex patterns
- Multiple interpretations through categorical views

## Common Tasks

### Adding New Features

1. Create feature branch following naming convention
2. Write specification in `specs/NNN-feature-name/spec.md`
3. Document design decisions and category-theoretic foundations
4. Implement with comprehensive tests
5. Ensure Haddock documentation for all public APIs
6. Verify constitution compliance
7. Submit PR with clear description

### Writing Tests

- Use Hspec for unit tests
- Use QuickCheck for property-based tests
- Test files mirror source structure: `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`
- Include both positive and negative test cases
- Test mathematical properties explicitly

### Documentation

- Use Haddock for API documentation
- Include `-- |` for function documentation
- Use `-- ^` for field documentation
- Provide examples in documentation
- Document invariants and preconditions

## Important Files

- `.specify/memory/constitution.md` - Project principles and standards
- `cabal.project` - Build configuration and package list
- `README.md` - Comprehensive project documentation
- `TODO.md` - Implementation roadmap

## When Making Changes

1. **Understand the mathematical foundation** before implementing
2. **Write tests first** or alongside implementation (TDD preferred)
3. **Document the category-theoretic structure** explicitly
4. **Maintain consistency** with existing code style
5. **Verify no breaking changes** to public APIs
6. **Update documentation** for any API changes
7. **Test cross-language translation concerns** - avoid Haskell-specific idioms in core data structures

## Red Flags to Avoid

- ❌ Committing directly to `main`
- ❌ Missing tests for public functions
- ❌ Undocumented public APIs
- ❌ Violating category-theoretic laws
- ❌ Poor code quality or unclear naming
- ❌ Missing mathematical foundations in documentation
- ❌ Language-specific optimizations that obscure the reference design

## Questions to Ask

Before implementing features, consider:

1. Does this align with category theory principles?
2. Is this design translatable to other languages?
3. Are the mathematical properties clearly stated?
4. Do I have comprehensive tests?
5. Is the documentation clear for someone unfamiliar with the codebase?
6. Does this follow the constitution's non-negotiable principles?
