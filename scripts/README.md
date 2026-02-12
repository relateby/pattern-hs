# CI Scripts

Scripts for running CI checks locally and managing project setup.

## init-submodules.sh

Initializes git submodules required for corpus tests.

### Usage

```bash
# From project root
./scripts/init-submodules.sh

# Or from anywhere
cd /path/to/pattern-hs
./scripts/init-submodules.sh
```

### What it does

- Initializes the `tree-sitter-gram` submodule at `libs/gram/test-data/tree-sitter-gram`
- Required for running corpus integration tests
- Tests will skip gracefully if submodules are not initialized

### Alternative: Clone with submodules

```bash
git clone --recurse-submodules <repository-url>
```

## ci-check.sh

Runs all CI checks that would be executed in continuous integration:

1. **Build all packages** - Compiles all libraries and executables
2. **Run all tests** - Executes all test suites with detailed output
3. **Build documentation** - Generates Haddock documentation for all packages

### Usage

```bash
# From project root
./scripts/ci-check.sh

# Or from anywhere
cd /path/to/pattern-hs
./scripts/ci-check.sh
```

### Exit Codes

- `0` - All checks passed
- `1` - One or more checks failed

### Example Output

```
==========================================
Running CI Checks for pattern-hs
==========================================

Running: Build all packages
✓ Build all packages passed

Running: Run all tests
✓ Run all tests passed

Running: Build Haddock documentation
✓ Build Haddock documentation passed

==========================================
All CI checks passed!
```

## Individual Checks

You can also run individual checks manually:

```bash
# Build only
cabal build all

# Test only
cabal test all --test-show-details=direct

# Documentation only
cabal haddock all

# Test specific library
cabal test libs/pattern:test --test-show-details=direct
```
