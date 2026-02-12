# gramref CLI Implementation Plan

**Status**: ✅ Implemented  
**Implementation**: `apps/pattern-hs-cli/`  
**Reference**: See `apps/pattern-hs-cli/README.md` for current documentation

## Overview

The `gramref` CLI serves as the reference implementation and conformance testing tool for the gram/pattern ecosystem. It provides canonical outputs for pattern operations, generates test suites, and enables validation of ports to other languages.

## Core Commands

### 1. Parse Command
```bash
gramref parse <input-file> [--format json|gram|debug]
```

**Purpose**: Parse gram notation and output canonical representation

**Features**:
- Read gram notation from file or stdin
- Output formats: JSON (canonical), gram (pretty-printed), debug (with parse tree)
- Deterministic output with sorted keys and stable formatting
- Include metadata: version, hash of result, parse time

**Use Cases**:
- Validate gram syntax
- Generate golden test files
- Debug parsing issues in ports

### 2. Match Command
```bash
gramref match <pattern-file> <data-file> [--format json|table|count]
```

**Purpose**: Execute pattern matching and output bindings

**Features**:
- Match pattern against data (both in gram notation)
- Output all bindings or just count of matches
- Include match metadata: positions, paths, confidence scores
- Support for multiple match strategies (first, all, longest)

**Use Cases**:
- Test pattern matching implementations
- Validate matching semantics across ports
- Generate matching test cases

### 3. Transform Command
```bash
gramref transform <operation> <input-file> [--param key=value]
```

**Purpose**: Apply pattern transformations

**Operations**:
- `fold` - Fold pattern with specified function
- `map` - Map function over pattern elements
- `filter` - Filter pattern elements
- `reverse` - Reverse pattern sequences
- `flatten` - Flatten nested patterns
- `normalize` - Canonical normalization

**Features**:
- Parameterized transformations via --param flags
- Pipeline multiple transformations
- Output before/after for validation

**Use Cases**:
- Test transformation correctness
- Generate transformation test cases
- Validate operation semantics

### 4. Generate Command
```bash
gramref generate <generator-type> [--count N] [--seed S] [--complexity C]
```

**Purpose**: Generate test data and patterns

**Generator Types**:
- `pattern` - Random valid patterns
- `graph` - Random graph structures
- `suite` - Complete test suite
- `property` - Property-based test cases

**Features**:
- Deterministic generation with seed
- Complexity levels: minimal, basic, standard, complex, adversarial
- Output formats: gram notation, JSON, test suite structure
- Include both positive and negative test cases

**Use Cases**:
- Create comprehensive test suites for ports
- Generate stress-test data
- Property-based testing scenarios

### 5. Validate Command
```bash
gramref validate <test-suite> [--runner external-command]
```

**Purpose**: Run conformance test suites

**Features**:
- Execute test suite against reference implementation
- Optionally run against external command for comparison
- Produce detailed conformance report
- Support partial compliance checking

**Test Suite Format**:
- YAML or JSON test definitions
- Expected outputs for each operation
- Metadata about required features
- Compliance levels (core, extended, experimental)

**Use Cases**:
- Validate port implementations
- Regression testing
- Compliance certification

### 6. Convert Command
```bash
gramref convert <input-file> --from <format> --to <format>
```

**Purpose**: Convert between different representations

**Supported Formats**:
- `gram` - Gram notation
- `json` - Canonical JSON
- `cypher` - Neo4j Cypher patterns (subset)
- `dot` - Graphviz for visualization
- `mermaid` - Mermaid diagram syntax

**Features**:
- Preserve all pattern information during conversion
- Validate during conversion
- Pretty-printing options
- Batch conversion support

**Use Cases**:
- Import/export to other tools
- Visualization generation
- Format migration

## Output Specifications

### Canonical JSON Format

All JSON output follows strict rules for determinism:

```json
{
  "_meta": {
    "version": "0.1.0",
    "command": "parse",
    "timestamp": "2024-01-01T00:00:00Z",
    "hash": "sha256:abc123..."
  },
  "_result": {
    "type": "Pattern",
    "value": { ... }
  },
  "_diagnostics": {
    "time_ms": 1.23,
    "memory_bytes": 1024
  }
}
```

### Error Format

Consistent error reporting across all commands:

```json
{
  "_error": {
    "type": "ParseError",
    "message": "Unexpected character at position 42",
    "location": {
      "file": "input.gram",
      "line": 3,
      "column": 7,
      "context": "...(node:Label[...]..."
    },
    "suggestion": "Did you mean ')' instead of ']'?"
  }
}
```

## Test Suite Structure

### Directory Layout
```
test-suite/
├── manifest.yaml           # Suite metadata and requirements
├── core/                   # Core compliance tests
│   ├── parsing/           # Gram notation parsing
│   ├── patterns/          # Pattern operations
│   └── matching/          # Pattern matching
├── extended/              # Extended features
│   ├── performance/       # Performance benchmarks
│   └── properties/        # Property-based tests
└── experimental/          # Experimental features
```

### Manifest Format
```yaml
version: "1.0"
name: "gramref-conformance"
levels:
  - core: "Required for basic compliance"
  - extended: "Additional features"
  - experimental: "Optional/future features"
requirements:
  gramref-version: ">=0.1.0"
tests:
  total: 500
  by-level:
    core: 200
    extended: 250
    experimental: 50
```

## Configuration

### Global Config File
```yaml
# ~/.gramref/config.yaml
default_format: json
json_options:
  indent: 2
  sort_keys: true
  include_meta: true
performance:
  timeout_ms: 5000
  memory_limit_mb: 512
```

### Environment Variables
- `GRAM_HS_FORMAT` - Default output format
- `GRAM_HS_SEED` - Default seed for generation
- `GRAM_HS_VERBOSE` - Verbosity level
- `GRAM_HS_CONFIG` - Path to config file

## Integration Features

### Pipe Support
All commands support Unix pipes:
```bash
cat input.gram | gramref parse | jq '.result'
gramref generate pattern --count 10 | gramref validate -
```

### Exit Codes
- `0` - Success
- `1` - Parse error
- `2` - Validation failure  
- `3` - Invalid arguments
- `4` - IO error
- `5` - Timeout/resource limit

### Machine-Readable Output
- `--format json` for all commands
- `--quiet` for minimal output
- `--verbose` for debugging
- `--no-meta` for pure results

## Development Workflow Integration

### Git Hooks
```bash
# pre-commit hook example
gramref validate tests/*.gram --quiet || exit 1
```

### CI/CD Support
```yaml
# GitHub Actions example
- name: Validate Examples
  run: gramref validate examples/ --format junit > results.xml
```

### Editor Integration
- Language server protocol support (future)
- Format on save capability
- Validation during editing

## Performance Considerations

### Streaming Mode
For large files:
```bash
gramref parse --streaming large-file.gram
```

### Batch Processing
```bash
gramref batch process-list.txt --parallel 4
```

### Memory Management
- Configurable memory limits
- Garbage collection hints
- Resource usage reporting

## Versioning Strategy

### Compatibility
- Semantic versioning for CLI
- Separate version for test suite format
- Backward compatibility for core commands

### Feature Flags
```bash
gramref --features experimental parse input.gram
```

## Distribution

### Installation Methods
- Binary releases for major platforms
- Homebrew formula (macOS)
- AUR package (Arch Linux)
- Docker image
- Nix package

### Package Contents
- Single binary executable
- Man pages
- Shell completions (bash, zsh, fish)
- Example test suites

## Success Metrics

1. **Adoption**: Used by 3+ port implementations
2. **Coverage**: Test suite covers 95% of pattern operations
3. **Performance**: Parse 10MB gram file in <1 second
4. **Reliability**: Zero false positives in validation
5. **Usability**: Clear error messages with suggestions

## Implementation Phases

### Phase 1: Core Commands (Week 1-2)
- Parse command with JSON output
- Basic validation framework
- Initial test suite structure

### Phase 2: Test Generation (Week 3)
- Generate command with pattern/graph generators
- Property-based test export
- Seed-based determinism

### Phase 3: Advanced Operations (Week 4)
- Match and transform commands
- Convert between formats
- Pipeline support

### Phase 4: Conformance Suite (Week 5)
- Complete test suite with 200+ tests
- Validation runner
- Compliance reporting

### Phase 5: Polish & Distribution (Week 6)
- Package for multiple platforms
- Documentation and examples
- CI/CD integration guides

## Future Extensions

- **REPL mode** for interactive exploration
- **Language Server Protocol** for IDE integration
- **Web service mode** for online validation
- **Performance profiling** commands
- **Fuzzing support** for finding edge cases
- **Visual diff** for pattern comparisons
