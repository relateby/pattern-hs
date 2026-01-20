# Porting Guide: Implementation Roadmap

**Audience**: Developers porting this library to other languages  
**Purpose**: Recommended implementation order and dependencies

## Overview

This guide provides a recommended implementation order for porting the gram-hs libraries to other languages. The order is designed to minimize dependencies and build a solid foundation before adding serialization capabilities.

## Implementation Phases

### Phase 1: Pattern Library (Foundation)
**Priority**: Must be first  
**Dependencies**: None  
**Reference**: `docs/reference/features/core-pattern.md`

The Pattern library is the foundation. All other libraries depend on it.

#### Implementation Order:
1. **Core Pattern Type** (`Pattern v`)
   - Data structure: `Pattern { value :: v, elements :: [Pattern v] }`
   - Field accessors: `value`, `elements`
   - See: `docs/reference/features/core-pattern.md#core-type`

2. **Basic Typeclass Instances**
   - `Show`, `Eq` (required for testing)
   - `Functor` (foundational for transformations)
   - `Foldable`, `Traversable` (enables many operations)
   - See: `docs/reference/features/typeclass-instances.md`

3. **Construction Functions**
   - `pattern :: v -> Pattern v`
   - `patternWith :: v -> [Pattern v] -> Pattern v`
   - See: `docs/reference/features/core-pattern.md#construction`

4. **Query Functions**
   - `length`, `size`, `depth`, `values`
   - See: `docs/reference/features/core-pattern.md#queries`

5. **Advanced Typeclasses** (as needed)
   - `Ord`, `Semigroup`, `Monoid`, `Hashable`, `Applicative`, `Comonad`
   - See: `docs/reference/features/typeclass-instances.md`

6. **Paramorphism** (structure-aware folding)
   - `para :: (Pattern v -> [r] -> r) -> Pattern v -> r`
   - Enables structure-aware aggregations
   - See: `docs/reference/features/paramorphism.md` and section below

7. **Graph Lens** (optional but recommended)
   - Enables graph interpretations
   - See: `docs/reference/features/graph-lens.md`

**Testing Strategy**: Implement tests alongside each feature. Tests serve as executable specifications.

---

### Phase 2: Subject Library (Value Type)
**Priority**: Before Gram serialization  
**Dependencies**: None (can be done in parallel with Pattern)  
**Reference**: `docs/reference/features/subject.md`

The Subject library provides a specialized value type for graph data (identity, labels, properties). It's used by Gram but doesn't depend on Pattern.

#### Implementation Order:
1. **Core Subject Type**
   - `Subject { identity :: Symbol, labels :: Set String, properties :: PropertyRecord }`
   - See: `docs/reference/features/subject.md#core-type`

2. **Value Types**
   - Standard types: String, Integer, Boolean, etc.
   - Extended types: Tagged strings, arrays, maps, ranges, measurements
   - See: `docs/reference/features/subject.md#value-types`

3. **Subject Operations**
   - Identity management
   - Label operations
   - Property access
   - See: `docs/reference/features/subject.md#operations`

**Note**: Subject can be implemented in parallel with Pattern Phase 1-4, but must be complete before Gram.

---

### Phase 3: Gram Serialization (Integration)
**Priority**: After Pattern and Subject  
**Dependencies**: Pattern library, Subject library  
**Reference**: `docs/reference/features/gram-serialization.md`

Gram serialization converts `Pattern Subject` to/from text notation. It requires both Pattern and Subject to be complete.

#### Gram Documents

A Gram Document represents the top-level container for patterns, potentially including a header record for metadata.

1. **Serialization**
   - `toGram :: Pattern Subject -> String`: Serialize a single pattern.
   - `toGramList :: [Pattern Subject] -> String`: Serialize multiple patterns.
   - `toGramWithHeader :: PropertyRecord -> [Pattern Subject] -> String`: Serialize a header and patterns.

2. **Parsing**
   - `fromGram :: String -> Either ParseError (Pattern Subject)`: Parse to a single pattern (wraps multiple patterns).
   - `fromGramList :: String -> Either ParseError [Pattern Subject]`: Parse to a list of patterns.
   - `fromGramWithHeader :: String -> Either ParseError (Maybe PropertyRecord, [Pattern Subject])`: Parse to an optional header and patterns.

#### Implementation Order:
1. **Serialization (`toGram`)**
   - Convert `Pattern Subject` to gram notation string
   - Handle all value types
   - Handle anonymous subjects
   - See: `docs/reference/features/gram-serialization.md#serialization`

2. **Parsing (`fromGram`)**
   - Parse gram notation to `Pattern Subject`
   - Handle pattern and path notation
   - See: `docs/reference/features/gram-serialization.md#parsing`

3. **Validation**
   - Duplicate definition checking
   - Undefined reference checking
   - Arity consistency checking
   - See: `docs/reference/features/gram-serialization.md#validation`

4. **Round-Trip Testing**
   - Verify `toGram . fromGram = id` (modulo formatting)
   - Test against corpus
   - See: `docs/reference/features/gram-serialization.md#testing`

---

## Dependency Graph

```
Pattern Library (Phase 1)
    │
    ├─→ Gram Library (Phase 3)
    │
Subject Library (Phase 2) ──┘
```

**Key Points**:
- Pattern has no dependencies
- Subject has no dependencies
- Gram depends on both Pattern and Subject
- Pattern and Subject can be developed in parallel

---

## Feature Implementation Within Each Phase

Within each phase, follow the feature order from `docs/history/specs/`:

### Pattern Library Features (in order):
1. Core data type
2. Basic typeclasses (Show, Eq)
3. Construction functions
4. Functor instance
5. Foldable instance
6. Traversable instance
7. Query functions
8. Advanced typeclasses (Ord, Semigroup, Monoid, Hashable, Applicative)
9. Predicate matching
10. Comonad instance
11. Graph Lens

### Subject Library Features:
[Order TBD based on subject library specs]

### Gram Library Features:
1. Serialization
2. Parsing
3. Validation
4. Round-trip verification

---

## Using the `gram-hs` CLI Tool for Testing

The `gram-hs` CLI tool serves as the **reference implementation** and **conformance testing tool**. It provides canonical outputs that you can use to validate your port.

### Building the CLI Tool

From the gram-hs repository root:

```bash
cabal build gram-hs-cli
```

Or install globally:

```bash
cabal install gram-hs-cli
```

### Key Testing Workflows

#### 1. Generate Test Cases

Generate test patterns and data for your implementation:

```bash
# Generate test patterns
gram-hs generate --type pattern --count 100 --format json > test-patterns.json

# Generate test graphs
gram-hs generate --type graph --count 50 --format gram > test-graphs.gram

# Generate property-based test suites
gram-hs generate --type property --complexity standard --format json > property-tests.json
```

**Use Cases**:
- Generate test data for your test suite
- Create edge cases with `--complexity adversarial`
- Ensure deterministic tests with `--seed <value>`

#### 2. Get Canonical Reference Outputs

Use the CLI to get canonical JSON outputs for comparison:

```bash
# Parse a gram file and get canonical JSON
gram-hs parse input.gram --format json > reference-output.json

# Transform a pattern and get canonical result
gram-hs transform --operation flatten input.gram --format json > reference-flatten.json
```

**Use Cases**:
- Compare your parser output against canonical JSON
- Verify transformation operations match reference behavior
- Validate round-trip serialization (parse → serialize → parse)

#### 3. Validate Your Implementation

Use the validate command to run conformance tests:

```bash
# Run tests against reference implementation
gram-hs validate test-suite.json

# Run tests against your port (external command)
gram-hs validate test-suite.json --runner "your-gram-tool parse"
```

**Use Cases**:
- Run the full conformance test suite
- Compare your implementation's output against reference
- Automate regression testing

#### 4. Parse and Serialize Testing

Test your parser and serializer:

```bash
# Step 1: Get canonical parse result
gram-hs parse input.gram --format json > canonical.json

# Step 2: Your parser should produce equivalent output
your-gram-tool parse input.gram > your-output.json

# Step 3: Compare (JSON should be equivalent modulo formatting)
diff <(jq -S . canonical.json) <(jq -S . your-output.json)
```

#### 5. Round-Trip Testing

Verify serialization round-trips:

```bash
# Original gram file
cat original.gram

# Parse to canonical JSON
gram-hs parse original.gram --format json > parsed.json

# Serialize back to gram (if your tool supports this)
gram-hs convert parsed.json --from json --to gram > roundtrip.gram

# Parse again and compare
gram-hs parse roundtrip.gram --format json > roundtrip-parsed.json

# Should be equivalent (modulo formatting/ordering)
```

### Output Format: Canonical JSON

All JSON output from `gram-hs` follows strict rules for determinism:

- **Sorted keys**: Object keys are always sorted
- **Stable formatting**: Consistent whitespace and structure
- **Metadata**: Includes version, command, timestamp, and hash
- **Deterministic**: Same input always produces same output

This makes it easy to compare your implementation's output:

```bash
# Normalize both outputs and compare
jq -S . reference.json > ref-normalized.json
jq -S . your-output.json > your-normalized.json
diff ref-normalized.json your-normalized.json
```

### Testing Workflow Example

Here's a complete workflow for testing your parser:

```bash
# 1. Generate test cases
gram-hs generate --type pattern --count 20 --seed 42 --format gram > test-cases.gram

# 2. Split into individual test files (if needed)
split -l 1 test-cases.gram test-case-

# 3. For each test case:
for file in test-case-*; do
  # Get reference output
  gram-hs parse "$file" --format json > "${file}.ref.json"
  
  # Your parser output
  your-gram-tool parse "$file" > "${file}.your.json"
  
  # Compare (should match)
  if ! diff <(jq -S . "${file}.ref.json") <(jq -S . "${file}.your.json"); then
    echo "Mismatch in $file"
  fi
done
```

### Available Commands

| Command | Purpose | Testing Use Case |
|---------|---------|------------------|
| `parse` | Parse gram notation | Get canonical parse results |
| `generate` | Generate test data | Create test cases |
| `validate` | Run test suites | Conformance testing |
| `transform` | Apply transformations | Test operation correctness |
| `convert` | Format conversion | Test serialization |
| `match` | Pattern matching | Test matching operations |

### Exit Codes

The CLI uses standard exit codes for automation:

- `0` - Success
- `1` - Parse error
- `2` - Validation failure
- `3` - Invalid arguments
- `4` - IO error
- `5` - Timeout/resource limit

Use these in CI/CD pipelines:

```bash
if gram-hs validate test-suite.json --runner "your-tool"; then
  echo "All tests passed"
else
  echo "Tests failed"
  exit 1
fi
```

### Integration with Your Test Suite

1. **Generate test data** using `gram-hs generate`
2. **Get reference outputs** using `gram-hs parse/transform`
3. **Compare your results** against canonical JSON
4. **Automate validation** using `gram-hs validate`

This ensures your port produces equivalent results to the reference implementation.

**See Also**: `apps/gram-hs-cli/README.md` for complete CLI documentation

---

## Testing Strategy

**Critical**: Tests are executable specifications. Implement tests alongside code.

1. **Unit Tests**: Each function/feature
2. **Property Tests**: Typeclass laws, invariants
3. **Integration Tests**: Cross-library functionality
4. **Conformance Tests**: Gram parsing against corpus

See `docs/reference/IMPLEMENTATION.md#testing` for patterns.

---

## Paramorphism Implementation

### Overview

Paramorphism enables structure-aware folding over patterns. Unlike `Foldable` (which only provides values), paramorphism gives your folding function access to the full pattern structure at each position.

### Core Function Signature

```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
```

**Parameters**:
- Folding function: `(Pattern v -> [r] -> r)` that receives:
  - Current pattern subtree: `Pattern v`
  - Recursively computed results from children: `[r]`
- Pattern to fold over: `Pattern v`

**Returns**: Aggregated result of type `r`

### Implementation Pattern

The standard paramorphism pattern for recursive tree structures:

```haskell
para f (Pattern v els) = 
  f (Pattern v els) (map (para f) els)
```

**Breakdown**:
1. Recursively compute results for all children using `para f`
2. Apply folding function `f` to: (1) current pattern subtree, (2) list of child results
3. Return aggregated result

### Language-Specific Implementations

#### Rust

```rust
impl<T> Pattern<T> {
    pub fn para<R, F>(&self, f: F) -> R
    where
        F: Fn(&Pattern<T>, &[R]) -> R,
    {
        let child_results: Vec<R> = self.elements.iter()
            .map(|child| child.para(&f))
            .collect();
        f(self, &child_results)
    }
}
```

**Key Considerations**:
- Use references (`&Pattern<T>`) to avoid ownership issues
- Collect child results into `Vec<R>` before passing to folding function
- Generic over result type `R` and folding function `F`

#### TypeScript

```typescript
function para<T, R>(
  pattern: Pattern<T>,
  f: (pat: Pattern<T>, childResults: R[]) => R
): R {
  const childResults = pattern.elements.map(child => para(child, f));
  return f(pattern, childResults);
}
```

**Key Considerations**:
- TypeScript's type system supports generic functions
- Array methods (`map`) work naturally with recursive structures
- No special ownership concerns (managed by runtime)

#### Python

```python
def para(pattern, f):
    """
    Paramorphism: structure-aware folding.
    
    Args:
        pattern: Pattern to fold over
        f: Folding function (pattern, child_results) -> result
    
    Returns:
        Aggregated result
    """
    child_results = [para(child, f) for child in pattern.elements]
    return f(pattern, child_results)
```

**Key Considerations**:
- Python's dynamic typing simplifies implementation
- List comprehensions provide clean recursive computation
- No type annotations required (but recommended for clarity)

### Relationship to Foldable and Comonad

**Foldable** (value-only folding):
- Operations: `foldr`, `foldl`, `foldMap`, `toList`
- Access: Only values, no structure
- Use when: You only need values, not structural information

**Paramorphism** (structure-aware folding):
- Operations: `para`
- Access: Structure + values for folding/aggregation
- Use when: You need structure-aware aggregations (depth-weighted sums, nesting-level statistics)

**Comonad** (structure-aware transformation):
- Operations: `extend`, `duplicate`, `extract`
- Access: Structure for transformation
- Use when: You need structure-aware transformation (not aggregation)

### Example: Depth-Weighted Sum

**Haskell**:
```haskell
depthWeightedSum = para (\pat rs -> value pat * depth pat + sum rs)
```

**Rust**:
```rust
let depth_weighted_sum = pattern.para(|pat, rs| {
    pat.value * pat.depth() + rs.iter().sum::<i32>()
});
```

**TypeScript**:
```typescript
const depthWeightedSum = para(pattern, (pat, rs) => 
    pat.value * depth(pat) + rs.reduce((a, b) => a + b, 0)
);
```

**Python**:
```python
def depth_weighted_sum(pattern):
    return para(pattern, lambda pat, rs: 
        pat.value * depth(pat) + sum(rs)
    )
```

### Testing

Verify paramorphism properties:

1. **Structure Access**: `para (\p _ -> depth p) pattern == depth pattern`
2. **Value Access**: `para (\p rs -> value p : concat rs) pattern == toList pattern`
3. **Relationship to Foldable**: `para (\p rs -> value p + sum rs) pattern == foldr (+) 0 pattern`

### Performance Considerations

- **Time Complexity**: O(n) where n is total number of nodes
- **Space Complexity**: O(d) where d is maximum nesting depth (recursion stack)
- **Order Preservation**: Element order is preserved when aggregating results

## Language-Specific Considerations

### Type Systems
- **Statically typed languages**: Use generics/templates for `Pattern v`
- **Dynamically typed languages**: Use runtime type checking
- **Functional languages**: Leverage algebraic data types
- **OOP languages**: Consider sealed classes/interfaces

### Memory Management
- **GC languages**: Recursive structures work naturally
- **Manual memory**: Consider reference counting or smart pointers
- **Rust**: Use `Box<Pattern>` for recursive types

### Typeclass/Trait Implementation
- **Haskell typeclasses**: Map to traits (Rust), interfaces (Java/C#), protocols (Swift)
- **Laws**: Verify functor laws, monoid laws, etc.
- See `docs/reference/IMPLEMENTATION.md#typeclass-patterns`

---

## Getting Started Checklist

- [ ] Read `docs/reference/ARCHITECTURE.md` (design principles)
- [ ] Read `docs/reference/SPECIFICATION.md` (feature overview)
- [ ] Set up test infrastructure (tests are critical)
- [ ] Start with Pattern Phase 1 (core type)
- [ ] Implement tests alongside code
- [ ] Reference `docs/reference/features/` for each feature
- [ ] Check `docs/reference/IMPLEMENTATION.md` for patterns
- [ ] Build and use `gram-hs` CLI tool for testing

---

## Questions?

- **Design questions**: See `docs/reference/ARCHITECTURE.md`
- **Feature details**: See `docs/reference/features/`
- **Implementation patterns**: See `docs/reference/IMPLEMENTATION.md`
- **Historical context**: See `docs/history/specs/` (development sequence)

