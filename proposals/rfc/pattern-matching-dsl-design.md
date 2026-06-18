# Pattern Matching DSL - Design Reference

**Status**: ❌ Deferred  
**Reason**: Predicate matching (Feature 9) provides similar functionality. Full DSL is extensive (3 layers, 6 phases). Only implement if predicate matching proves insufficient for real use cases.  
**Reference**: See `TODO.md` for detailed analysis

## Overview

This document specifies a pattern matching DSL for the `Pattern` data structure, conceptualized as "regex for recursive patterns." The design follows a layered architecture, progressing from simple structural matching to complex graph patterns with quantification and predicates.

## Core Concepts

### Pattern Data Structure (Existing)
```haskell
data Pattern v = Pattern v [Pattern v]
  -- Represents decorated sequences: a value paired with a list of sub-patterns
  -- Atomic pattern: Pattern v []
  -- Composite pattern: Pattern v [p1, p2, ...]
```

### Analogy to Regular Expressions
- **Regex** matches sequences of characters → **PatternExpr** matches tree-like patterns
- **Regex atoms** (characters) → **Pattern atoms** (values with no elements)
- **Regex quantifiers** (`*`, `+`, `{m,n}`) → **Pattern quantifiers** for repetition
- **Regex alternation** (`|`) → **Pattern alternatives** (`POr`)
- **Regex concatenation** → **Pattern sequencing** (`PThen`)
- **Regex capture groups** → **Pattern variable bindings** (`PBind`)

## Architecture Layers

### Layer 1: Pattern Expressions

```haskell
-- Core pattern expression type
data PatternExpr v where
  -- Basic patterns
  PAny      :: PatternExpr v                              -- Match any pattern
  PAtom     :: v -> PatternExpr v                        -- Match atomic with specific value
  PSequence :: v -> [PatternExpr v] -> PatternExpr v    -- Match decorated sequence
  
  -- Predicates
  PWhere    :: PatternExpr v -> (Pattern v -> Bool) -> PatternExpr v
  
  -- Quantifiers
  PRepeat   :: PatternExpr v -> Quantifier -> PatternExpr v
  
  -- Combinators
  PThen     :: PatternExpr v -> PatternExpr v -> PatternExpr v  -- Sequential
  POr       :: PatternExpr v -> PatternExpr v -> PatternExpr v  -- Alternative
  
  -- Variables for binding
  PBind     :: String -> PatternExpr v -> PatternExpr v

-- Quantifier specification
data Quantifier 
  = Exactly Int
  | Between Int Int
  | AtLeast Int
  | AtMost Int
  | ZeroOrMore    -- *
  | OneOrMore     -- +
```

### Layer 2: Path Patterns

```haskell
data PathPattern v where
  -- Single step in a path
  Step :: PatternExpr v -> PathPattern v
  
  -- Quantified repetition
  Repeat :: PathPattern v -> Quantifier -> PathPattern v
  
  -- Sequential composition
  Seq :: PathPattern v -> PathPattern v -> PathPattern v
  
  -- Bind intermediate patterns
  Capture :: String -> PathPattern v -> PathPattern v
  
  -- Predicates on the entire path
  Constrain :: PathPattern v -> ([(String, Pattern v)] -> Bool) -> PathPattern v
```

### Layer 3: Graph Patterns (Non-linear)

```haskell
data GraphPattern v where
  -- Single path
  Path :: PathPattern v -> GraphPattern v
  
  -- Multiple paths with potential shared variables (equijoins)
  Paths :: [PathPattern v] -> GraphPattern v
  
  -- Logical combinations
  AndGraph :: GraphPattern v -> GraphPattern v -> GraphPattern v
  OrGraph :: GraphPattern v -> GraphPattern v -> GraphPattern v
```

## Matching Engine

### Core Types

```haskell
-- Variable bindings from pattern matching
type Binding v = Map String (Pattern v)

-- Match result with position information
data MatchResult v = MatchResult
  { matchedPattern :: Pattern v
  , captures      :: Binding v
  , matchPath     :: [Int]  -- Path to matched position in tree
  }

-- Compiled pattern for efficiency
newtype CompiledPattern v = CompiledPattern 
  { runCompiled :: Pattern v -> [MatchResult v] }
```

### Core Functions

```haskell
-- Compile pattern expression for efficient matching
compile :: PatternExpr v -> CompiledPattern v

-- Basic matching
match :: PatternExpr v -> Pattern v -> Maybe (MatchResult v)
matchAll :: PatternExpr v -> Pattern v -> [MatchResult v]

-- Find patterns within a larger pattern
find :: PatternExpr v -> Pattern v -> Maybe (MatchResult v)
findAll :: PatternExpr v -> Pattern v -> [MatchResult v]

-- Replace matched patterns
replace :: PatternExpr v -> (MatchResult v -> Pattern v) -> Pattern v -> Pattern v
replaceAll :: PatternExpr v -> (MatchResult v -> Pattern v) -> Pattern v -> Pattern v

-- Check if pattern matches
matches :: PatternExpr v -> Pattern v -> Bool
```

### Monadic Interface

```haskell
-- Pattern matching monad for complex queries
newtype PatternMatch v a = PatternMatch 
  { runMatch :: Pattern v -> Binding v -> [(a, Binding v)] }

instance Monad (PatternMatch v) where
  return a = PatternMatch $ \_ b -> [(a, b)]
  m >>= k = PatternMatch $ \p b -> do
    (a, b') <- runMatch m p b
    runMatch (k a) p b'

-- Combinators
matchAny :: PatternMatch v (Pattern v)
matchAtomic :: (v -> Bool) -> PatternMatch v v
matchSequence :: (v -> Bool) -> PatternMatch v [Pattern v]
whereM :: PatternMatch v a -> (a -> Bool) -> PatternMatch v a
bind :: String -> PatternMatch v a -> PatternMatch v a
```

## Combinator Library

### Basic Combinators

```haskell
-- Pattern construction
atom :: v -> PatternExpr v
atom = PAtom

sequence :: v -> [PatternExpr v] -> PatternExpr v
sequence = PSequence

any :: PatternExpr v
any = PAny

-- Quantifiers
zeroOrMore :: PatternExpr v -> PatternExpr v
zeroOrMore p = PRepeat p ZeroOrMore

oneOrMore :: PatternExpr v -> PatternExpr v
oneOrMore p = PRepeat p OneOrMore

exactly :: Int -> PatternExpr v -> PatternExpr v
exactly n p = PRepeat p (Exactly n)

between :: Int -> Int -> PatternExpr v -> PatternExpr v
between m n p = PRepeat p (Between m n)

-- Combinators
(<~>) :: PatternExpr v -> PatternExpr v -> PatternExpr v
(<~>) = PThen  -- Sequential composition

(<|>) :: PatternExpr v -> PatternExpr v -> PatternExpr v
(<|>) = POr    -- Alternative

-- Predicates
satisfying :: PatternExpr v -> (Pattern v -> Bool) -> PatternExpr v
satisfying = PWhere

-- Binding
as :: String -> PatternExpr v -> PatternExpr v
as = PBind
```

### Derived Combinators

```haskell
-- Optional pattern
optional :: PatternExpr v -> PatternExpr v
optional p = p <|> empty
  where empty = PSequence undefined []  -- Matches empty sequence

-- List of patterns
listOf :: [PatternExpr v] -> PatternExpr v
listOf = foldr (<~>) any

-- At least n repetitions
atLeast :: Int -> PatternExpr v -> PatternExpr v
atLeast n p = PRepeat p (AtLeast n)

-- At most n repetitions
atMost :: Int -> PatternExpr v -> PatternExpr v
atMost n p = PRepeat p (AtMost n)
```

## Usage Examples

### Basic Patterns

```haskell
-- Match any atomic pattern with value > 5
atomicGreaterThan5 :: PatternExpr Int
atomicGreaterThan5 = PAtom 0 `satisfying` (\(Pattern v _) -> v > 5 && null _)

-- Match sequence with exactly 3 elements
threeElements :: PatternExpr v
threeElements = PSequence _ [any, any, any]

-- Match pattern with repeated structure
repeated :: PatternExpr Int
repeated = oneOrMore (atom 1 <~> atom 2)
```

### Path Patterns

```haskell
-- Match path with alternating values
alternating :: PathPattern Int
alternating = Repeat (Step (atom 1 <~> atom 2)) OneOrMore

-- Capture intermediate nodes
withCaptures :: PathPattern v
withCaptures = 
  Capture "start" (Step any) `Seq`
  Repeat (Step any) (Between 1 5) `Seq`
  Capture "end" (Step any)
```

### Graph Patterns

```haskell
-- Match "H" shape: (a)->(x), (b)->(x), (x)->(c)
hShape :: GraphPattern v
hShape = Paths 
  [ Step (as "a" any <~> as "x" any)
  , Step (as "b" any <~> as "x" any)  -- x is shared
  , Step (as "x" any <~> as "c" any)  -- x is shared
  ]

-- Match cycle
cycle :: GraphPattern v
cycle = Paths
  [ Step (as "start" any <~> any)
  , Repeat (Step any) ZeroOrMore
  , Step (any <~> as "start" any)  -- Returns to start
  ]
```

## Implementation Strategy

### Phase 1: Basic Matching
1. Implement `PatternExpr` type
2. Basic matching without quantifiers
3. Simple predicates (`PWhere`)
4. Variable binding (`PBind`)

### Phase 2: Quantification
1. Add `Quantifier` type
2. Implement `PRepeat`
3. Handle backtracking for alternatives
4. Optimize common cases (`*`, `+`)

### Phase 3: Path Patterns
1. Implement `PathPattern` type
2. Add path-level predicates
3. Support capturing intermediate nodes
4. Implement path composition

### Phase 4: Graph Patterns
1. Implement `GraphPattern` type
2. Handle equijoins (shared variables)
3. Support non-linear patterns
4. Add logical operators

### Phase 5: Optimization
1. Compile patterns to efficient matchers
2. Implement memoization
3. Add indexing for large patterns
4. Consider NFA/DFA techniques

### Phase 6: Surface Syntax (Optional)
1. Parser for Cypher-like syntax
2. QuasiQuoter for embedded patterns
3. Pretty-printing for patterns

## Performance Considerations

### Matching Complexity
- Basic matching: O(n*m) where n = pattern size, m = expression size
- With quantifiers: Potentially exponential (backtracking)
- Optimization: Compile to automata where possible

### Memory Usage
- Store compiled patterns for reuse
- Share common sub-patterns
- Lazy evaluation for large result sets

### Optimization Techniques
1. **Memoization**: Cache partial matches
2. **Early termination**: Fail fast on predicates
3. **Index structures**: For large pattern sets
4. **Parallel matching**: For independent sub-patterns

## Testing Strategy

### Property-Based Testing
```haskell
-- Properties to test
prop_matchComplete :: PatternExpr v -> Pattern v -> Bool
prop_matchSound :: PatternExpr v -> Pattern v -> MatchResult v -> Bool
prop_bindingsUnique :: PatternExpr v -> Pattern v -> Bool
prop_quantifierSemantics :: Quantifier -> PatternExpr v -> Pattern v -> Bool
```

### Test Categories
1. **Unit tests**: Individual combinators
2. **Integration tests**: Complex patterns
3. **Performance tests**: Large patterns and datasets
4. **Regression tests**: Edge cases and bug fixes

## Future Extensions

### Potential Features
1. **Negation patterns**: Match patterns that don't satisfy a condition
2. **Lookahead/lookbehind**: Assert without consuming
3. **Recursive patterns**: Self-referential pattern definitions
4. **Pattern grammars**: Define pattern languages
5. **Incremental matching**: For streaming data
6. **Pattern inference**: Learn patterns from examples

### Integration Points
1. **Serialization**: Convert patterns to/from text format
2. **Visualization**: Graphical representation of patterns
3. **Query optimization**: Rewrite patterns for efficiency
4. **Static analysis**: Verify pattern properties
5. **Code generation**: Compile to efficient matchers

## References

### Inspiration Sources
- Neo4j Cypher pattern matching
- Regular expression theory
- Tree automata
- XPath/XQuery patterns
- Datalog query patterns

### Key Papers
- "A Play on Regular Expressions" (Functional Pearl)
- "Derivatives of Regular Expressions" (Brzozowski)
- "Tree Automata Techniques and Applications"
- "Pattern Matching in Trees and Nets" (Hoffmann & O'Donnell)

## Appendix: Comparison with Cypher

| Cypher Feature | PatternExpr Equivalent |
|---------------|------------------------|
| `(n)` | `any` |
| `(n:Label)` | `any \`satisfying\` hasLabel` |
| `(n {prop: value})` | `any \`satisfying\` hasProp` |
| `()-[r]->()` | `any <~> any` |
| `()-[*1..3]->()` | `between 1 3 any` |
| `((n)-[r]->(m))+` | `oneOrMore (any <~> any)` |
| `SHORTEST` | Use `find` with early termination |
| Graph patterns | `GraphPattern` type |