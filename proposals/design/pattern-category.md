**Status**: 📝 Design Only  
**Implementation**: Not currently planned. This document explores category-theoretic foundations.

If so, are composed patterns with the same start and end leaf  equivalent, or congruent?

**Compositions via CategoryLens:**

Yes, any composition using `composeWith lens` will produce a valid morphism *structurally*. However, validity in the full categorical sense also requires that objects align (codomain of first equals domain of second), which is checked by `validComposition`.

**Equivalence vs Congruence:**

For composed patterns with the same start and end leaves:

```haskell
-- Both go from b to z
Pattern "comp1" [Pattern "a" [b, c], Pattern "x" [c, z]]  -- leaves: b,c,c,z
Pattern "comp2" [Pattern "p" [b, y], Pattern "q" [y, z]]  -- leaves: b,y,y,z
```

These are **not equivalent** under strict structural equivalence (`structuralEquiv`) - they have different intermediate objects.

However, under endpoint equivalence (`endpointEquiv`), they are **congruent** - they're morphisms in the same hom-set (same source and target):

```haskell
endpointEquiv lens comp1 comp2  -- True: both map b -> z
structuralEquiv lens comp1 comp2  -- False: different paths
```

**Category-theoretic interpretation:**

In category theory, morphisms in the same hom-set (same source and target) can be distinct. Whether two such morphisms are "equal" depends on additional structure:

1. **Free category**: Every distinct path is a distinct morphism (use structural equivalence)
2. **Quotient category**: Paths are equivalent if they satisfy certain relations (use custom equivalence)
3. **Hom-set membership**: Only source and target matter (use endpoint equivalence)

The choice of equivalence relation in `CategoryStructure` determines which interpretation you're working with.

## Example: Building a Category

```haskell
-- Step 1: Define the lens (interpretation)
graphLens :: CategoryLens String
graphLens = CategoryLens
  { valueOp = \v1 v2 -> v1 ++ ";" ++ v2  -- concatenate labels
  , isObject = isLeaf                      -- leaves are objects
  }

-- Step 2: Compositional validity derived for free
canCompose :: Pattern String -> Pattern String -> Bool
canCompose = validComposition graphLens

-- Step 3: Choose equivalence relation (reasoning layer)
strictCategory :: CategoryStructure String
strictCategory = CategoryStructure
  { lens = graphLens
  , morphismEquiv = structuralEquiv graphLens  -- free category
  , validComposition = Nothing  -- use default
  }

looseCategory :: CategoryStructure String  
looseCategory = CategoryStructure
  { lens = graphLens
  , morphismEquiv = endpointEquiv graphLens  -- quotient category
  , validComposition = Nothing
  }

-- Same underlying patterns, two different categories!
-- In strictCategory: comp1 ≠ comp2
-- In looseCategory: comp1 ≡ comp2 (if same endpoints)
```# Inducing Categories from Patterns

## Overview

This document describes how to induce categorical structure from the generic `Pattern` data type through interpretation rather than enforcement. The key insight is that categorical properties emerge from how we *interpret* patterns, not from constraints built into the type itself.

## The Pattern Structure

```haskell
data Pattern v = Pattern v [Pattern v]
```

A pattern consists of:
- A **value** of type `v`
- A list of **elements** that are themselves patterns

This is structurally similar to an s-expression but without computational semantics—it's pure data.

## Categorical Interpretation

### Objects and Morphisms

Rather than fixing what constitutes an "object" or "morphism" in the type system, we define these through predicates:

- **Objects**: Patterns satisfying some predicate (e.g., leaves, patterns at a specific depth, patterns with values in a given set)
- **Morphisms**: Patterns whose leaf sequence represents a path through objects
- **Composition**: Structure-preserving concatenation

### Structure-Preserving Composition

Unlike list concatenation which flattens, pattern composition preserves the structure of composed patterns:

```haskell
compose :: (v -> v -> v) -> Pattern v -> Pattern v -> Pattern v
compose f (Pattern v1 elems1) (Pattern v2 elems2) =
    Pattern (f v1 v2) [Pattern v1 elems1, Pattern v2 elems2]
```

**Example:**
```haskell
Pattern "a" [Pattern "b" [], Pattern "c" []]
  ∘
Pattern "x" [Pattern "y" [], Pattern "z" []]
  =
Pattern (a ⊕ x) 
  [Pattern "a" [Pattern "b" [], Pattern "c" []], 
   Pattern "x" [Pattern "y" [], Pattern "z" []]]
```

The original patterns become elements of the composite, maintaining their internal structure.

## Object Identification

Objects are identified post-hoc through predicates rather than enforced during construction:

```haskell
-- Generic object predicate
isObject :: (Pattern v -> Bool) -> Pattern v -> Bool
isObject predicate p = predicate p

-- Common predicates
atDepth :: Int -> Pattern v -> Bool
atDepth 0 (Pattern _ []) = True
atDepth 0 _ = False
atDepth n (Pattern _ elems) = all (atDepth (n-1)) elems

isLeaf :: Pattern v -> Bool
isLeaf = atDepth 0

-- Value-based membership
inSet :: Eq v => Set v -> Pattern v -> Bool
inSet objSet (Pattern v _) = Set.member v objSet
```

## Morphism Equivalence

Two patterns are equivalent as morphisms if they have the same sequence of objects (leaves at the chosen depth):

```haskell
-- Extract object sequence
leaves :: Pattern v -> [v]
leaves (Pattern v []) = [v]
leaves (Pattern _ elems) = concatMap leaves elems

-- Morphism equivalence
equivalent :: Eq v => Pattern v -> Pattern v -> Bool
equivalent p1 p2 = leaves p1 == leaves p2
```

**Example:**
```haskell
Pattern "comp1" 
  [Pattern "a" [b, c], 
   Pattern "x" [y, z]]

Pattern "comp2" 
  [Pattern "p" [b], 
   Pattern "q" [c, y], 
   Pattern "r" [z]]
```

Both are equivalent if `b, c, y, z` are defined as objects, since both have the same leaf sequence: `[b, c, y, z]`.

## The Value Magma

For composition to be well-defined, values need a binary operation:

```haskell
(⊕) :: v -> v -> v
```

This forms a **magma** (a set with a binary operation). Depending on the domain, this operation might have additional properties:

- **Associative** → Semigroup
- **With identity** → Monoid  
- **With inverses** → Group

The magma operation is **domain-dependent**. Different interpretations of patterns may use different value composition strategies.

## Categorical Properties

Once objects and composition are defined via interpretation:

1. **Composition is associative** (structurally, modulo value magma)
2. **Identity morphisms** exist (empty element list or self-reference, depending on interpretation)
3. **Morphisms compose** when the leaf sequences align appropriately

The category structure is not inherent to `Pattern` but emerges from the chosen interpretation lens.

## Practical Implementation

### Base Layer: CategoryLens

The `CategoryLens` defines the minimal structure needed to interpret patterns categorically:

```haskell
-- Define domain-specific value composition
type ValueCompose v = v -> v -> v

-- Define what counts as objects in this domain
type ObjectPredicate v = Pattern v -> Bool

-- Minimal categorical interpretation
data CategoryLens v = CategoryLens
  { valueOp :: ValueCompose v
  , isObject :: ObjectPredicate v
  }

-- Compose patterns under a given lens
composeWith :: CategoryLens v -> Pattern v -> Pattern v -> Pattern v
composeWith lens = compose (valueOp lens)
```

### Derived Layer: Compositional Validity

Compositional validity can be derived from the lens - it checks whether two patterns can be composed (codomain of first matches domain of second):

```haskell
-- Check if composition is valid (objects align)
validComposition :: Eq v => CategoryLens v 
                 -> Pattern v -> Pattern v -> Bool
validComposition lens p1 p2 = 
  let objs1 = filter (isObject lens) (leaves p1)
      objs2 = filter (isObject lens) (leaves p2)
  in not (null objs1) && not (null objs2) 
     && last objs1 == head objs2  -- codomain matches domain
```

### Higher Layer: Morphism Equivalence

Different notions of when two morphisms are "the same":

```haskell
-- Structural equivalence: same complete leaf sequence
structuralEquiv :: Eq v => CategoryLens v -> Pattern v -> Pattern v -> Bool
structuralEquiv lens p1 p2 = leaves p1 == leaves p2

-- Endpoint equivalence: same source and target (hom-set membership)
endpointEquiv :: Eq v => CategoryLens v -> Pattern v -> Pattern v -> Bool
endpointEquiv lens p1 p2 = 
    let l1 = leaves p1; l2 = leaves p2
    in not (null l1) && not (null l2) 
       && (head l1, last l1) == (head l2, last l2)

-- Custom domain equivalences can be defined as needed
type MorphismEquiv v = Pattern v -> Pattern v -> Bool
```

### Complete Structure

For domains needing explicit categorical reasoning, combine these layers:

```haskell
data CategoryStructure v = CategoryStructure
  { lens :: CategoryLens v
  , morphismEquiv :: Pattern v -> Pattern v -> Bool
  , validComposition :: Maybe (Pattern v -> Pattern v -> Bool)
    -- Nothing means use default derived from lens
    -- Just provides custom compositional validity
  }

-- Helper to get compositional validity
getValidComposition :: Eq v => CategoryStructure v 
                    -> (Pattern v -> Pattern v -> Bool)
getValidComposition cs = case validComposition cs of
    Nothing -> validComposition (lens cs)
    Just vc -> vc
```

## Key Benefits

1. **Construction freedom**: Build patterns without depth or structural constraints
2. **Flexible interpretation**: Same pattern can be interpreted differently by different lenses
3. **Layered reasoning**: Separate concerns of interpretation (lens), validity (derivable), and equivalence (domain-specific)
4. **Domain adaptability**: Value composition, object definitions, and equivalence relations are all domain-specific
5. **Multiple categories from same objects**: Different lenses and equivalence relations can define different categorical structures over the same underlying patterns

## Connection to Gram Notation

In gram notation, patterns can be represented in two forms:

- **Pattern notation**: `[:KNOWS | (:Person), (:Person)]`
- **Path notation**: `(:Person)-[:KNOWS]->(:Person)`

Both map to:
```haskell
Pattern ":KNOWS" [Pattern ":Person" [], Pattern ":Person" []]
```

Under a **graph lens**, this pattern is interpreted as:
- Objects: The leaf patterns (`:Person`)
- Morphism: The 2-element pattern representing an edge
- Value: The edge label (`:KNOWS`)

Other lenses might interpret the same structure differently.

## Applications

### Crossfold
An environment for disciplined structural composition where:
- Pattern represents the transport layer
- Different views (lenses) impose different categorical structures
- Value vs. structure distinction enables flexible knowledge representation

### Schema-as-Category
Graph schemas are small and stable enough to materialize as actual categories:
- Objects: Node labels
- Morphisms: Relationship types
- Compositions: Multi-hop patterns

The pattern structure naturally represents these schema-level categorical relationships.

## Summary

Categories are **induced from patterns through layered interpretation**:

### Layer 1: CategoryLens
Defines minimal categorical structure:
1. What counts as objects (via predicate)
2. How values compose (via magma)

### Layer 2: Compositional Validity
Derived from the lens (or optionally customized):
- Checks if codomain of first pattern matches domain of second

### Layer 3: Morphism Equivalence  
Domain-specific choice:
- Structural equivalence (same leaf sequence)
- Endpoint equivalence (same source and target)
- Custom semantic equivalence

This "schema-lazy" approach provides:
- Maximum flexibility during construction
- Multiple valid interpretations of the same structure
- Separation of interpretation from reasoning
- Clean separation between structural and value information

**Building a category requires:**
1. Define an object predicate and value magma (CategoryLens)
2. Compositional validity is derived for free (or customized if needed)
3. Choose an equivalence relation for morphisms (domain-specific)

**The same set of patterns can support many different categories** by varying the lens, validity rules, or equivalence relations.
