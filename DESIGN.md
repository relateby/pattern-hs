# Patterns as Graph Views: A Categorical Framework

## Core Data Structure

Patterns form a recursive data structure representing decorated sequences that can be interpreted as graphs through different views:

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
  deriving (Eq)
```

**Note**: `Show` is implemented as a manual instance. `Functor`, `Foldable`, and `Traversable` are planned but not yet implemented.

**Key Insight**: The `elements` field IS the pattern - it contains the sequence that defines the pattern. The `value` field provides decoration about what kind of pattern it is. For example, the pattern "A B B A" with decoration "Enclosed rhyme" represents a specific sequence pattern (A B B A) that is classified as an "Enclosed rhyme".

While implemented using a recursive tree structure, the primary semantic is that elements form the pattern sequence itself, not that they are children of a node. The tree structure is an implementation detail that supports the sequence representation.

## Graph Elements

Pattern variants are structural classifications based on their element structure that can be interpreted through different graph views. The following classification functions are planned but not yet implemented:

```haskell
-- Simple graph element classification (planned)
isNode :: Pattern v -> Bool
isNode p = all (not . isGraphElement) (elements p)

isRelationship :: Pattern v -> Bool
isRelationship p = length (elements p) == 2 && all isNode (elements p)

isSubgraph :: Pattern v -> Bool
isSubgraph p = all isGraphElement (elements p)

isPath :: Pattern v -> Bool
isPath p = isSubgraph p && chainsCorrectly (elements p)
  where
    chainsCorrectly [] = True
    chainsCorrectly [_] = True
    chainsCorrectly (r1:r2:rs) = 
      isRelationship r1 && isRelationship r2 &&
      target r1 == source r2 && chainsCorrectly (r2:rs)
```

**Status**: ⏳ Planned (classification functions not yet implemented)

## Category Theoretic Perspective

### Patterns as a Free Structure

The Pattern type forms a category **Pat** where:
- Objects: Individual patterns
- Morphisms: Structural transformations preserving pattern shape

### Graph Views as Functors

Each graph view defines a functor F: **Pat** → **Graph_k** for some specific graph category:

```haskell
class GraphView view where
  type Direction view :: *
  
  -- Interpret pattern structure
  interpretNode :: view -> Pattern v -> Bool
  interpretRel  :: view -> Pattern v -> Bool
  
  -- Extract graph semantics  
  direction :: view -> Pattern v -> Direction view
  canChain  :: view -> Pattern v -> Pattern v -> Bool
  
  -- The functor mapping
  toGraph :: view -> Pattern v -> Graph (Direction view) v
```

## Standard Views (Planned)

Graph views provide different semantic interpretations of pattern structures. Views are planned but not yet implemented.

### Directed Graph View (Planned)

```haskell
data DirectedView = DirectedView

instance GraphView DirectedView where
  type Direction DirectedView = Ordered
  
  direction _ r = Directed (elements r !! 0) (elements r !! 1)
  canChain _ r1 r2 = target r1 == source r2
```

**Status**: ⏳ Planned (not yet implemented)

### Undirected Graph View (Planned)

```haskell
data UndirectedView = UndirectedView

instance GraphView UndirectedView where
  type Direction UndirectedView = Unordered
  
  direction _ r = Undirected (Set.fromList $ elements r)
  canChain _ r1 r2 = not $ Set.null $ 
    Set.intersection (nodes r1) (nodes r2)
```

**Status**: ⏳ Planned (not yet implemented)

## Forgetful Pattern Matching

Views form a hierarchy of forgetful functors:

```
Pat[Full] --F₁--> Pat[Shape] --F₂--> Pat[Topology] --F₃--> Pat[Connected]
```

Where each functor forgets information:

```haskell
-- Forget decorations, preserve structure
forgetValues :: Pattern v -> Pattern ()
forgetValues = fmap (const ())

-- Forget direction, preserve connectivity  
forgetDirection :: GraphView view => view -> Graph Ordered v -> Graph Unordered v

-- Forget specific edges, preserve paths
forgetPaths :: Graph dir v -> ConnectivityClass
```

## Composition and Navigation

### Zipper for Focus (future work)

```haskell
data Zipper v = Zipper
  { focus   :: Pattern v
  , context :: Context v
  }

data Context v = Context
  { parent  :: v
  , left    :: [Pattern v]
  , right   :: [Pattern v]  
  , above   :: Maybe (Context v)
  }
```

**Status**: ⏳ Planned (future work)

### Pattern Morphisms (Planned)

Morphisms between patterns respect structure while potentially forgetting decorations:

```haskell
type PatternMorphism v w = Pattern v -> Pattern w

-- Structure-preserving map
homomorphism :: (v -> w) -> PatternMorphism v w
homomorphism f = fmap f

-- Forgetful morphism  
forget :: PatternMorphism v ()
forget = forgetValues
```

**Status**: ⏳ Planned (not yet implemented)

## Key Properties

1. **Schema-lazy**: Patterns don't commit to specific graph semantics; interpretation happens in the view
2. **Compositional**: Views can be composed, stacked, or swapped without changing underlying patterns  
3. **Open-ended**: New views can be defined for any graph-like interpretation
4. **Categorical**: Each view defines a functor; forgetful pattern matching uses functor composition

## Example: Multiple Interpretations

```haskell
-- Same pattern, different views
let p = Pattern "graph" 
          [ Pattern "A" []
          , Pattern "rel" [Pattern "A" [], Pattern "B" []]
          , Pattern "B" []
          ]

-- As directed graph: A → B
directedGraph = toGraph DirectedView p

-- As undirected graph: A — B  
undirectedGraph = toGraph UndirectedView p

-- Custom view using decorations
data WeightedView = WeightedView

instance GraphView WeightedView where
  -- Use value v to determine direction
  direction view rel = 
    if weight (value rel) > 0.5 
    then Directed (source rel) (target rel)
    else Undirected (Set.fromList [source rel, target rel])
```

## Analogical Reasoning via Forgetful Matching

```haskell
-- Match patterns that are "similar" under forgetting
analogicalMatch :: (GraphView v1, GraphView v2) 
                => v1 -> v2 
                -> Pattern a -> Pattern b 
                -> Bool
analogicalMatch view1 view2 p1 p2 =
  toGraph view1 (forget p1) `isIsomorphic` toGraph view2 (forget p2)
```

This framework provides a principled way to handle graph-like structures while maintaining flexibility in interpretation, enabling both strict pattern matching and analogical reasoning through categorical abstractions.

