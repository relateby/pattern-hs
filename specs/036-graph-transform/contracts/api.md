# Contracts: Graph Transform API

This defines the Haskell module signatures and contract obligations.

**Note**: Simplified signatures shown below omit some type constraints for readability.
Actual implementations include constraints like `GraphValue v`, `Eq v`, `Mergeable v`,
`HasIdentity v (Id v)`, and `Refinable v` where needed. See source files for complete signatures.

## `Pattern.Core`

```haskell
-- | Dual of para. Expands a seed recursively into a pattern tree.
unfold :: (a -> (v, [a])) -> a -> Pattern v
```

## `Pattern.Graph` (Types)

```haskell
-- | Universal graph-like interface pairing traversal with classified elements
data GraphView extra v = GraphView
  { viewQuery    :: GraphQuery v
  , viewElements :: [(GraphClass extra, Pattern v)]
  }
```

## `Pattern.PatternGraph` (Construction & Materialization)

```haskell
-- | Construct a GraphView from a PatternGraph
toGraphView :: (GraphValue v, Eq v)
            => GraphClassifier extra v
            -> PatternGraph extra v
            -> GraphView extra v

-- | Materialize a GraphView back into a concrete PatternGraph
materialize :: ( GraphValue v, Eq v
               , Mergeable v, HasIdentity v (Id v), Refinable v )
            => GraphClassifier extra v
            -> ReconciliationPolicy (MergeStrategy v)
            -> GraphView extra v
            -> PatternGraph extra v  -- Note: returns PatternGraph extra v, not PatternGraph v
```

## `Pattern.Graph` (GraphLens Integration)

```haskell
-- | Construct a GraphView from a GraphLens
toGraphView :: (GraphValue v, Eq v)
            => GraphClassifier extra v
            -> GraphLens v
            -> GraphView extra v
```

## `Pattern.Graph.Transform`

```haskell
-- | Build a PatternGraph by expanding seeds into patterns and batch-merging
unfoldGraph :: ( GraphValue v, Eq v
               , Mergeable v, HasIdentity v (Id v), Refinable v )
            => GraphClassifier extra v
            -> ReconciliationPolicy (MergeStrategy v)
            -> (a -> [Pattern v])
            -> [a]
            -> PatternGraph extra v  -- Note: returns PatternGraph extra v, not PatternGraph v

-- | Map over view classifications discretely (with INLINE pragma for performance)
mapGraph :: GraphClassifier extra v
         -> (Pattern v -> Pattern v)  -- nodes
         -> (Pattern v -> Pattern v)  -- relationships
         -> (Pattern v -> Pattern v)  -- walks
         -> (Pattern v -> Pattern v)  -- annotations
         -> (Pattern v -> Pattern v)  -- other
         -> GraphView extra v -> GraphView extra v

-- | Apply a single function uniformly to every element
mapAllGraph :: (Pattern v -> Pattern v) -> GraphView extra v -> GraphView extra v

-- | Filter elements with container gap repair via Substitution
filterGraph :: GraphClassifier extra v
            -> (GraphClass extra -> Pattern v -> Bool)
            -> Substitution v
            -> GraphView extra v -> GraphView extra v

-- | Reduce all view elements into a Monoid accumulation
foldGraph :: Monoid m
          => (GraphClass extra -> Pattern v -> m)
          -> GraphView extra v -> m

-- | Map with access to the original snapshot GraphQuery (for enrichment)
mapWithContext :: GraphClassifier extra v
               -> (GraphQuery v -> Pattern v -> Pattern v)
               -> GraphView extra v -> GraphView extra v

-- | Single-round structural fold in bottom-up containment order
paraGraph :: GraphValue v
          => (GraphQuery v -> Pattern v -> [r] -> r)
          -> GraphView extra v
          -> Map (Id v) r

-- | Iterate structural fold rounds until convergence
paraGraphFixed :: (GraphValue v, Ord (Id v))
               => (r -> r -> Bool)         -- convergence predicate: True when stable
               -> (GraphQuery v -> Pattern v -> [r] -> r)
               -> r                         -- initial value for all elements
               -> GraphView extra v
               -> Map (Id v) r
```