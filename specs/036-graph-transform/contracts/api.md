# Contracts: Graph Transform API

This defines the Haskell module signatures and contract obligations.

## `Pattern.Core`

```haskell
-- | Dual of para. Expands a seed recursively into a pattern tree.
unfold :: (a -> (v, [a])) -> a -> Pattern v
```

## `Pattern.Graph`

```haskell
data GraphView extra v = GraphView
  { viewQuery    :: GraphQuery v
  , viewElements :: [(GraphClass extra, Pattern v)]
  }

-- | Constructs concrete PatternGraph from lazily evaluated View mappings.
materialize :: GraphClassifier extra v
            -> ReconciliationPolicy (MergeStrategy v)
            -> GraphView extra v
            -> PatternGraph v
```

## `Pattern.Graph.Transform`

```haskell
-- | Turns seed lists into populated Graph Views based on recursive patterns.
unfoldGraph :: GraphClassifier extra v
            -> ReconciliationPolicy (MergeStrategy v)
            -> (a -> [Pattern v])
            -> [a]
            -> PatternGraph v

-- | Maps over view classifications discretely
mapGraph :: GraphClassifier extra v
         -> (Pattern v -> Pattern v)  -- nodes
         -> (Pattern v -> Pattern v)  -- relationships
         -> (Pattern v -> Pattern v)  -- walks
         -> (Pattern v -> Pattern v)  -- annotations
         -> (Pattern v -> Pattern v)  -- other
         -> GraphView extra v -> GraphView extra v

-- | Applies single map uniformly
mapAllGraph :: (Pattern v -> Pattern v) -> GraphView extra v -> GraphView extra v

-- | Resolves container states post-filter via Substitution parameter
filterGraph :: GraphClassifier extra v
            -> (GraphClass extra -> Pattern v -> Bool)
            -> Substitution v
            -> GraphView extra v -> GraphView extra v

-- | Reduces entire view space into a categorized aggregation
foldGraph :: Monoid m
          => (GraphClass extra -> Pattern v -> m)
          -> GraphView extra v -> m

-- | Provides full snapshot context to mapper (for enrichment)
mapWithContext :: GraphClassifier extra v
               -> (GraphQuery v -> Pattern v -> Pattern v)
               -> GraphView extra v -> GraphView extra v

-- | Iterative topological folding algorithm
paraGraph :: (GraphQuery v -> Pattern v -> [r] -> r)
          -> GraphView extra v
          -> Map (Id v) r

-- | Fixpoint iteration for cyclic topology convergences
paraGraphFixed :: (r -> r -> Bool)         -- convergence predicate: True when stable
               -> (GraphQuery v -> Pattern v -> [r] -> r)
               -> r                         -- initial value for all elements
               -> GraphView extra v
               -> Map (Id v) r
```