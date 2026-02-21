-- | Main module for the Pattern library.
--
-- This module provides convenient access to all Pattern library functionality
-- by re-exporting public APIs from core modules. Import this module to access
-- all Pattern types, functions, and typeclass instances without needing to
-- import individual modules.
--
-- == Library Organization
--
-- The Pattern library is organized into several modules:
--
-- * @Pattern.Core@ - Core Pattern data type, construction functions, query functions,
--   predicate functions, and typeclass instances (Functor, Applicative, Comonad, etc.)
-- * @Pattern.Graph@ - Low-level graph structural operations (GraphLens, nodes, relationships,
--   incidentRels, etc.) and 'GraphView' constructor 'toGraphView'.
-- * @Pattern.Graph.GraphQuery@ - Portable graph query interface ('GraphQuery', 'TraversalWeight',
--   'fromGraphLens', combinators)
-- * @Pattern.Graph.Types@ - 'GraphView' and 'Substitution' types (re-exported via @Pattern.Graph@)
-- * @Pattern.Graph.Algorithms@ - Graph algorithms operating on 'GraphQuery' (bfs, dfs,
--   shortestPath, connectedComponents, etc.)
-- * @Pattern.Graph.Transform@ - Bulk graph transformations: 'unfoldGraph', 'mapGraph',
--   'mapAllGraph', 'filterGraph', 'foldGraph', 'mapWithContext', 'paraGraph', 'paraGraphFixed'
-- * @Pattern.PatternGraph@ - Typed graph container with O(log n) lookups; 'fromPatternGraph',
--   'materialize', 'toGraphView'
-- * @Pattern.Reconcile@ - Pattern reconciliation for normalizing duplicate identities
--
-- == Usage
--
-- Import the main Pattern module to access all functionality:
--
-- >>> import Pattern
-- >>> let p = point "test"
-- >>> value p
-- "test"
--
-- For graph transformation pipelines:
--
-- > import Pattern
-- > import Pattern.Graph.Transform (mapAllGraph, filterGraph, mapWithContext)
-- > import Pattern.PatternGraph (fromPatternGraph, materialize)
--
-- For graph algorithms, import the algorithm modules directly:
--
-- > import Pattern.Graph.GraphQuery (directed)
-- > import qualified Pattern.Graph.Algorithms as Alg
--
-- All public functions, types, and typeclass instances from Pattern.Core are
-- available through this module. See individual module documentation for
-- detailed information about specific functionality.
--
-- == Re-export Structure
--
-- This module re-exports:
--
-- * All public exports from @Pattern.Core@ (Pattern type, construction functions,
--   query functions, predicate functions, helper functions, and all typeclass instances)
-- * All public exports from @Pattern.Graph@ (graph operations, 'GraphView', 'Substitution')
-- * All public exports from @Pattern.Graph.GraphQuery@ (portable graph query interface)
-- * All public exports from @Pattern.Graph.Transform@ (bulk transformations and iterative algorithms)
-- * Selected exports from @Pattern.PatternGraph@ (PatternGraph, mergeWithPolicy, fromPatterns,
--   fromPatternsWithPolicy, empty, fromPatternGraph, materialize). For a @GraphView@ from a
--   @PatternGraph@ use @Pattern.PatternGraph.toGraphView@; 'toGraphView' for 'GraphLens' is
--   re-exported from @Pattern.Graph@.
-- * All public exports from @Pattern.Reconcile@ (reconciliation operations)
--
-- Internal implementation details and helper functions are not exported through
-- this module, ensuring a clean public API.
module Pattern
  ( -- * Core Pattern Type and Operations
    module Pattern.Core
    -- * Graph Operations and GraphView
  , module Pattern.Graph
    -- * Portable Graph Query Interface
  , module Pattern.Graph.GraphQuery
    -- * Graph Transformations
  , module Pattern.Graph.Transform
    -- * Typed Graph Container
  , PatternGraph(..)
  , mergeWithPolicy
  , fromPatterns
  , fromPatternsWithPolicy
  , empty
  , fromPatternGraph
  , materialize
    -- * Reconciliation Operations
  , module Pattern.Reconcile
  ) where

import Pattern.Core
import Pattern.Graph
import Pattern.Graph.GraphQuery
import Pattern.Graph.Transform
import Pattern.PatternGraph
  ( PatternGraph(..)
  , mergeWithPolicy
  , fromPatterns
  , fromPatternsWithPolicy
  , empty
  , fromPatternGraph
  , materialize
  )
import Pattern.Reconcile
