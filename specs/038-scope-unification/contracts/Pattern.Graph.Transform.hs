module Pattern.Graph.Transform
  ( scopeDictFromGraphView
  , paraGraph
  , paraGraphFixed
  ) where

import Data.Map.Strict (Map)
import Pattern.Core (Pattern(..), ScopeQuery(..), ScopeDict)
import Pattern.Graph.GraphClassifier (GraphValue(..))
import Pattern.Graph.GraphQuery (GraphQuery(..))
import Pattern.Graph.Types (GraphView(..))

-- Internal adapter that exposes full GraphView scope to the generic scope layer
-- without changing the public GraphQuery record shape.
data GraphViewScope v = GraphViewScope
  { gvsQuery      :: GraphQuery v
  , gvsElements   :: [Pattern v]
  , gvsIndex      :: Map (Id v) (Pattern v)
  , gvsContainers :: Map (Id v) [Pattern v]
  }

instance ScopeQuery GraphViewScope v where
  type ScopeId GraphViewScope v = Id v

scopeDictFromGraphView :: GraphValue v => GraphView extra v -> ScopeDict (Id v) v

graphViewScope :: GraphValue v => GraphView extra v -> GraphViewScope v

-- Existing public API remains unchanged.
paraGraph
  :: GraphValue v
  => (GraphQuery v -> Pattern v -> [r] -> r)
  -> GraphView extra v
  -> Map (Id v) r

paraGraphFixed
  :: (GraphValue v, Ord (Id v))
  => (r -> r -> Bool)
  -> (GraphQuery v -> Pattern v -> [r] -> r)
  -> r
  -> GraphView extra v
  -> Map (Id v) r
