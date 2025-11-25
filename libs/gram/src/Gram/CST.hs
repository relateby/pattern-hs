{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Gram.CST
  ( Gram(..)
  , Pattern(..)
  , PatternElement(..)
  , Path(..)
  , PathSegment(..)
  , Node(..)
  , Relationship(..)
  , Subject(..)
  , Attributes(..)
  , Value(..)
  , RangeValue(..)
  , Identifier(..)
  , Symbol(..)
  ) where

import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Set (Set)
import qualified Subject.Core as Core
import qualified Subject.Value as CoreVal

-- | Top-level Gram structure
-- gram: optional(record) + repeat(pattern)
data Gram = Gram
  { gramRecord :: Maybe (Map String Value)
  , gramPatterns :: [Pattern]
  } deriving (Show, Eq, Generic)

-- | A pattern sequence
-- pattern: optional(annotations) + commaSep1(pattern_element)
-- For now, annotations are not fully specified in the grammar reference provided,
-- so we omit them or could add them later.
data Pattern = Pattern
  { patternElements :: [PatternElement]
  } deriving (Show, Eq, Generic)

-- | Element of a pattern
-- pattern_element: subject | path
data PatternElement
  = PEPath Path
  | PESubject Subject
  deriving (Show, Eq, Generic)

-- | A path structure (node connected by relationships)
-- This represents the linearized path: (a)-[r1]->(b)<-[r2]-(c)
data Path = Path
  { pathStart :: Node
  , pathSegments :: [PathSegment]
  } deriving (Show, Eq, Generic)

-- | A segment of a path: a relationship and the next node
data PathSegment = PathSegment
  { segmentRel :: Relationship
  , segmentNode :: Node
  } deriving (Show, Eq, Generic)

-- | A node structure
-- node: (attributes?)
data Node = Node
  { nodeAttributes :: Maybe Attributes
  } deriving (Show, Eq, Generic)

-- | A relationship structure
-- relationship: node + relationship_kind + path (in recursive definition)
-- In CST, we capture the arrow and the optional attributes
data Relationship = Relationship
  { relArrow :: String        -- The raw arrow string, e.g., "-->", "<==>", "-[...]->"
  , relAttributes :: Maybe Attributes
  } deriving (Show, Eq, Generic)

-- | A subject structure
-- subject: [attributes? | sub_pattern?]
data Subject = Subject
  { subjectAttributes :: Maybe Attributes
  , subjectNested :: [PatternElement] -- Nested patterns after pipe
  } deriving (Show, Eq, Generic)

-- | Attributes container
data Attributes = Attributes
  { attrIdentifier :: Maybe Identifier
  , attrLabels :: Set String
  , attrProperties :: Map String Value
  } deriving (Show, Eq, Generic)

-- | Identifiers
newtype Symbol = Symbol String
  deriving (Show, Eq, Ord, Generic)

data Identifier
  = IdentSymbol Symbol
  | IdentString String
  | IdentInteger Integer
  deriving (Show, Eq, Generic)

-- | Values (mirroring Subject.Value but local to CST if needed, 
-- or we can reuse Core types if they are purely data)
-- We will reuse Core.Value for leaf values to avoid redundant definitions,
-- assuming Core.Value is sufficient for CST needs (it seems to be).
type Value = CoreVal.Value
type RangeValue = CoreVal.RangeValue
