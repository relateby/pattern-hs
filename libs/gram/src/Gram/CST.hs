{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Gram.CST
  ( Gram(..)
  , AnnotatedPattern(..)
  , PatternElement(..)
  , Path(..)
  , PathSegment(..)
  , Node(..)
  , Relationship(..)
  , SubjectPattern(..)
  , SubjectData(..)
  , Annotation(..)
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
-- gram: optional(record) + repeat(annotated_pattern)
data Gram = Gram
  { gramRecord :: Maybe (Map String Value)
  , gramPatterns :: [AnnotatedPattern]
  } deriving (Show, Eq, Generic)

-- | A top-level annotated pattern
-- annotated_pattern: optional(annotations) + commaSep1(pattern_element)
data AnnotatedPattern = AnnotatedPattern
  { apAnnotations :: [Annotation]
  , apElements :: [PatternElement]
  } deriving (Show, Eq, Generic)

-- | Element of a pattern
-- pattern_element: subject_pattern | path_pattern | pattern_reference
data PatternElement
  = PEPath Path
  | PESubjectPattern SubjectPattern
  | PEReference Identifier
  deriving (Show, Eq, Generic)

-- | A path structure (node connected by relationships)
-- path_pattern: node_pattern | relationship_pattern
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
-- node_pattern: (subject?)
data Node = Node
  { nodeSubject :: Maybe SubjectData
  } deriving (Show, Eq, Generic)

-- | A relationship structure
-- relationship_pattern
-- In CST, we capture the arrow and the optional subject
data Relationship = Relationship
  { relArrow :: String
  , relSubject :: Maybe SubjectData
  } deriving (Show, Eq, Generic)

-- | A subject pattern structure (bracket notation)
-- subject_pattern: [subject | elements]
data SubjectPattern = SubjectPattern
  { spSubject :: Maybe SubjectData
  , spElements :: [PatternElement]
  } deriving (Show, Eq, Generic)

-- | Subject data container (Identifier, Labels, Record)
-- Maps to 'subject' in grammar
data SubjectData = SubjectData
  { dataIdentifier :: Maybe Identifier
  , dataLabels :: Set String
  , dataProperties :: Map String Value
  } deriving (Show, Eq, Generic)

-- | Metadata annotation
-- annotation: @key(value)
data Annotation = Annotation
  { annKey :: Symbol
  , annValue :: Value
  } deriving (Show, Eq, Generic)

-- | Identifiers
newtype Symbol = Symbol String
  deriving (Show, Eq, Ord, Generic)

data Identifier
  = IdentSymbol Symbol
  | IdentString String
  | IdentInteger Integer
  deriving (Show, Eq, Ord, Generic)

-- | Values (mirroring Subject.Value but local to CST if needed, 
-- or we can reuse Core types if they are purely data)
type Value = CoreVal.Value
type RangeValue = CoreVal.RangeValue
