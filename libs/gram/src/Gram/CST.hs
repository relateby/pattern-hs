{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Gram.CST
  ( GramDoc(..)
  , AnnotatedPattern(..)
  , PatternElement(..)
  , Path(..)
  , PathSegment(..)
  , Node(..)
  , Relationship(..)
  , SubjectPattern(..)
  , SubjectData(..)
  , Annotation(..)
  , Identifier(..)
  , Symbol(..)
  , Value
  , RangeValue
  ) where

import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Set (Set)
import qualified Subject.Value as CoreVal

-- | Top-level Gram structure (a Document)
-- gram: optional(record) + repeat(annotated_pattern)
data GramDoc = GramDoc
  { gramRecord :: Maybe (Map String Value)
  , gramPatterns :: [AnnotatedPattern]
  } deriving (Show, Eq, Generic)

-- | A top-level annotated pattern
-- annotated_pattern: optional(annotations) + single(pattern_element)
-- Note: In tree-sitter-gram 0.2.7, annotated_pattern contains exactly ONE element.
-- Comma-separated sequences only appear inside subject_pattern elements (after |).
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

-- | Metadata annotation: either property-style or identified/labeled.
--
-- * 'PropertyAnnotation': From @key(value). Key is a symbol, value is any gram value.
--
-- * 'IdentifiedAnnotation': From @@ with optional identifier and/or labels (e.g. @@p (a),
--   @@:L (a), @@p:L (a)). At most one per annotated pattern; must appear first if present.
--   Empty @@ is invalid (parser rejects it).
--
-- See specs/032-gram-annotation-syntax/contracts/annotations.md and data-model.md.
data Annotation
  = PropertyAnnotation
      { paKey :: Symbol
      , paValue :: Value
      }
  | IdentifiedAnnotation
      { iaIdentifier :: Maybe Identifier
      , iaLabels :: Set String
      }
  deriving (Show, Eq, Generic)

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
-- NOTE: These type aliases are kept for potential future use.
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
type Value = CoreVal.Value
type RangeValue = CoreVal.RangeValue
