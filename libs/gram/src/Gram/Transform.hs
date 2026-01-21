{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- | Transformation of CST (Concrete Syntax Tree) to Pattern Subject.
--
-- This module provides functions to transform parsed gram notation (CST)
-- into the core Pattern Subject representation. The default behavior
-- preserves anonymous subjects as 'Symbol ""' to enable round-trip
-- compatibility, while optional functions are available for explicit
-- ID assignment when needed.
--
-- == Design Decision: Anonymity Preservation by Default
--
-- The default 'transformGram' function preserves anonymous subjects
-- (represented as 'Symbol ""') rather than assigning generated IDs.
-- This enables true round-trip compatibility: parsing and serializing
-- anonymous patterns preserves their anonymous nature.
--
-- == When to Use ID Assignment
--
-- Use 'transformGramWithIds' or 'assignIdentities' when:
--
-- * You need unique identifiers for graph algorithms
-- * You need to distinguish between anonymous instances
-- * You're working with external systems that require explicit IDs
--
-- Use 'transformGram' (default) when:
--
-- * You need round-trip compatibility
-- * You want to preserve the original gram notation structure
-- * Anonymous subjects should remain anonymous
--
-- == Examples
--
-- >>> transformGram (parseGram "()")
-- Pattern with Subject {identity = Symbol "", ...}
--
-- >>> transformGramWithIds (parseGram "() ()")
-- Pattern with two subjects having identities Symbol "#1" and Symbol "#2"
module Gram.Transform
  ( transformGram
  , transformGramWithIds
  , transformGramList
  , transformGramWithHeader
  , transformPattern
  , assignIdentities
  ) where

import qualified Gram.CST as CST
import qualified Pattern.Core as P
import qualified Subject.Core as S
import qualified Subject.Value as V
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (State, evalState, get, put)
import Data.Char (isDigit)

type Transform = State Int

-- | Transform a CST GramDoc into a list of Core Pattern Subject.
--
-- This function preserves anonymous subjects as 'Symbol ""' to enable
-- round-trip compatibility. Anonymous subjects in the gram notation
-- (e.g., @()@, @()-[]->()@) will be represented with empty identity.
--
-- If you need unique IDs assigned to anonymous subjects, use
-- 'transformGramWithIds' instead.
transformGram :: CST.GramDoc -> [P.Pattern S.Subject]
transformGram = transformGramList

-- | Transform a CST GramDoc into a list of Core Pattern Subject.
-- A leading bare record (header) is represented as an anonymous pattern with
-- no elements (@Pattern (Subject (Symbol \"\") Set.empty props) []@) and
-- prepended as the first element. Use 'transformGramWithHeader' to obtain
-- the header as 'Maybe PropertyRecord' separately from the pattern list.
transformGramList :: CST.GramDoc -> [P.Pattern S.Subject]
transformGramList (CST.GramDoc record patterns) =
  let pats = evalState (mapM transformPattern patterns) 0
  in case record of
    Just props -> P.Pattern (S.Subject (S.Symbol "") Set.empty props) [] : pats
    Nothing -> pats

-- | Transform a CST GramDoc into a header and list of Core Pattern Subject.
transformGramWithHeader :: CST.GramDoc -> (Maybe (Map String V.Value), [P.Pattern S.Subject])
transformGramWithHeader (CST.GramDoc record patterns) =
  (record, evalState (mapM transformPattern patterns) 0)

-- | Find the maximum numeric suffix of IDs matching "#<N>" in the CST
findMaxId :: CST.GramDoc -> Int
findMaxId (CST.GramDoc _ patterns) = maximum (0 : concatMap scanPattern patterns)
  where
    scanPattern (CST.AnnotatedPattern _ elements) = concatMap scanElement elements

    scanElement (CST.PEPath path) = scanPath path
    scanElement (CST.PESubjectPattern sp) = scanSubjectPattern sp
    scanElement (CST.PEReference ident) = scanIdentifier (Just ident)

    scanPath (CST.Path startNode segments) = 
      scanNode startNode ++ concatMap scanSegment segments

    scanSegment (CST.PathSegment rel nextNode) = 
      scanRelationship rel ++ scanNode nextNode

    scanNode (CST.Node subjData) = scanSubjectData subjData

    scanRelationship (CST.Relationship _ subjData) = scanSubjectData subjData

    scanSubjectPattern (CST.SubjectPattern subjData nested) = 
      scanSubjectData subjData ++ concatMap scanElement nested

    scanSubjectData Nothing = []
    scanSubjectData (Just (CST.SubjectData ident _ _)) = scanIdentifier ident

    scanIdentifier (Just (CST.IdentSymbol (CST.Symbol s))) = case parseGeneratedId s of
      Just n -> [n]
      Nothing -> []
    scanIdentifier _ = []

    parseGeneratedId ('#':rest) | all isDigit rest && not (null rest) = Just (read rest)
    parseGeneratedId _ = Nothing

transformPattern :: CST.AnnotatedPattern -> Transform (P.Pattern S.Subject)
transformPattern (CST.AnnotatedPattern annotations elements) = do
  -- Transform elements (AnnotatedPattern contains exactly ONE element in 0.2.7)
  transformedElements <- mapM transformElement elements
  
  case annotations of
    [] -> case transformedElements of
      [el] -> return el
      (first:rest) -> return $ P.Pattern (P.value first) (P.elements first ++ rest)
      [] -> return $ P.Pattern (S.Subject (S.Symbol "") Set.empty Map.empty) []
      
    anns -> do
      -- Convert annotations to properties
      let annProps = annotationsToProperties anns
      
      -- Annotations become properties of a wrapper subject pattern.
      -- The single element from annotated_pattern becomes the content.
      
      -- Preserve anonymity for annotation wrapper subjects
      -- Note: In the semantic mapping, the annotations become properties of the "subject"
      -- that wraps the content.
      
      let wrapperSubject = S.Subject (S.Symbol "") Set.empty annProps
      
      return $ P.Pattern wrapperSubject transformedElements

annotationsToProperties :: [CST.Annotation] -> Map String V.Value
annotationsToProperties = Map.fromList . map (\(CST.Annotation (CST.Symbol k) v) -> (k, v))


transformElement :: CST.PatternElement -> Transform (P.Pattern S.Subject)
transformElement (CST.PEPath path) = transformPath path
transformElement (CST.PESubjectPattern b) = transformSubjectPattern b
transformElement (CST.PEReference ident) = do
  sym <- transformIdentifier (Just ident)
  return $ P.Pattern (S.Subject sym Set.empty Map.empty) []

-- | Transform a path into a Pattern.
-- 
-- 1. Single Node: (a) -> Pattern a []
-- 2. Single Edge: (a)-[r]->(b) -> Pattern r [a, b]
-- 3. Walk: (a)-[r1]->(b)-[r2]->(c) -> Pattern walk [Pattern r1 [a, b], Pattern r2 [b, c]]
transformPath :: CST.Path -> Transform (P.Pattern S.Subject)
transformPath (CST.Path startNode segments) =
  case segments of
    [] -> transformNode startNode
    [seg] -> do
      -- Single Edge case: Return the edge pattern directly
      -- (a)-[r]->(b) becomes [r | a, b]
      left <- transformNode startNode
      right <- transformNode (CST.segmentNode seg)
      rel <- transformRelationship (CST.segmentRel seg)
      return $ P.Pattern (P.value rel) [left, right]
    _ -> do
      -- Walk case (multiple segments): Return a Walk Pattern containing edges
      -- (a)-[r1]->(b)-[r2]->(c) becomes [walk | [r1 | a, b], [r2 | b, c]]
      leftP <- transformNode startNode
      edges <- constructWalkEdges leftP segments
      -- Use a specific label for Walk container to distinguish it
      let walkSubject = S.Subject (S.Symbol "") (Set.singleton "Gram.Walk") Map.empty
      return $ P.Pattern walkSubject edges

-- | Construct a list of Edge Patterns from a start node and path segments.
-- We pass the transformed left pattern to ensure identity continuity in the walk.
constructWalkEdges :: P.Pattern S.Subject -> [CST.PathSegment] -> Transform [P.Pattern S.Subject]
constructWalkEdges _ [] = return []
constructWalkEdges leftP (seg:rest) = do
  let rightNode = CST.segmentNode seg
  rightP <- transformNode rightNode
  relP <- transformRelationship (CST.segmentRel seg)
  -- Create self-contained edge: [rel | left, right]
  let edge = P.Pattern (P.value relP) [leftP, rightP]
  restEdges <- constructWalkEdges rightP rest
  return (edge : restEdges)

transformNode :: CST.Node -> Transform (P.Pattern S.Subject)
transformNode (CST.Node subjData) = do
  subj <- maybe transformEmptySubject transformSubjectData subjData
  return $ P.Pattern subj []

transformSubjectPattern :: CST.SubjectPattern -> Transform (P.Pattern S.Subject)
transformSubjectPattern (CST.SubjectPattern subjData nested) = do
  subj <- maybe transformEmptySubject transformSubjectData subjData
  nestedPats <- mapM transformElement nested
  return $ P.Pattern subj nestedPats

transformRelationship :: CST.Relationship -> Transform (P.Pattern S.Subject)
transformRelationship (CST.Relationship _ subjData) = do
  -- Arrow string is currently ignored in Pattern Subject (as per design)
  subj <- maybe transformEmptySubject transformSubjectData subjData
  return $ P.Pattern subj []

transformSubjectData :: CST.SubjectData -> Transform S.Subject
transformSubjectData (CST.SubjectData ident labels props) = do
  sym <- transformIdentifier ident
  return $ S.Subject
    sym
    labels
    props

transformIdentifier :: Maybe CST.Identifier -> Transform S.Symbol
transformIdentifier Nothing = return (S.Symbol "")  -- Preserve anonymity
transformIdentifier (Just (CST.IdentSymbol (CST.Symbol s))) = return $ S.Symbol s
transformIdentifier (Just (CST.IdentString s)) = return $ S.Symbol s
transformIdentifier (Just (CST.IdentInteger i)) = return $ S.Symbol (show i)

transformEmptySubject :: Transform S.Subject
transformEmptySubject = return $ S.Subject (S.Symbol "") Set.empty Map.empty

generateId :: Transform S.Symbol
generateId = do
  i <- get
  put (i + 1)
  return $ S.Symbol ("#" ++ show i)

-- | Find the maximum numeric suffix of IDs matching "#<N>" in a Pattern.
--
-- Used by 'assignIdentities' to determine the starting counter value
-- to avoid collisions with existing generated IDs.
findMaxIdInPattern :: P.Pattern S.Subject -> Int
findMaxIdInPattern (P.Pattern subj elems) =
  maximum (0 : scanSubject subj : map findMaxIdInPattern elems)
  where
    scanSubject (S.Subject (S.Symbol s) _ _) = case parseGeneratedId s of
      Just n -> n
      Nothing -> 0
    parseGeneratedId ('#':rest) | all isDigit rest && not (null rest) = Just (read rest)
    parseGeneratedId _ = Nothing

-- | Assign unique sequential IDs to anonymous subjects in a Pattern.
--
-- This function recursively traverses a pattern and assigns IDs of the form
-- @#N@ to all subjects with empty identity ('Symbol ""'). The counter starts
-- from the maximum existing @#N@-style ID found in the pattern plus one,
-- ensuring no collisions.
--
-- Named subjects (non-empty identity) are left unchanged.
assignIdentities :: P.Pattern S.Subject -> P.Pattern S.Subject
assignIdentities pattern = evalState (assignIdentities' pattern) (findMaxIdInPattern pattern + 1)

assignIdentities' :: P.Pattern S.Subject -> Transform (P.Pattern S.Subject)
assignIdentities' (P.Pattern subj elems) = do
  newSubj <- case subj of
    S.Subject (S.Symbol "") lbls props -> do
      i <- get
      put (i + 1)
      return $ S.Subject (S.Symbol ("#" ++ show i)) lbls props
    _ -> return subj
  newElems <- mapM assignIdentities' elems
  return $ P.Pattern newSubj newElems

-- | Transform a CST GramDoc into a list of Core Pattern Subject with ID assignment.
--
-- This function is equivalent to applying 'assignIdentities' to each pattern
-- in the result of 'transformGram', threading the ID counter across the list
-- so IDs are unique across the whole document. It assigns unique sequential IDs
-- (e.g., @#1@, @#2@) to all anonymous subjects in the parsed patterns.
--
-- Use this function when you need unique identifiers for anonymous subjects,
-- such as for graph algorithms or when distinguishing between anonymous
-- instances is important.
--
-- For round-trip compatibility, use 'transformGram' instead, which preserves
-- anonymity.
transformGramWithIds :: CST.GramDoc -> [P.Pattern S.Subject]
transformGramWithIds gram =
  let patterns = transformGram gram
      start = maximum (0 : map findMaxIdInPattern patterns) + 1
  in evalState (mapM assignIdentities' patterns) start
