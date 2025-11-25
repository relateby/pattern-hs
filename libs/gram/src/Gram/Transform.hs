module Gram.Transform
  ( transformGram
  ) where

import qualified Gram.CST as CST
import qualified Pattern.Core as P
import qualified Subject.Core as S
import qualified Subject.Value as V
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Transform a CST Gram into a Core Pattern Subject
transformGram :: CST.Gram -> P.Pattern S.Subject
transformGram (CST.Gram record patterns) =
  case (record, patterns) of
    (Just props, []) -> 
      -- Only record
      P.Pattern (S.Subject (S.Symbol "") Set.empty props) []
    (Just props, pats) ->
      -- Record + Patterns: Record becomes root properties, patterns become elements
      P.Pattern (S.Subject (S.Symbol "") Set.empty props) (map transformPattern pats)
    (Nothing, [p]) ->
      -- Single pattern (common case)
      transformPattern p
    (Nothing, pats) ->
      -- Multiple patterns without root record: wrap in implicit root
      P.Pattern (S.Subject (S.Symbol "") Set.empty Map.empty) (map transformPattern pats)

transformPattern :: CST.Pattern -> P.Pattern S.Subject
transformPattern (CST.Pattern elements) =
  case elements of
    [el] -> transformElement el
    (first:rest) -> 
      -- Multiple elements: First acts as container/root of the sequence (legacy behavior preserved)
      -- OR we should wrap them? The existing parser treated comma-separated lists by nesting:
      -- "a, b" -> Pattern a [Pattern b]
      -- Let's preserve this for now to pass tests.
      let root = transformElement first
          others = map transformElement rest
      in P.Pattern (P.value root) (P.elements root ++ others)
    [] -> P.Pattern (S.Subject (S.Symbol "") Set.empty Map.empty) []

transformElement :: CST.PatternElement -> P.Pattern S.Subject
transformElement (CST.PEPath path) = transformPath path
transformElement (CST.PESubject subj) = transformSubject subj

transformPath :: CST.Path -> P.Pattern S.Subject
transformPath (CST.Path start segments) = transformPathRecursive start segments

transformPathRecursive :: CST.Node -> [CST.PathSegment] -> P.Pattern S.Subject
transformPathRecursive node [] = transformNode node
transformPathRecursive leftNode (seg:rest) =
  let rightPattern = transformPathRecursive (CST.segmentNode seg) rest
      relPattern = transformRelationship (CST.segmentRel seg)
      leftPattern = transformNode leftNode
  in P.Pattern (P.value relPattern) [leftPattern, rightPattern]

-- Override transformPath to use recursive version
-- transformPath (CST.Path start segments) = transformPathRecursive start segments

transformNode :: CST.Node -> P.Pattern S.Subject
transformNode (CST.Node attrs) =
  let subj = maybe emptySubject transformAttributes attrs
  in P.Pattern subj []

transformSubject :: CST.Subject -> P.Pattern S.Subject
transformSubject (CST.Subject attrs nested) =
  let subj = maybe emptySubject transformAttributes attrs
      nestedPats = map transformElement nested
  in P.Pattern subj nestedPats

transformRelationship :: CST.Relationship -> P.Pattern S.Subject
transformRelationship (CST.Relationship _ attrs) =
  -- Arrow string is currently ignored in Pattern Subject (as per design)
  let subj = maybe emptySubject transformAttributes attrs
  in P.Pattern subj []

transformAttributes :: CST.Attributes -> S.Subject
transformAttributes (CST.Attributes ident labels props) =
  S.Subject
    (transformIdentifier ident)
    labels
    props

transformIdentifier :: Maybe CST.Identifier -> S.Symbol
transformIdentifier Nothing = S.Symbol ""
transformIdentifier (Just (CST.IdentSymbol (CST.Symbol s))) = S.Symbol s
transformIdentifier (Just (CST.IdentString s)) = S.Symbol s
transformIdentifier (Just (CST.IdentInteger i)) = S.Symbol (show i)

emptySubject :: S.Subject
emptySubject = S.Subject (S.Symbol "") Set.empty Map.empty
