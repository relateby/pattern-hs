{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Spec.Pattern.RepresentationMapSpec where

import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Pattern (PatternKind(..), RepresentationMap(..), checkKind, compose)
import Pattern.Core (Pattern(..), ScopeDict(..), ScopeQuery(..), TrivialScope, paraWithScope, trivialScope)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldContain)
import Subject.Core (Subject(..), Symbol(..))
import qualified Subject.Core as Subj
import Subject.Value (Value(..))
import Test.QuickCheck (Property, property)
import qualified Test.QuickCheck as QC

newtype VisibleScope v = VisibleScope [Pattern v]

instance ScopeQuery VisibleScope v where
  type ScopeId VisibleScope v = Int

  containers _ _ = []
  siblings _ _ = []
  byIdentity (VisibleScope ps) i
    | i < 0 = Nothing
    | otherwise = case drop i ps of
        p:_ -> Just p
        [] -> Nothing
  allElements (VisibleScope ps) = ps

leafPatternKind :: PatternKind String
leafPatternKind = PatternKind
  { kindName = "Leaf"
  , kindPred = \_ p -> value p == "leaf" && null (elements p)
  , kindExample = Pattern "leaf" []
  }

branchPatternKind :: PatternKind String
branchPatternKind = PatternKind
  { kindName = "BranchWithLeaf"
  , kindPred = \_ p ->
      value p == "branch"
        && case elements p of
          [Pattern childValue childElements] ->
            childValue == "leaf" && null childElements
          _ -> False
  , kindExample = Pattern "branch" [Pattern "leaf" []]
  }

visiblePatternKind :: PatternKind String
visiblePatternKind = PatternKind
  { kindName = "VisibleLeaf"
  , kindPred = \q p -> value p == "target" && p `elem` allElements q
  , kindExample = Pattern "target" []
  }

wrappedPatternKind :: PatternKind String
wrappedPatternKind = PatternKind
  { kindName = "WrappedLeaf"
  , kindPred = \_ p ->
      case p of
        Pattern "wrapped" [child] -> checkKind leafPatternKind (trivialScope child) child
        _ -> False
  , kindExample = Pattern "wrapped" [kindExample leafPatternKind]
  }

boxedPatternKind :: PatternKind String
boxedPatternKind = PatternKind
  { kindName = "BoxedLeaf"
  , kindPred = \_ p ->
      case p of
        Pattern "boxed" [child] -> checkKind wrappedPatternKind (trivialScope child) child
        _ -> False
  , kindExample = Pattern "boxed" [kindExample wrappedPatternKind]
  }

aliasedWrappedPatternKind :: PatternKind String
aliasedWrappedPatternKind = PatternKind
  { kindName = "AliasedWrapped"
  , kindPred = kindPred wrappedPatternKind
  , kindExample = kindExample wrappedPatternKind
  }

wrapLeafMap :: RepresentationMap String
wrapLeafMap = RepresentationMap
  { name = "wrapLeaf"
  , domain = leafPatternKind
  , codomain = wrappedPatternKind
  , forward = \_ p -> Pattern "wrapped" [p]
  , inverse = \_ p ->
      case p of
        Pattern "wrapped" [child] -> child
        _ -> p
  , roundTrip = \q p -> (inverse wrapLeafMap q . forward wrapLeafMap q) p == p
  }

boxWrappedMap :: RepresentationMap String
boxWrappedMap = RepresentationMap
  { name = "boxWrapped"
  , domain = wrappedPatternKind
  , codomain = boxedPatternKind
  , forward = \_ p -> Pattern "boxed" [p]
  , inverse = \_ p ->
      case p of
        Pattern "boxed" [child] -> child
        _ -> p
  , roundTrip = \q p -> (inverse boxWrappedMap q . forward boxWrappedMap q) p == p
  }

mismatchedBoxMap :: RepresentationMap String
mismatchedBoxMap = RepresentationMap
  { name = "mismatchedBox"
  , domain = aliasedWrappedPatternKind
  , codomain = boxedPatternKind
  , forward = \_ p -> Pattern "boxed" [p]
  , inverse = \_ p ->
      case p of
        Pattern "boxed" [child] -> child
        _ -> p
  , roundTrip = \q p -> (inverse mismatchedBoxMap q . forward mismatchedBoxMap q) p == p
  }

subjectWith :: String -> [String] -> [(String, Value)] -> Subject
subjectWith sym lbls props =
  Subject (Symbol sym) (Set.fromList lbls) (Map.fromList props)

nodePattern :: String -> String -> [(String, Value)] -> Pattern Subject
nodePattern sym lbl props = Pattern (subjectWith sym [lbl] props) []

relationshipPattern :: String -> String -> Pattern Subject -> Pattern Subject -> Pattern Subject
relationshipPattern sym lbl src tgt =
  Pattern (subjectWith sym [lbl] []) [src, tgt]

graphRoot :: String -> [Pattern Subject] -> Pattern Subject
graphRoot sym es = Pattern (subjectWith sym ["DiagnosticGraph"] []) es

hasLabel :: String -> Subject -> Bool
hasLabel lbl subj = lbl `Set.member` Subj.labels subj

patternHasLabel :: String -> Pattern Subject -> Bool
patternHasLabel lbl = hasLabel lbl . value

integerProperty :: String -> Subject -> Maybe Integer
integerProperty key subj =
  case Map.lookup key (properties subj) of
    Just (VInteger n) -> Just n
    _ -> Nothing

stripDiagnosticMetadata :: Subject -> Subject
stripDiagnosticMetadata subj =
  subj { properties = Map.delete "_depth" (Map.delete "_arity" (properties subj)) }

topLevelGraphScope :: Pattern Subject -> ScopeDict Symbol Subject
topLevelGraphScope graph = ScopeDict
  { dictContainers = \pat -> filter (\container -> pat `elem` elements container) topLevel
  , dictSiblings = \pat ->
      concatMap
        (\container -> filter (/= pat) (elements container))
        (filter (\container -> pat `elem` elements container) topLevel)
  , dictByIdentity = \sym -> find ((== sym) . Subj.identity . value) topLevel
  , dictAllElements = topLevel
  }
  where
    topLevel = elements graph

validDiagnosticPattern :: Pattern Subject -> Bool
validDiagnosticPattern pat =
  case pat of
    Pattern locationSubject [diagnosticPat] ->
      hasLabel "Location" locationSubject
        && case diagnosticPat of
          Pattern diagnosticSubject remediationPats ->
            hasLabel "Diagnostic" diagnosticSubject
              && all isAtomicRemediation remediationPats
          _ -> False
    _ -> False
  where
    isAtomicRemediation (Pattern remediationSubject remediationElements) =
      hasLabel "Remediation" remediationSubject && null remediationElements

validGraphElement :: Pattern Subject -> Bool
validGraphElement pat
  | patternHasLabel "Location" pat = atomicEntity pat
  | patternHasLabel "Diagnostic" pat = atomicEntity pat
  | patternHasLabel "Remediation" pat = atomicEntity pat
  | patternHasLabel "AT" pat = binaryRelationship "Location" "Diagnostic" pat
  | patternHasLabel "HAS_REMEDIATION" pat = binaryRelationship "Diagnostic" "Remediation" pat
  | otherwise = False
  where
    atomicEntity (Pattern subj childPats) =
      null childPats
        && integerProperty "_arity" subj /= Nothing
        && integerProperty "_depth" subj /= Nothing
    binaryRelationship sourceLabel targetLabel (Pattern subj [src, tgt]) =
      null (properties subj)
        && patternHasLabel sourceLabel src
        && patternHasLabel targetLabel tgt
    binaryRelationship _ _ _ = False

connectedBy :: String -> Pattern Subject -> Pattern Subject -> Pattern Subject -> Bool
connectedBy relLabel src tgt relPat =
  patternHasLabel relLabel relPat
    && case elements relPat of
      [relSrc, relTgt] -> relSrc == src && relTgt == tgt
      _ -> False

diagnosticPatternKind :: PatternKind Subject
diagnosticPatternKind = PatternKind
  { kindName = "DiagnosticPattern"
  , kindPred = \_ -> validDiagnosticPattern
  , kindExample = canonicalDiagnosticPattern
  }

diagnosticGraphKind :: PatternKind Subject
diagnosticGraphKind = PatternKind
  { kindName = "DiagnosticGraph"
  , kindPred = \_ graph ->
      hasLabel "DiagnosticGraph" (value graph)
        && let scope = topLevelGraphScope graph
               graphElements = allElements scope
               locations = filter (patternHasLabel "Location") graphElements
               diagnostics = filter (patternHasLabel "Diagnostic") graphElements
               remediations = filter (patternHasLabel "Remediation") graphElements
               atRels = filter (patternHasLabel "AT") graphElements
               remediationRels = filter (patternHasLabel "HAS_REMEDIATION") graphElements
           in all validGraphElement graphElements
                && case (locations, diagnostics, atRels) of
                  ([loc], [diag], [atRel]) ->
                    connectedBy "AT" loc diag atRel
                      && all
                        (\remediation ->
                          any (connectedBy "HAS_REMEDIATION" diag remediation) remediationRels
                        )
                        remediations
                      && length remediationRels == length remediations
                  _ -> False
  , kindExample = canonicalDiagnosticGraph
  }

canonicalDiagnosticPattern :: Pattern Subject
canonicalDiagnosticPattern =
  Pattern (subjectWith "location-0" ["Location"] [])
    [ Pattern (subjectWith "diagnostic-0" ["Diagnostic"] [])
        [ Pattern (subjectWith "remediation-0" ["Remediation"] []) []
        ]
    ]

canonicalDiagnosticGraph :: Pattern Subject
canonicalDiagnosticGraph =
  let locationNode = nodePattern "location-0" "Location" [("_arity", VInteger 1), ("_depth", VInteger 0)]
      diagnosticNode = nodePattern "diagnostic-0" "Diagnostic" [("_arity", VInteger 1), ("_depth", VInteger 1)]
      remediationNode = nodePattern "remediation-0" "Remediation" [("_arity", VInteger 0), ("_depth", VInteger 2)]
      atRel = relationshipPattern "at-location-diagnostic" "AT" locationNode diagnosticNode
      remediationRel =
        relationshipPattern "has-remediation-0" "HAS_REMEDIATION" diagnosticNode remediationNode
  in graphRoot "diagnostic-graph-0" [locationNode, diagnosticNode, remediationNode, atRel, remediationRel]

subjectSymbolText :: Subject -> String
subjectSymbolText = (\(Symbol s) -> s) . Subj.identity

graphNodeFromSubject :: Int -> Subject -> Pattern Subject
graphNodeFromSubject depth subj =
  Pattern
    subj
      { properties =
          Map.insert "_depth" (VInteger (fromIntegral depth))
            (Map.insert "_arity" (VInteger (fromIntegral arity)) (properties subj))
      }
    []
  where
    arity = 0

graphNodeFromPattern :: Int -> Pattern Subject -> Pattern Subject
graphNodeFromPattern depth pat =
  Pattern updatedSubject []
  where
    updatedSubject =
      (value pat)
        { properties =
            Map.insert "_depth" (VInteger (fromIntegral depth))
              (Map.insert "_arity" (VInteger (fromIntegral (length (elements pat)))) (properties (value pat)))
        }

relationshipId :: String -> Pattern Subject -> Pattern Subject -> String
relationshipId prefix src tgt =
  prefix <> "-" <> subjectSymbolText (value src) <> "-" <> subjectSymbolText (value tgt)

diagnosticForward :: ScopeQuery q Subject => q Subject -> Pattern Subject -> Pattern Subject
diagnosticForward q pat =
  graphRoot (subjectSymbolText (value pat) <> "-graph") graphElements
  where
    (_, graphElements) = paraWithScope q build pat 0

    build :: ScopeQuery q Subject => q Subject -> Pattern Subject -> [Int -> (Pattern Subject, [Pattern Subject])] -> Int -> (Pattern Subject, [Pattern Subject])
    build _ current childBuilders depth =
      let currentNode = graphNodeFromPattern depth current
          childResults = map ($ (depth + 1)) childBuilders
          childNodes = map fst childResults
          childElements = concatMap snd childResults
          rels = concatMap (relationshipFor currentNode) childNodes
      in (currentNode, [currentNode] ++ childElements ++ rels)

    relationshipFor :: Pattern Subject -> Pattern Subject -> [Pattern Subject]
    relationshipFor parentNode childNode
      | patternHasLabel "Location" parentNode && patternHasLabel "Diagnostic" childNode =
          [relationshipPattern (relationshipId "at" parentNode childNode) "AT" parentNode childNode]
      | patternHasLabel "Diagnostic" parentNode && patternHasLabel "Remediation" childNode =
          [relationshipPattern (relationshipId "has-remediation" parentNode childNode) "HAS_REMEDIATION" parentNode childNode]
      | otherwise = []

diagnosticInverse :: ScopeQuery q Subject => q Subject -> Pattern Subject -> Pattern Subject
diagnosticInverse _ graph =
  case find (patternHasLabel "Diagnostic") graphElements of
    Nothing -> graph
    Just diagnosticNode ->
      case findLocation diagnosticNode of
        Nothing -> graph
        Just locationNode ->
          Pattern (stripDiagnosticMetadata (value locationNode))
            [ Pattern (stripDiagnosticMetadata (value diagnosticNode))
                (map toRemediationPattern remediationNodes)
            ]
      where
        remediationNodes =
          filter
            (\candidate ->
              patternHasLabel "Remediation" candidate
                && any (connectedBy "HAS_REMEDIATION" diagnosticNode candidate) (containers scope candidate)
            )
            graphElements
  where
    scope = topLevelGraphScope graph
    graphElements = allElements scope

    findLocation diagnosticNode =
      do
        atRel <- find (connectedToDiagnostic diagnosticNode) (containers scope diagnosticNode)
        case elements atRel of
          [locationNode, _] | patternHasLabel "Location" locationNode -> Just locationNode
          _ -> Nothing

    connectedToDiagnostic diagnosticNode rel =
      patternHasLabel "AT" rel
        && case elements rel of
          [_, targetNode] -> targetNode == diagnosticNode
          _ -> False

    toRemediationPattern remNode = Pattern (stripDiagnosticMetadata (value remNode)) []

diagnosticMap :: RepresentationMap Subject
diagnosticMap = RepresentationMap
  { name = "diagnosticMap"
  , domain = diagnosticPatternKind
  , codomain = diagnosticGraphKind
    -- The current prototype documents its encoding choices here rather than
    -- storing prose on the map value itself: `_arity` preserves direct element
    -- count and `_depth` preserves nesting depth in graph-form nodes.
  , forward = diagnosticForward
  , inverse = diagnosticInverse
  , roundTrip = \q p -> (inverse diagnosticMap q . forward diagnosticMap q) p == p
  }

genDiagnosticPattern :: QC.Gen (Pattern Subject)
genDiagnosticPattern = do
  seed <- QC.chooseInt (1, 100000)
  remediationCount <- QC.chooseInt (0, 3)
  let locationId = "location-" <> show seed
      diagnosticId = "diagnostic-" <> show seed
      remediationIds = [0 .. remediationCount - 1]
      remediationPats =
        [ Pattern (subjectWith ("remediation-" <> show seed <> "-" <> show idx) ["Remediation"] []) []
        | idx <- remediationIds
        ]
  pure $
    Pattern (subjectWith locationId ["Location"] [])
      [Pattern (subjectWith diagnosticId ["Diagnostic"] []) remediationPats]

identityLikeMap :: RepresentationMap Subject
identityLikeMap = RepresentationMap
  { name = "diagnosticGraphIdentity"
  , domain = diagnosticGraphKind
  , codomain = diagnosticGraphKind
  , forward = \_ p -> p
  , inverse = \_ p -> p
  , roundTrip = \_ _ -> True
  }

spec :: Spec
spec =
  describe "Pattern.RepresentationMap scaffolding" $
    do
      describe "PatternKind" $ do
        it "canonical example satisfies its own kind" $ do
          let results =
                [ kindPred leafPatternKind
                    (trivialScope (kindExample leafPatternKind))
                    (kindExample leafPatternKind)
                , kindPred branchPatternKind
                    (trivialScope (kindExample branchPatternKind))
                    (kindExample branchPatternKind)
                , kindPred visiblePatternKind
                    (trivialScope (kindExample visiblePatternKind))
                    (kindExample visiblePatternKind)
                ]

          results `shouldBe` [True, True, True]

        it "returns False for a pattern outside the kind's structural constraint" $ do
          let nonMatching = Pattern "branch" [Pattern "twig" []]

          checkKind branchPatternKind (trivialScope nonMatching) nonMatching `shouldBe` False

        it "can use allElements to recognize patterns visible only through scope" $ do
          let hidden = Pattern "target" []
              root = Pattern "root" [Pattern "other" []]
              visibleScope = VisibleScope [root, hidden]

          checkKind visiblePatternKind visibleScope hidden `shouldBe` True
          checkKind visiblePatternKind (trivialScope root) hidden `shouldBe` False

        it "returns the same result for structural predicates regardless of scope" $ do
          let candidate = kindExample leafPatternKind
              narrowScope :: TrivialScope String
              narrowScope = trivialScope candidate
              wideScope = VisibleScope [Pattern "noise" [], candidate, kindExample branchPatternKind]

          kindPred leafPatternKind narrowScope candidate
            `shouldBe` kindPred leafPatternKind wideScope candidate

      describe "RepresentationMap" $ do
        it "re-exports key APIs from Pattern" $ do
          let example = kindExample leafPatternKind

          checkKind leafPatternKind (trivialScope example) example `shouldBe` True
          case compose wrapLeafMap boxWrappedMap of
            Left err -> expectationFailure err
            Right composedMap -> name composedMap `shouldBe` "wrapLeaf >>> boxWrapped"

        it "forward produces a pattern accepted by the codomain kind" $ do
          let domainPattern = kindExample (domain wrapLeafMap)
              scope = trivialScope domainPattern

          checkKind (codomain wrapLeafMap) scope (forward wrapLeafMap scope domainPattern)
            `shouldBe` True

        it "inverse produces a pattern accepted by the domain kind" $ do
          let codomainPattern = kindExample (codomain wrapLeafMap)
              scope = trivialScope codomainPattern

          checkKind (domain wrapLeafMap) scope (inverse wrapLeafMap scope codomainPattern)
            `shouldBe` True

      describe "compose" $ do
        it "returns a composed map with the expected joined name" $ do
          case compose wrapLeafMap boxWrappedMap of
            Left err -> expectationFailure err
            Right composedMap -> name composedMap `shouldBe` "wrapLeaf >>> boxWrapped"

        it "returns Left with both kind names when map kinds are incompatible" $ do
          case compose wrapLeafMap mismatchedBoxMap of
            Left err -> do
              err `shouldContain` "WrappedLeaf"
              err `shouldContain` "AliasedWrapped"
            Right _ -> expectationFailure "Expected incompatible kinds to fail composition"

        it "applies forward then inverse in the documented composition order" $ do
          case compose wrapLeafMap boxWrappedMap of
            Left err -> expectationFailure err
            Right composedMap -> do
              let leafPattern = kindExample leafPatternKind
                  boxedPattern = kindExample boxedPatternKind
                  leafScope = trivialScope leafPattern
                  boxedScope = trivialScope boxedPattern

              forward composedMap leafScope leafPattern
                `shouldBe` forward boxWrappedMap leafScope (forward wrapLeafMap leafScope leafPattern)

              inverse composedMap boxedScope boxedPattern
                `shouldBe` inverse wrapLeafMap boxedScope (inverse boxWrappedMap boxedScope boxedPattern)

      describe "diagnosticMap" $ do
        it "forward maps the canonical diagnostic pattern into the graph kind" $ do
          let scope = trivialScope canonicalDiagnosticPattern

          checkKind diagnosticGraphKind scope (forward diagnosticMap scope canonicalDiagnosticPattern)
            `shouldBe` True

        it "inverse maps the canonical diagnostic graph into the pattern kind" $ do
          let scope = topLevelGraphScope canonicalDiagnosticGraph

          checkKind diagnosticPatternKind scope (inverse diagnosticMap scope canonicalDiagnosticGraph)
            `shouldBe` True

        it "round-trips the canonical diagnostic example" $ do
          let scope = trivialScope canonicalDiagnosticPattern

          (inverse diagnosticMap scope . forward diagnosticMap scope) canonicalDiagnosticPattern
            `shouldBe` canonicalDiagnosticPattern

        it "preserves generated diagnostic patterns under round-trip" $
          property $
            QC.forAll genDiagnosticPattern $ \pat ->
              roundTrip diagnosticMap (trivialScope pat) pat

        it "composes with an identity-like graph map and preserves canonical round-trip" $ do
          case compose diagnosticMap identityLikeMap of
            Left err -> expectationFailure err
            Right composedMap ->
              roundTrip composedMap (trivialScope canonicalDiagnosticPattern) canonicalDiagnosticPattern
                `shouldBe` True
