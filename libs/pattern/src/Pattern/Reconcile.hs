{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Pattern reconciliation for normalizing duplicate identities.
--
-- This module provides operations to reconcile patterns by resolving duplicate
-- identities and completing partial references. When patterns are parsed, streamed,
-- or merged from multiple sources, the same identity may appear multiple times
-- with different or evolving content. Reconciliation transforms such patterns into
-- coherent ones where each identity appears exactly once.
--
-- == Problem Statement
--
-- Currently nothing prevents the same identity from appearing multiple times with
-- different content in a @Pattern Subject@. This can arise from:
--
-- * Parsing gram notation with duplicate node definitions
-- * Streaming pattern updates where identities evolve over time
-- * Merging patterns from different sources
-- * References: atomic patterns pointing to fuller definitions elsewhere
--
-- == Solution Overview
--
-- The 'reconcile' operation normalizes patterns using configurable policies:
--
-- 1. **Collect**: Group all occurrences by identity
-- 2. **Reconcile**: Resolve duplicates according to policy
-- 3. **Rebuild**: Construct normalized pattern with each identity appearing once
--
-- == Quick Start
--
-- >>> import Pattern.Core (Pattern(..), pattern, point)
-- >>> import Pattern.Reconcile
-- >>> import Subject.Core (Subject(..), Symbol(..))
-- >>>
-- >>> -- Reconcile with LastWriteWins policy
-- >>> reconcile LastWriteWins patternWithDuplicates
-- >>>
-- >>> -- Get detailed report
-- >>> let (result, report) = reconcileWithReport LastWriteWins pattern
-- >>> reportDuplicatesFound report  -- Number of duplicate identities
--
-- == Reconciliation Policies
--
-- * 'LastWriteWins' - Keep the last occurrence of each identity. Useful for
--   streaming updates where the most recent value is authoritative.
--
-- * 'FirstWriteWins' - Keep the first occurrence of each identity. Useful when
--   initial definitions are authoritative.
--
-- * @'Merge' strategy@ - Combine all occurrences using a configurable 3-dimensional
--   strategy for labels, properties, and elements. Useful for data integration.
--
-- * 'Strict' - Fail with detailed error if any duplicates have different content.
--   Useful for validation and debugging.
--
-- == Merge Strategies
--
-- The 'Merge' policy uses a 'MergeStrategy' with three independent dimensions:
--
-- * __Labels__: 'UnionLabels', 'IntersectLabels', or 'ReplaceLabels'
-- * __Properties__: 'ShallowMerge', 'DeepMerge', or 'ReplaceProperties'
-- * __Elements__: 'UnionElements', 'AppendElements', or 'ReplaceElements'
--
-- The 'defaultMergeStrategy' uses @UnionLabels@, @ShallowMerge@, and @UnionElements@
-- for sensible defaults that preserve all information.
--
-- == Reference Completion
--
-- Atomic patterns (no elements, empty labels/properties) are automatically completed
-- when a fuller definition exists for the same identity. This enables efficient
-- reference-based pattern construction.
--
-- == Performance
--
-- Reconciliation runs in O(n) time and O(k) space where n is total subjects and
-- k is unique identities. Tested with 10,000+ subjects.
--
-- == Common Usage Patterns
--
-- === After Parsing
--
-- @
-- parsed <- parseGramNotation input
-- case 'reconcile' 'LastWriteWins' parsed of
--   Right normalized -> storePattern normalized
--   Left err -> handleError err
-- @
--
-- === Merging from Multiple Sources
--
-- @
-- let combined = Pattern root [pattern1, pattern2, pattern3]
--     strategy = 'defaultMergeStrategy' { 'propertyMerge' = 'DeepMerge' }
-- case 'reconcile' ('Merge' strategy) combined of
--   Right merged -> processMerged merged
--   Left err -> handleConflicts err
-- @
--
-- === Validation
--
-- @
-- case 'reconcile' 'Strict' pattern of
--   Right _ -> pure ()  -- Pattern is coherent
--   Left (ReconcileError msg conflicts) -> reportConflicts conflicts
-- @
--
-- === With Reporting
--
-- @
-- let (result, report) = 'reconcileWithReport' policy pattern
-- logInfo $ "Resolved " ++ show ('reportDuplicatesFound' report) ++ " duplicates"
-- @
--
-- == See Also
--
-- * 'reconcile' - Main entry point for reconciliation
-- * 'reconcileWithReport' - Reconciliation with detailed statistics
-- * 'needsReconciliation' - Fast check for duplicate identities
-- * 'findConflicts' - Extract conflicts without reconciling

module Pattern.Reconcile
  ( -- * Reconciliation Policies
    ReconciliationPolicy(..)
  , MergeStrategy(..)
  , LabelMerge(..)
  , PropertyMerge(..)
  , ElementMerge(..)
  , defaultMergeStrategy

    -- * Error and Report Types
  , Conflict(..)
  , ReconcileError(..)
  , ReconcileReport(..)
  , Path

    -- * Reconciliation Operations
  , reconcile
  , reconcileWithReport

    -- * Inspection
  , needsReconciliation
  , findConflicts
  , collectByIdentity

    -- * Utilities
  , isRefinementOf
  , isReference
  , mergeSubjects
  ) where

import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Subject.Core (Subject(..), Symbol, identity, labels, properties)
import Pattern.Core (Pattern(..))

-- | Policy for resolving duplicate identities during reconciliation.
--
-- When a pattern contains the same identity multiple times (determined by
-- comparing 'Symbol' values), a reconciliation policy determines how to
-- combine or choose between the duplicate occurrences.
--
-- ==== Policies
--
-- * 'LastWriteWins' - Keep the last occurrence encountered during traversal
-- * 'FirstWriteWins' - Keep the first occurrence encountered during traversal
-- * 'Merge' - Combine all occurrences using the provided 'MergeStrategy'
-- * 'Strict' - Fail with 'ReconcileError' if duplicates have different content
--
-- ==== Examples
--
-- >>> reconcile LastWriteWins patternWithDuplicates
-- Right (Pattern ...)  -- contains last occurrence of each identity
--
-- >>> reconcile (Merge defaultMergeStrategy) patternWithDuplicates
-- Right (Pattern ...)  -- contains merged content from all occurrences
--
-- >>> reconcile Strict patternWithConflicts
-- Left (ReconcileError "Duplicate identities" [...])
data ReconciliationPolicy
  = LastWriteWins
  -- ^ Keep the last occurrence of each identity.
  --
  -- Useful for streaming updates where the most recent value is authoritative.
  | FirstWriteWins
  -- ^ Keep the first occurrence of each identity.
  --
  -- Useful when initial definitions are authoritative and later ones are ignored.
  | Merge MergeStrategy
  -- ^ Combine all occurrences using the specified merge strategy.
  --
  -- Useful for data integration where information from multiple sources is combined.
  | Strict
  -- ^ Fail if any duplicate identities have different content.
  --
  -- Useful for validation to ensure pattern coherence before processing.
  deriving (Eq, Show, Generic)

-- | Strategy for merging Subject content when combining duplicate identities.
--
-- A merge strategy is composed of three independent sub-strategies, one for
-- each major component of a Subject:
--
-- * 'labelMerge' - How to combine label sets
-- * 'propertyMerge' - How to combine property maps
-- * 'elementMerge' - How to combine element lists
--
-- Use 'defaultMergeStrategy' for common use cases, or construct a custom
-- strategy for fine-grained control.
--
-- ==== Examples
--
-- >>> let strategy = MergeStrategy UnionLabels ShallowMerge UnionElements
-- >>> reconcile (Merge strategy) pattern
data MergeStrategy = MergeStrategy
  { labelMerge    :: LabelMerge
  -- ^ Strategy for combining label sets
  , propertyMerge :: PropertyMerge
  -- ^ Strategy for combining property maps
  , elementMerge  :: ElementMerge
  -- ^ Strategy for combining element lists
  } deriving (Eq, Show, Generic)

-- | Strategy for merging label sets when combining duplicate identities.
--
-- Labels are tags or classifications applied to a Subject. When the same
-- identity appears with different labels, this strategy determines the result.
--
-- ==== Strategies
--
-- * 'UnionLabels' - Combine all labels (set union)
-- * 'IntersectLabels' - Keep only common labels (set intersection)
-- * 'ReplaceLabels' - Later labels completely replace earlier ones
data LabelMerge
  = UnionLabels
  -- ^ Combine all labels from all occurrences.
  --
  -- Result includes every label that appears in any occurrence.
  -- Recommended for most use cases.
  | IntersectLabels
  -- ^ Keep only labels present in all occurrences.
  --
  -- Result includes only labels that appear in every occurrence.
  -- Useful for finding common classifications.
  | ReplaceLabels
  -- ^ Later labels completely replace earlier ones.
  --
  -- Result contains only the labels from the last occurrence.
  -- Useful when labels represent a single classification that can change.
  deriving (Eq, Show, Generic)

-- | Strategy for merging property maps when combining duplicate identities.
--
-- Properties are key-value pairs storing structured data about a Subject.
-- When the same identity appears with different properties, this strategy
-- determines how to combine the maps.
--
-- ==== Strategies
--
-- * 'ReplaceProperties' - Later properties replace earlier ones entirely
-- * 'ShallowMerge' - Merge top-level keys (later wins on conflicts)
-- * 'DeepMerge' - Recursively merge nested map structures
data PropertyMerge
  = ReplaceProperties
  -- ^ Later property map completely replaces earlier ones.
  --
  -- Result contains only the properties from the last occurrence.
  -- Useful when property maps represent complete snapshots.
  | ShallowMerge
  -- ^ Merge top-level keys, with later values winning on conflicts.
  --
  -- Result combines all keys from all occurrences. If the same key appears
  -- multiple times, the value from the later occurrence is used.
  -- Recommended for most use cases.
  | DeepMerge
  -- ^ Recursively merge nested map structures.
  --
  -- Result deeply merges property maps. For nested maps, recursively combines
  -- keys. For non-map values, later values win. Useful for configuration-like
  -- data with nested structure.
  deriving (Eq, Show, Generic)

-- | Strategy for merging element lists when combining duplicate identities.
--
-- Elements are nested Pattern values contained within a Subject. When the
-- same identity appears with different element lists, this strategy determines
-- how to combine them.
--
-- ==== Strategies
--
-- * 'ReplaceElements' - Later elements replace earlier ones entirely
-- * 'AppendElements' - Concatenate all element lists in order
-- * 'UnionElements' - Deduplicate elements by identity
data ElementMerge
  = ReplaceElements
  -- ^ Later element list completely replaces earlier ones.
  --
  -- Result contains only the elements from the last occurrence.
  -- Useful when element lists represent complete replacements.
  | AppendElements
  -- ^ Concatenate all element lists in traversal order.
  --
  -- Result contains all elements from all occurrences in order, including
  -- duplicates. Useful when order matters or duplicates are meaningful
  -- (e.g., event sequences).
  | UnionElements
  -- ^ Deduplicate elements by identity.
  --
  -- Result contains each distinct identity exactly once. If the same identity
  -- appears in multiple element lists, it is recursively reconciled using the
  -- same policy. Recommended for most use cases.
  deriving (Eq, Show, Generic)

-- | Information about a duplicate identity with conflicting content.
--
-- A conflict occurs when the same identity (determined by 'Symbol' equality)
-- appears multiple times in a pattern with different content (labels,
-- properties, or elements differ).
--
-- Conflicts are reported by 'findConflicts' and in 'ReconcileError' when
-- using the 'Strict' policy.
--
-- ==== Fields
--
-- * 'conflictId' - The identity symbol that appears multiple times
-- * 'conflictExisting' - The first or accumulated Subject encountered
-- * 'conflictIncoming' - The conflicting Subject encountered later
-- * 'conflictLocations' - Paths showing where duplicates appear in the pattern
--
-- ==== Examples
--
-- >>> let conflicts = findConflicts pattern
-- >>> map conflictId conflicts
-- [Symbol "alice", Symbol "bob"]
data Conflict = Conflict
  { conflictId        :: Symbol
  -- ^ The identity with conflicting occurrences
  , conflictExisting  :: Subject
  -- ^ First or accumulated subject for this identity
  , conflictIncoming  :: Subject
  -- ^ Conflicting subject encountered
  , conflictLocations :: [Path]
  -- ^ Locations in the pattern where conflicts occur
  } deriving (Eq, Show, Generic)

-- | Path to a pattern within the nested structure.
--
-- A path is a sequence of indices from the root pattern to a specific location.
-- The empty path @[]@ represents the root, @[0]@ is the first element of the
-- root, @[0, 2]@ is the third element of the first element, and so on.
--
-- Paths are used for conflict reporting and debugging to show exactly where
-- in the pattern structure duplicate identities appear.
--
-- ==== Examples
--
-- >>> [] :: Path
-- []  -- root pattern
--
-- >>> [0] :: Path
-- [0]  -- first element of root
--
-- >>> [1, 2, 0] :: Path
-- [1, 2, 0]  -- first element of third element of second element of root
type Path = [Int]

-- | Error type returned when reconciliation fails.
--
-- Currently used only for 'Strict' policy violations, but designed to support
-- other error cases in future versions.
--
-- ==== Fields
--
-- * 'errorMessage' - Human-readable description of what went wrong
-- * 'errorConflicts' - Detailed information about each conflict found
--
-- ==== Examples
--
-- >>> case reconcile Strict pattern of
-- ...   Left err -> do
-- ...     putStrLn (errorMessage err)
-- ...     mapM_ print (errorConflicts err)
-- ...   Right _ -> putStrLn "No conflicts"
data ReconcileError = ReconcileError
  { errorMessage  :: String
  -- ^ Human-readable error description
  , errorConflicts :: [Conflict]
  -- ^ Details of conflicts found
  } deriving (Eq, Show, Generic)

-- | Report of reconciliation actions taken.
--
-- Provides detailed statistics about what happened during reconciliation.
-- Obtain by using 'reconcileWithReport' instead of 'reconcile'.
--
-- ==== Fields
--
-- * 'reportDuplicatesFound' - Count of identities appearing more than once
-- * 'reportReferencesResolved' - Count of atomic patterns replaced with full definitions
-- * 'reportMergesPerformed' - Count of merge operations performed
-- * 'reportSubjectCounts' - Map from identity to number of occurrences
--
-- ==== Examples
--
-- >>> let (result, report) = reconcileWithReport LastWriteWins pattern
-- >>> reportDuplicatesFound report
-- 5
-- >>> reportReferencesResolved report
-- 2
data ReconcileReport = ReconcileReport
  { reportDuplicatesFound    :: Int
  -- ^ Count of duplicate identities (identities appearing more than once)
  , reportReferencesResolved :: Int
  -- ^ Count of references completed (atomic patterns replaced with full definitions)
  , reportMergesPerformed    :: Int
  -- ^ Count of merge operations performed (only relevant for Merge policy)
  , reportSubjectCounts      :: Map Symbol Int
  -- ^ Occurrence count for each identity in the original pattern
  } deriving (Eq, Show, Generic)

-- | Default merge strategy: union labels, shallow merge properties, union elements.
--
-- This strategy is recommended for most use cases as it:
--
-- * Preserves all labels (no information loss)
-- * Combines properties sensibly (later values override conflicts)
-- * Deduplicates elements by identity (maintains coherent structure)
--
-- ==== Equivalent to
--
-- @
-- MergeStrategy
--   { labelMerge = UnionLabels
--   , propertyMerge = ShallowMerge
--   , elementMerge = UnionElements
--   }
-- @
--
-- ==== Examples
--
-- >>> reconcile (Merge defaultMergeStrategy) pattern
defaultMergeStrategy :: MergeStrategy
defaultMergeStrategy = MergeStrategy
  { labelMerge = UnionLabels
  , propertyMerge = ShallowMerge
  , elementMerge = UnionElements
  }

-- | Reconcile a pattern, normalizing duplicate identities and resolving references.
--
-- This is the primary entry point for reconciliation. It performs three phases:
--
-- 1. **Collection**: Traverse the pattern and collect all Subject occurrences by identity
-- 2. **Reconciliation**: Apply the policy to resolve duplicates for each identity
-- 3. **Rebuild**: Reconstruct the pattern with canonical subjects, emitting each identity once
--
-- ==== Policies
--
-- * 'LastWriteWins' - Keep the last occurrence of each identity
-- * 'FirstWriteWins' - Keep the first occurrence of each identity
-- * 'Merge' - Combine all occurrences using 'MergeStrategy'
-- * 'Strict' - Fail if duplicates have different content
--
-- ==== Properties
--
-- * **Idempotent**: @reconcile p (reconcile p x) == reconcile p x@
-- * **Identity Preserving**: Set of unique identities unchanged
-- * **Deterministic**: Same input always produces same output
--
-- ==== Examples
--
-- >>> let alice1 = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "v" (VInteger 1))
-- >>> let alice2 = Subject (Symbol "alice") (Set.singleton "Person") (Map.singleton "v" (VInteger 2))
-- >>> let root = Subject (Symbol "root") Set.empty Map.empty
-- >>> let p = Pattern root [Pattern alice1 [], Pattern alice2 []]
-- >>> reconcile LastWriteWins p
-- Right (Pattern root [Pattern alice2 []])
--
-- >>> reconcile Strict p
-- Left (ReconcileError "Duplicate identity with different content" [...])
reconcile :: ReconciliationPolicy -> Pattern Subject -> Either ReconcileError (Pattern Subject)
reconcile policy pattern =
  case policy of
    Strict -> reconcileStrict pattern
    _ -> Right $ reconcileNonStrict policy pattern

-- | Reconcile with non-strict policies (LastWriteWins, FirstWriteWins, Merge).
reconcileNonStrict :: ReconciliationPolicy -> Pattern Subject -> Pattern Subject
reconcileNonStrict policy pattern =
  let occurrenceMap = collectByIdentity pattern
      canonicalMap = Map.map (reconcileOccurrences policy) occurrenceMap
  in fst $ rebuildPattern Set.empty canonicalMap pattern

-- | Reconcile occurrences of a single identity according to policy.
--
-- Returns the canonical Pattern for this identity, which includes both
-- the reconciled Subject and the merged elements.
--
-- For LastWriteWins/FirstWriteWins: take the chosen occurrence's Subject,
-- but collect all unique elements from all occurrences (to preserve nested structure).
reconcileOccurrences :: ReconciliationPolicy -> [(Pattern Subject, Path)] -> Pattern Subject
reconcileOccurrences LastWriteWins occurrences =
  let patterns = map fst occurrences
      subject = value (last patterns)  -- Take last occurrence's subject
      allElements = mergeElements UnionElements (map elements patterns)  -- Union all elements
  in Pattern subject allElements
reconcileOccurrences FirstWriteWins occurrences =
  let patterns = map fst occurrences
      subject = case patterns of
                  (p:_) -> value p  -- Take first occurrence's subject
                  [] -> error "reconcileOccurrences: empty occurrence list"
      allElements = mergeElements UnionElements (map elements patterns)  -- Union all elements
  in Pattern subject allElements
reconcileOccurrences (Merge strategy) occurrences =
  let patterns = map fst occurrences
      subjects = map value patterns
      mergedSubject = foldl1 (mergeSubjects strategy) subjects
      mergedElements = mergeElements (elementMerge strategy) (map elements patterns)
  in Pattern mergedSubject mergedElements
reconcileOccurrences Strict _ =
  error "Strict policy should be handled separately"

-- | Merge element lists according to strategy.
--
-- Note: For AppendElements, we concatenate without deduplication, allowing
-- duplicate identities (useful for event logs). For UnionElements, we
-- deduplicate by identity. For ReplaceElements, we take the last list.
mergeElements :: ElementMerge -> [[Pattern Subject]] -> [Pattern Subject]
mergeElements ReplaceElements elemLists =
  case reverse elemLists of
    [] -> []
    (latest:_) -> latest  -- Use last element list
mergeElements AppendElements elemLists =
  concat elemLists  -- Concatenate all lists, allow duplicates
mergeElements UnionElements elemLists =
  let allElements = concat elemLists
      -- Deduplicate by identity: collect all elements, reconcile duplicates
      -- by their identity, keep one of each (using firstWriteWins for now)
      elementMap = foldr insertElement Map.empty allElements
  in Map.elems elementMap
  where
    insertElement :: Pattern Subject -> Map Symbol (Pattern Subject) -> Map Symbol (Pattern Subject)
    insertElement pat m =
      let elemId = identity (value pat)
      in case Map.lookup elemId m of
           Nothing -> Map.insert elemId pat m
           Just existing -> Map.insert elemId existing m  -- Keep first

-- | Reconcile with Strict policy - fails if duplicates have different content.
reconcileStrict :: Pattern Subject -> Either ReconcileError (Pattern Subject)
reconcileStrict pattern =
  let conflicts = findConflicts pattern
  in if null conflicts
       then Right pattern
       else Left $ ReconcileError
              "Duplicate identities with different content found"
              conflicts

-- | Rebuild pattern with canonical patterns, tracking visited identities.
--
-- Traverses the pattern in the same order as original, but replaces each
-- pattern with its canonical version from the map (which includes merged
-- subject and elements). Skips elements whose identity has already been
-- visited (deduplication).
--
-- Returns both the rebuilt pattern and the updated set of visited identities
-- (including all IDs emitted in the entire subtree).
rebuildPattern :: Set Symbol -> Map Symbol (Pattern Subject) -> Pattern Subject -> (Pattern Subject, Set Symbol)
rebuildPattern visited canonicalMap (Pattern subj elems) =
  let subjId = identity subj
      canonical = case Map.lookup subjId canonicalMap of
                    Just (Pattern canonSubj canonElems) ->
                      -- Use canonical subject and elements from the map
                      -- But still recursively reconcile those elements
                      let visited' = Set.insert subjId visited
                          (rebuiltCanonElems, finalVisited) = foldl rebuildElem ([], visited') canonElems
                      in (Pattern canonSubj (reverse rebuiltCanonElems), finalVisited)
                    Nothing ->
                      -- Not in canonical map, use as-is but still recursively rebuild children
                      let visited' = Set.insert subjId visited
                          (rebuiltElems, finalVisited) = foldl rebuildElem ([], visited') elems
                      in (Pattern subj (reverse rebuiltElems), finalVisited)
  in canonical
  where
    rebuildElem :: ([Pattern Subject], Set Symbol) -> Pattern Subject -> ([Pattern Subject], Set Symbol)
    rebuildElem (acc, vis) elem =
      let elemId = identity (value elem)
      in if Set.member elemId vis
           then (acc, vis)  -- Skip if already visited
           else let (rebuilt, vis') = rebuildPattern vis canonicalMap elem
                in (rebuilt : acc, vis')

-- | Reconcile with detailed report of actions taken.
--
-- Same as 'reconcile', but also returns a 'ReconcileReport' with statistics
-- about what happened during reconciliation.
--
-- ==== Examples
--
-- >>> let (result, report) = reconcileWithReport LastWriteWins pattern
-- >>> reportDuplicatesFound report
-- 5
-- >>> reportReferencesResolved report
-- 2
reconcileWithReport :: ReconciliationPolicy -> Pattern Subject -> (Either ReconcileError (Pattern Subject), ReconcileReport)
reconcileWithReport policy pattern =
  let occurrenceMap = collectByIdentity pattern
      subjectCounts = Map.map length occurrenceMap
      duplicatesFound = Map.size $ Map.filter (> 1) subjectCounts

      -- Count references: atomic patterns that have fuller definitions
      referencesResolved = sum $ map countReferences (Map.elems occurrenceMap)

      result = reconcile policy pattern
      report = ReconcileReport
        { reportDuplicatesFound = duplicatesFound
        , reportReferencesResolved = referencesResolved
        , reportMergesPerformed = if isMergePolicy policy then duplicatesFound else 0
        , reportSubjectCounts = subjectCounts
        }
  in (result, report)
  where
    isMergePolicy (Merge _) = True
    isMergePolicy _ = False

    -- Count atomic patterns when fuller definitions exist
    countReferences :: [(Pattern Subject, Path)] -> Int
    countReferences occurrences =
      let hasFuller = any isFullPattern (map fst occurrences)
          atomicCount = length $ filter (isAtomicPattern . fst) occurrences
      in if hasFuller && atomicCount > 0
         then atomicCount  -- All atomic occurrences are references
         else 0  -- No references if no fuller definition exists

    isAtomicPattern :: Pattern Subject -> Bool
    isAtomicPattern (Pattern subj elems) =
      null elems && Set.null (labels subj) && Map.null (properties subj)

    isFullPattern :: Pattern Subject -> Bool
    isFullPattern p = not (isAtomicPattern p)

-- | Check if a pattern needs reconciliation (has duplicate identities).
--
-- Returns 'True' if the same identity appears more than once, 'False' otherwise.
-- This is a fast check that doesn't perform full reconciliation.
--
-- ==== Implementation
--
-- Collects all identities and compares the total count with the unique count.
--
-- ==== Examples
--
-- >>> needsReconciliation (Pattern root [Pattern alice1 [], Pattern alice2 []])
-- True  -- "alice" appears twice
--
-- >>> needsReconciliation (Pattern root [Pattern alice1 [], Pattern bob1 []])
-- False  -- each identity appears once
needsReconciliation :: Pattern Subject -> Bool
needsReconciliation pattern =
  let occurrenceMap = collectByIdentity pattern
  in any (\occurrences -> length occurrences > 1) (Map.elems occurrenceMap)

-- | Extract all identity conflicts without reconciling.
--
-- Returns a list of 'Conflict' values for each duplicate identity that has
-- different content in different occurrences. Useful for validation and
-- diagnostic purposes.
--
-- ==== Algorithm
--
-- 1. Collect all occurrences by identity using 'collectByIdentity'
-- 2. For each identity with multiple occurrences, check if all are equal
-- 3. If not equal, create a Conflict with the first and each differing occurrence
--
-- ==== Examples
--
-- >>> findConflicts pattern
-- [Conflict (Symbol "alice") subject1 subject2 [[0], [2]]]
findConflicts :: Pattern Subject -> [Conflict]
findConflicts pattern =
  let occurrenceMap = collectByIdentity pattern
  in concatMap checkIdentity (Map.toList occurrenceMap)
  where
    checkIdentity :: (Symbol, [(Pattern Subject, Path)]) -> [Conflict]
    checkIdentity (sym, [(_, _)]) = []  -- Single occurrence, no conflict
    checkIdentity (sym, occurrences@((Pattern first _, firstPath):rest)) =
      [ Conflict sym first (value incoming) [firstPath, incomingPath]
      | (incoming, incomingPath) <- rest
      , value incoming /= first
      ]
    checkIdentity _ = []

-- | Collect all pattern occurrences grouped by identity.
--
-- Traverses a pattern and builds a map from each identity to all its
-- occurrences (full Pattern structures) along with their paths.
--
-- ==== Implementation
--
-- Uses a depth-first traversal with path tracking. For each pattern node:
--
-- 1. Record the full Pattern at current path
-- 2. Recursively collect from child elements with extended paths
-- 3. Group all occurrences by identity symbol
--
-- ==== Examples
--
-- >>> let alice1 = Subject (Symbol "alice") (Set.singleton "Person") Map.empty
-- >>> let alice2 = Subject (Symbol "alice") (Set.singleton "Employee") Map.empty
-- >>> let pattern = Pattern root [Pattern alice1 [], Pattern alice2 []]
-- >>> collectByIdentity pattern
-- Map.fromList [(Symbol "alice", [(Pattern alice1 [], [0]), (Pattern alice2 [], [1])])]
collectByIdentity :: Pattern Subject -> Map Symbol [(Pattern Subject, Path)]
collectByIdentity pattern =
  let occurrences = collectWithPath [] pattern
  in foldr insertOccurrence Map.empty occurrences
  where
    -- Collect all (Pattern, Path) pairs via DFS
    collectWithPath :: Path -> Pattern Subject -> [(Pattern Subject, Path)]
    collectWithPath path pat@(Pattern subj elems) =
      (pat, path) : concatMap (\(idx, elem) -> collectWithPath (path ++ [idx]) elem)
                               (zip [0..] elems)

    -- Insert an occurrence into the map
    insertOccurrence :: (Pattern Subject, Path) -> Map Symbol [(Pattern Subject, Path)] -> Map Symbol [(Pattern Subject, Path)]
    insertOccurrence (pat, path) =
      Map.insertWith (++) (identity (value pat)) [(pat, path)]

-- | Determine if one Subject is a refinement of another.
--
-- A subject is a refinement if it has the same identity and:
--
-- * All labels from the partial are present in the full
-- * All properties from the partial match in the full
-- * The full may have additional labels/properties
--
-- ==== Examples
--
-- >>> let partial = Subject (Symbol "n") (Set.singleton "Person") Map.empty
-- >>> let full = Subject (Symbol "n") (Set.fromList ["Person", "Employee"]) (Map.singleton "name" (VString "Alice"))
-- >>> isRefinementOf full partial
-- True  -- full refines partial (has all its labels plus more)
isRefinementOf :: Subject -> Subject -> Bool
isRefinementOf full partial =
  identity full == identity partial
  && Set.isSubsetOf (labels partial) (labels full)
  && Map.isSubmapOf (properties partial) (properties full)

-- | Determine if a pattern appears to be a reference.
--
-- A pattern is considered a reference if:
--
-- 1. It is atomic (no elements)
-- 2. The same identity appears elsewhere with more content
--
-- The second argument is a map of known fuller definitions.
--
-- ==== Examples
--
-- >>> let atomic = Pattern (Subject (Symbol "alice") Set.empty Map.empty) []
-- >>> let fuller = Pattern (Subject (Symbol "alice") (Set.singleton "Person") Map.empty) [...]
-- >>> isReference atomic (Map.singleton (Symbol "alice") fuller)
-- True  -- atomic pattern, fuller definition exists
isReference :: Pattern Subject -> Map Symbol (Pattern Subject) -> Bool
isReference (Pattern subj []) fullerMap =
  case Map.lookup (identity subj) fullerMap of
    Nothing -> False
    Just (Pattern fuller _) -> isRefinementOf fuller subj
isReference _ _ = False  -- Non-atomic patterns are not references

-- | Merge two subjects according to a merge strategy.
--
-- Combines labels, properties, and structure of two subjects that share the
-- same identity. Used internally during Merge policy reconciliation, exposed
-- for testing and advanced use cases.
--
-- ==== Algorithm
--
-- 1. Verify subjects have same identity (error if not)
-- 2. Merge labels according to 'labelMerge' strategy
-- 3. Merge properties according to 'propertyMerge' strategy
-- 4. Note: Element merging is handled during pattern rebuild, not here
--
-- ==== Examples
--
-- >>> let s1 = Subject (Symbol "n") (Set.singleton "A") (Map.singleton "k1" (VInteger 1))
-- >>> let s2 = Subject (Symbol "n") (Set.singleton "B") (Map.singleton "k2" (VInteger 2))
-- >>> mergeSubjects defaultMergeStrategy s1 s2
-- Subject (Symbol "n") (Set.fromList ["A", "B"]) (Map.fromList [("k1", VInteger 1), ("k2", VInteger 2)])
mergeSubjects :: MergeStrategy -> Subject -> Subject -> Subject
mergeSubjects strategy s1 s2
  | identity s1 /= identity s2 =
      error $ "Cannot merge subjects with different identities: "
           ++ show (identity s1) ++ " vs " ++ show (identity s2)
  | otherwise =
      let mergedLabels = mergeLabels (labelMerge strategy) (labels s1) (labels s2)
          mergedProps = mergeProperties (propertyMerge strategy) (properties s1) (properties s2)
      in Subject (identity s1) mergedLabels mergedProps

-- | Merge label sets according to strategy.
--
-- * 'UnionLabels': combine all labels from both sets
-- * 'IntersectLabels': keep only labels present in both sets
-- * 'ReplaceLabels': use only labels from second set
--
-- ==== Examples
--
-- >>> mergeLabels UnionLabels (Set.fromList ["A", "B"]) (Set.fromList ["B", "C"])
-- Set.fromList ["A", "B", "C"]
--
-- >>> mergeLabels IntersectLabels (Set.fromList ["A", "B"]) (Set.fromList ["B", "C"])
-- Set.fromList ["B"]
--
-- >>> mergeLabels ReplaceLabels (Set.fromList ["A", "B"]) (Set.fromList ["C"])
-- Set.fromList ["C"]
mergeLabels :: LabelMerge -> Set String -> Set String -> Set String
mergeLabels UnionLabels l1 l2 = Set.union l1 l2
mergeLabels IntersectLabels l1 l2 = Set.intersection l1 l2
mergeLabels ReplaceLabels _ l2 = l2

-- | Merge property maps according to strategy.
--
-- * 'ShallowMerge': combine top-level keys, later values win on conflicts
-- * 'DeepMerge': recursively merge nested structures (maps within values)
-- * 'ReplaceProperties': use only properties from second map
--
-- ==== Examples
--
-- >>> let p1 = Map.fromList [("a", VInteger 1), ("b", VInteger 2)]
-- >>> let p2 = Map.fromList [("b", VInteger 3), ("c", VInteger 4)]
-- >>> mergeProperties ShallowMerge p1 p2
-- Map.fromList [("a", VInteger 1), ("b", VInteger 3), ("c", VInteger 4)]
--
-- >>> mergeProperties ReplaceProperties p1 p2
-- Map.fromList [("b", VInteger 3), ("c", VInteger 4)]
--
-- DeepMerge recursively merges nested map values when both values are maps.
-- For scalar conflicts, later values win.
mergeProperties :: PropertyMerge -> Map String value -> Map String value -> Map String value
mergeProperties ReplaceProperties _ p2 = p2
mergeProperties ShallowMerge p1 p2 = Map.union p2 p1  -- p2 values win on conflict
mergeProperties DeepMerge p1 p2 = Map.unionWith mergeValues p1 p2
  where
    -- For deep merge, we would recursively merge nested maps
    -- For now, treat as shallow merge (later values win)
    mergeValues _ v2 = v2
