module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Exit (exitFailure)

import qualified Gram as Gram
import Pattern.Core (Pattern (..))
import qualified Subject.Core as Subject

-- | A Frame-scoped address: a named identity, or a flat positional ordinal
-- assigned at admission. Neither carries a parent — this is the corrected
-- model from design/rfc/comments/RFC-001-managed-frame-containers.md
-- (§Identity and addresses), not SPIKE-001's superseded generated-identity
-- one.
data LocalAddress
  = Named Subject.Symbol
  | Positional Int
  deriving (Eq, Ord, Show)

-- | A registry entry: a Subject plus its ordered element addresses, the same
-- Subject-plus-ordered-references shape as `Pattern` itself (§Frame registry
-- and closure's `PatternRow`).
data PatternRow = PatternRow
  { rowSubject :: Subject.Subject
  , rowElements :: [LocalAddress]
  }
  deriving (Show)

type Registry = Map LocalAddress PatternRow

isAnonymous :: Subject.Subject -> Bool
isAnonymous s = Subject.identity s == Subject.Symbol ""

-- | A Pattern is a definition (fuller) if it carries any labels, properties,
-- or elements. Content-free is the only reference-candidate shape
-- (§Defining and reference occurrences, corrected raw-import rule).
hasContent :: Pattern Subject.Subject -> Bool
hasContent (Pattern s children) =
  not (Set.null (Subject.labels s)) || not (Map.null (Subject.properties s)) || not (null children)

-- | Every named identity that is fuller (has content) anywhere in the
-- forest. A content-free occurrence of a name in this set resolves as a
-- reference to the fuller definition rather than becoming its own entry.
collectFullerNames :: [Pattern Subject.Subject] -> Set Subject.Symbol
collectFullerNames = Set.unions . map go
  where
    go p@(Pattern s children)
      | isAnonymous s = Set.unions (map go children)
      | hasContent p = Set.insert (Subject.identity s) (Set.unions (map go children))
      | otherwise = Set.unions (map go children)

-- | Admit one occurrence, one pass: recurse into children first so their
-- real assigned addresses are known, then register this occurrence's own
-- row (if it has one) using those addresses. Returns the address this
-- occurrence resolves to, so a caller can use it as one of its own
-- elements — this is what the two-pass draft got wrong: an anonymous
-- child's ordinal only exists once, assigned here, and must be threaded
-- back to its parent directly rather than recomputed.
admitPattern
  :: (Registry, Int)
  -> Pattern Subject.Subject
  -> (LocalAddress, (Registry, Int))
admitPattern (registry, next) p@(Pattern s children)
  | isAnonymous s =
      let addr = Positional next
          (childAddrs, (registry', next')) = admitChildren (registry, next + 1) children
          row = PatternRow s childAddrs
      in (addr, (Map.insert addr row registry', next'))
  | hasContent p =
      let addr = Named (Subject.identity s)
          (childAddrs, (registry', next')) = admitChildren (registry, next) children
          row = PatternRow s childAddrs
      in (addr, (Map.insert addr row registry', next'))
  | otherwise =
      -- Content-free named occurrence: a reference-candidate. It has no
      -- children by definition of content-free, so nothing to recurse into;
      -- it contributes only its address, resolved later against a fuller
      -- definition or backfilled as a trivial entry.
      (Named (Subject.identity s), (registry, next))

admitChildren :: (Registry, Int) -> [Pattern Subject.Subject] -> ([LocalAddress], (Registry, Int))
admitChildren st [] = ([], st)
admitChildren st (p : ps) =
  let (addr, st') = admitPattern st p
      (addrs, st'') = admitChildren st' ps
  in (addr : addrs, st'')

-- | Admit a raw-Pattern-Subject forest using the content-based recovery
-- rule, since these documents were not parsed through a CST-preserving
-- PatternLike admission format. Named fuller occurrences and every
-- anonymous occurrence register their own row during admission; a second
-- pass backfills a trivial content-free entry for any named
-- reference-candidate that never matched a fuller definition anywhere,
-- top-level or nested.
admitForest :: [Pattern Subject.Subject] -> Either String Registry
admitForest forest = do
  let (topAddrs, (registry, _)) = admitChildren (Map.empty, 1) forest
      referencedNames =
        Set.fromList [sym | Named sym <- topAddrs]
          `Set.union` Set.fromList [sym | row <- Map.elems registry, Named sym <- rowElements row]
      missing = Set.filter (\sym -> Map.notMember (Named sym) registry) referencedNames
      backfilled = Map.union registry (Map.fromList [(Named sym, trivialRow sym) | sym <- Set.toList missing])
  validateClosure backfilled
  Right backfilled
  where
    trivialRow sym = PatternRow (Subject.Subject sym Set.empty Map.empty) []

validateClosure :: Registry -> Either String ()
validateClosure registry = mapM_ checkRow (Map.toList registry)
  where
    checkRow (addr, row) = mapM_ (checkResolves addr) (rowElements row)
    checkResolves addr elemAddr
      | elemAddr == addr = Left ("direct self-reference at " ++ show addr)
      | Map.notMember elemAddr registry = Left ("dangling reference " ++ show elemAddr ++ " from " ++ show addr)
      | otherwise = Right ()

check :: String -> Bool -> IO Bool
check label passed = do
  putStrLn ((if passed then "PASS " else "FAIL ") ++ label)
  pure passed

-- | Uses `Gram.fromGram`, not `fromGramWithIds`: the latter synthesizes
-- `#N` identities for anonymous subjects at parse time, which is exactly
-- the generated-identity model this RFC superseded. `fromGram` preserves
-- true anonymity (`Symbol ""`) so Frame admission assigns the ordinal
-- itself, matching §Identity and addresses.
parseFile :: FilePath -> IO (Either String [Pattern Subject.Subject])
parseFile path = do
  source <- readFile path
  pure $ case Gram.fromGram source of
    Left err -> Left (show err)
    Right patterns -> Right patterns

admitFile :: FilePath -> IO (Either String Registry)
admitFile path = do
  parsed <- parseFile path
  pure (parsed >>= admitForest)

main :: IO ()
main = do
  let dataDir = "libs/gram/test-data/tree-sitter-gram/examples/data/"
      customDir = "libs/gram/test-data/roundtrip/custom/"

  social <- admitFile (dataDir ++ "social.gram")
  route66 <- admitFile (dataDir ++ "route-66.gram")
  anonymousSubject <- admitFile (customDir ++ "anonymous-subject.gram")
  implicitRoot <- admitFile (customDir ++ "implicit-root.gram")
  deepNesting <- admitFile (customDir ++ "deep-nesting.gram")

  let groupReferences reg =
        maybe [] rowElements (Map.lookup (Named (Subject.Symbol "graphistas")) reg)
      expectedGroup = [Named (Subject.Symbol n) | n <- ["abk", "ee", "mh", "le", "fh"]]
      isGeneratedIdentity (Named (Subject.Symbol sym)) = take 1 sym == "#"
      isGeneratedIdentity _ = False
      hasNoGeneratedIdentities reg = not (any isGeneratedIdentity (Map.keys reg))
      hasSomePositionalAddress reg = any isPositional (Map.keys reg)
      isPositional (Positional _) = True
      isPositional _ = False

      checks =
        [ ("parses social.gram", either (const False) (const True) social)
        , ("admits social.gram with named group-membership resolved in order", either (const False) ((== expectedGroup) . groupReferences) social)
        , ("parses route-66.gram", either (const False) (const True) route66)
        , ("admits route-66.gram at real document scale (>= 15 registry entries)", either (const False) ((>= 15) . Map.size) route66)
        , ("parses anonymous-subject.gram", either (const False) (const True) anonymousSubject)
        , ("admits anonymous-subject.gram via a positional ordinal", either (const False) hasSomePositionalAddress anonymousSubject)
        , ("admits anonymous-subject.gram with no generated identity", either (const False) hasNoGeneratedIdentities anonymousSubject)
        , ("parses implicit-root.gram", either (const False) (const True) implicitRoot)
        , ("admits implicit-root.gram with closure holding", either (const False) (const True) implicitRoot)
        , ("parses deep-nesting.gram", either (const False) (const True) deepNesting)
        , ("admits deep-nesting.gram with closure holding across 5 levels", either (const False) ((== 5) . Map.size) deepNesting)
        ]

  outcomes <- mapM (uncurry check) checks
  mapM_
    reportRegistry
    [ ("social.gram", social)
    , ("route-66.gram", route66)
    , ("anonymous-subject.gram", anonymousSubject)
    , ("implicit-root.gram", implicitRoot)
    , ("deep-nesting.gram", deepNesting)
    ]
  if and outcomes then pure () else exitFailure
  where
    reportRegistry (label, Left err) = putStrLn ("-- " ++ label ++ ": ADMISSION FAILED: " ++ err)
    reportRegistry (label, Right reg) = putStrLn ("-- " ++ label ++ ": " ++ show (Map.size reg) ++ " registry entries: " ++ show (Map.keys reg))
