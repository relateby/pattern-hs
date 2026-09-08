module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import System.Exit (exitFailure)

import qualified Gram as Gram
import Pattern.Core (Pattern(..))
import qualified Pattern.Reconcile as Reconcile
import qualified Subject.Core as Subject

type LocalIdentity = Subject.Symbol

data PatternLike
  = Definition Subject.Subject [PatternLike]
  | Reference LocalIdentity
  deriving (Eq, Show)

data Member = Member
  { memberSubject :: Subject.Subject
  , memberElements :: [LocalIdentity]
  }
  deriving (Eq, Show)

data Frame = Frame
  { frameSubject :: Subject.Subject
  , frameMembers :: Map LocalIdentity Member
  }
  deriving (Eq, Show)

data Pair = Pair
  { pairSubject :: Subject.Subject
  , pairLeft :: LocalIdentity
  , pairRight :: LocalIdentity
  }
  deriving (Eq, Show)

data Span = Span
  { spanSubject :: Subject.Subject
  , spanLeftFrame :: LocalIdentity
  , spanRightFrame :: LocalIdentity
  , spanPairs :: Map LocalIdentity Pair
  }
  deriving (Eq, Show)

data FrameSpace = FrameSpace
  { spaceFrames :: Map LocalIdentity Frame
  , spaceSpans :: Map LocalIdentity Span
  }
  deriving (Eq, Show)

subject :: String -> Subject.Subject
subject name = Subject.Subject (Subject.Symbol name) Set.empty Map.empty

labelled :: String -> String -> Subject.Subject
labelled name label = Subject.Subject (Subject.Symbol name) (Set.singleton label) Map.empty

identityOf :: Subject.Subject -> LocalIdentity
identityOf = Subject.identity

emptyFrame :: Subject.Subject -> Frame
emptyFrame root = Frame root Map.empty

frameId :: Frame -> LocalIdentity
frameId = identityOf . frameSubject

spanId :: Span -> LocalIdentity
spanId = identityOf . spanSubject

anonymous :: Subject.Subject -> Bool
anonymous candidate = identityOf candidate == Subject.Symbol ""

explicitIds :: PatternLike -> Set LocalIdentity
explicitIds (Reference _) = Set.empty
explicitIds (Definition candidate children)
  | anonymous candidate = Set.unions (map explicitIds children)
  | otherwise = Set.insert (identityOf candidate) (Set.unions (map explicitIds children))

assignAnonymous :: Set LocalIdentity -> Int -> PatternLike -> (PatternLike, Set LocalIdentity, Int)
assignAnonymous used next (Reference ident) = (Reference ident, used, next)
assignAnonymous used next (Definition candidate children) =
  let (assigned, used', next') =
        if anonymous candidate
          then let (fresh, nextFresh) = freshIdentity used next
               in (replaceIdentity fresh candidate, Set.insert fresh used, nextFresh)
          else (candidate, Set.insert (identityOf candidate) used, next)
      (children', used'', next'') = assignAnonymousList used' next' children
  in (Definition assigned children', used'', next'')

assignAnonymousList :: Set LocalIdentity -> Int -> [PatternLike] -> ([PatternLike], Set LocalIdentity, Int)
assignAnonymousList used next [] = ([], used, next)
assignAnonymousList used next (entry:rest) =
  let (entry', used', next') = assignAnonymous used next entry
      (rest', used'', next'') = assignAnonymousList used' next' rest
  in (entry' : rest', used'', next'')

freshIdentity :: Set LocalIdentity -> Int -> (LocalIdentity, Int)
freshIdentity used next =
  let candidate = Subject.Symbol ("#spike-" ++ show next)
  in if Set.member candidate used
       then freshIdentity used (next + 1)
       else (candidate, next + 1)

replaceIdentity :: LocalIdentity -> Subject.Subject -> Subject.Subject
replaceIdentity ident candidate = candidate { Subject.identity = ident }

flatten :: PatternLike -> [(LocalIdentity, Member)]
flatten (Reference _) = []
flatten (Definition candidate children) =
  let ident = identityOf candidate
      childIds = map rootIdentity children
  in (ident, Member candidate childIds) : concatMap flatten children

rootIdentity :: PatternLike -> LocalIdentity
rootIdentity (Reference ident) = ident
rootIdentity (Definition candidate _) = identityOf candidate

admit :: Frame -> [PatternLike] -> Either String Frame
admit frame entries = do
  let used = Set.union (Map.keysSet (frameMembers frame)) (Set.unions (map explicitIds entries))
      (assignedEntries, _, _) = assignAnonymousList used 1 entries
      rows = concatMap flatten assignedEntries
      ids = map fst rows
      incoming = Map.fromList rows
      merged = Map.union incoming (frameMembers frame)
  if hasDuplicates ids
    then Left "duplicate definition in admission batch"
    else if any (`Map.member` frameMembers frame) ids
      then Left "existing member requires reconciliation, not admission"
      else validateMembers merged (Map.elems incoming) >> Right frame { frameMembers = merged }

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates values = Set.size (Set.fromList values) /= length values

validateMembers :: Map LocalIdentity Member -> [Member] -> Either String ()
validateMembers registry = mapM_ validate
  where
    validate member
      | identityOf (memberSubject member) `elem` memberElements member = Left "direct self-reference"
      | any (`Map.notMember` registry) (memberElements member) = Left "unresolved local reference"
      | otherwise = Right ()

rawImport :: Pattern Subject.Subject -> Either String ()
rawImport raw
  | any ambiguous (Map.elems grouped) = Left "raw Pattern import is ambiguous"
  | otherwise = Right ()
  where
    grouped = Map.fromListWith combine (facts raw)
    facts (Pattern candidate children) =
      (identityOf candidate, (null children, not (null children))) : concatMap facts children
    combine (atomicA, fullA) (atomicB, fullB) = (atomicA || atomicB, fullA || fullB)
    ambiguous (hasAtomic, hasFull) = hasAtomic && hasFull

validateSpan :: Map LocalIdentity Frame -> Span -> Either String ()
validateSpan frames span = do
  left <- maybe (Left "left Frame is absent") Right (Map.lookup (spanLeftFrame span) frames)
  right <- maybe (Left "right Frame is absent") Right (Map.lookup (spanRightFrame span) frames)
  mapM_ (validatePair left right) (Map.elems (spanPairs span))
  where
    validatePair left right pair
      | anonymous (pairSubject pair) = Left "pair identity is anonymous"
      | Map.notMember (pairLeft pair) (frameMembers left) = Left "pair left endpoint is absent"
      | Map.notMember (pairRight pair) (frameMembers right) = Left "pair right endpoint is absent"
      | otherwise = Right ()

addSpan :: Span -> FrameSpace -> Either String FrameSpace
addSpan span space
  | Map.member (spanId span) (spaceSpans space) = Left "Span identity already exists"
  | otherwise = validateSpan (spaceFrames space) span >> Right space
      { spaceSpans = Map.insert (spanId span) span (spaceSpans space) }

replaceFrame :: Frame -> FrameSpace -> Either String FrameSpace
replaceFrame replacement space
  | Map.notMember (frameId replacement) (spaceFrames space) = Left "Frame identity is absent"
  | otherwise =
      let frames' = Map.insert (frameId replacement) replacement (spaceFrames space)
      in mapM_ (validateSpan frames') (Map.elems (spaceSpans space)) >> Right space { spaceFrames = frames' }

removeMember :: LocalIdentity -> Frame -> Either String Frame
removeMember ident frame
  | Map.notMember ident (frameMembers frame) = Left "member is absent"
  | any (elem ident . memberElements) (Map.elems (frameMembers frame)) = Left "member has local references"
  | otherwise = Right frame { frameMembers = Map.delete ident (frameMembers frame) }

rebindPair :: LocalIdentity -> LocalIdentity -> LocalIdentity -> LocalIdentity -> FrameSpace -> Either String FrameSpace
rebindPair targetSpan targetPair left right space = do
  span <- maybe (Left "Span is absent") Right (Map.lookup targetSpan (spaceSpans space))
  pair <- maybe (Left "pair is absent") Right (Map.lookup targetPair (spanPairs span))
  let pair' = pair { pairLeft = left, pairRight = right }
      span' = span { spanPairs = Map.insert targetPair pair' (spanPairs span) }
  validateSpan (spaceFrames space) span'
  Right space { spaceSpans = Map.insert targetSpan span' (spaceSpans space) }

closure :: Frame -> [LocalIdentity] -> Either String [LocalIdentity]
closure frame roots = fmap reverse (foldl' visit (Right []) roots)
  where
    visit result ident = do
      seen <- result
      if ident `elem` seen then Right seen else do
        member <- maybe (Left "closure root is absent") Right (Map.lookup ident (frameMembers frame))
        foldl' visit (Right (ident : seen)) (memberElements member)

importSubgraph :: Frame -> [LocalIdentity] -> Map LocalIdentity LocalIdentity -> Frame -> Either String Frame
importSubgraph source roots remap destination = do
  sourceIds <- closure source roots
  let target ident = Map.findWithDefault ident ident remap
      targetIds = map target sourceIds
  if hasDuplicates targetIds
    then Left "import map is not injective"
    else if any (`Map.member` frameMembers destination) targetIds
      then Left "import collides with destination identity"
      else do
        imported <- mapM (copyMember target) sourceIds
        let destination' = destination { frameMembers = Map.union (Map.fromList imported) (frameMembers destination) }
        validateMembers (frameMembers destination') (map snd imported)
        Right destination'
  where
    copyMember target ident = do
      member <- maybe (Left "source member is absent") Right (Map.lookup ident (frameMembers source))
      let copiedSubject = replaceIdentity (target ident) (memberSubject member)
      Right (target ident, Member copiedSubject (map target (memberElements member)))

attach :: LocalIdentity -> [LocalIdentity] -> Frame -> Either String Frame
attach target roots frame = do
  member <- maybe (Left "attachment target is absent") Right (Map.lookup target (frameMembers frame))
  let member' = member { memberElements = memberElements member ++ roots }
      frame' = frame { frameMembers = Map.insert target member' (frameMembers frame) }
  validateMembers (frameMembers frame') [member']
  Right frame'

check :: String -> Bool -> IO Bool
check label passed = do
  putStrLn ((if passed then "PASS " else "FAIL ") ++ label)
  pure passed

parseFixture :: FilePath -> IO Bool
parseFixture path = do
  source <- readFile path
  pure $ case Gram.fromGramWithIds source of
    Left _ -> False
    Right patterns -> not (null patterns)

main :: IO ()
main = do
  parsedAircraft <- parseFixture "design/spikes/SPIKE-001-frame-registry/fixtures/aircraft-maintenance.gram"
  parsedAmbiguous <- parseFixture "design/spikes/SPIKE-001-frame-registry/fixtures/ambiguous-reference.gram"
  parsedRepair <- parseFixture "design/spikes/SPIKE-001-frame-registry/fixtures/repair-plan-collision.gram"

  let aircraftInput =
        [ Definition (subject "engine") [Reference (Subject.Symbol "fuel-system")]
        , Definition (subject "fuel-system") [Reference (Subject.Symbol "fuel-pump")]
        , Definition (subject "fuel-pump") [Reference (Subject.Symbol "diagnostic-procedure")]
        , Definition (subject "diagnostic-procedure") [Reference (Subject.Symbol "engine")]
        , Definition (subject "sensor") []
        ]
      maintenanceInput =
        [ Definition (subject "inspect-engine") []
        , Definition (subject "inspect-fuel") []
        ]
      anonymousInput = [Definition (subject "") []]
      Right aircraft = admit (emptyFrame (subject "aircraft-17")) aircraftInput
      Right maintenance = admit (emptyFrame (subject "maintenance-17")) maintenanceInput
      Right anonymousFrame = admit (emptyFrame (subject "anonymous-17")) anonymousInput
      initialSpace = FrameSpace
        (Map.fromList [(frameId aircraft, aircraft), (frameId maintenance, maintenance)])
        Map.empty
      pairEngine = Pair (labelled "pair-engine" "Summarizes") (Subject.Symbol "engine") (Subject.Symbol "inspect-engine")
      pairSensor = Pair (labelled "pair-sensor" "Summarizes") (Subject.Symbol "sensor") (Subject.Symbol "inspect-fuel")
      maintenanceSpan = Span (subject "aircraft-maintenance") (frameId aircraft) (frameId maintenance)
        (Map.fromList [(identityOf (pairSubject pairEngine), pairEngine), (identityOf (pairSubject pairSensor), pairSensor)])
      Right spaceWithSpan = addSpan maintenanceSpan initialSpace
      rawAmbiguousPattern = Pattern (subject "root")
        [ Pattern (labelled "engine" "Engine") [Pattern (subject "sensor") []]
        , Pattern (subject "engine") []
        ]
      reconcileInput = Pattern (subject "root")
        [ Pattern (labelled "fuel-pump" "Pump") []
        , Pattern (labelled "fuel-pump" "Critical") []
        ]
      reconcileResult = Reconcile.reconcile
        (Reconcile.Merge Reconcile.UnionElements Reconcile.defaultSubjectMergeStrategy)
        reconcileInput
      replacePolicyInput = Pattern (subject "root")
        [ Pattern (labelled "engine" "First") [Pattern (subject "first-child") []]
        , Pattern (labelled "engine" "Last") [Pattern (subject "last-child") []]
        ]
      lastWriteResult = Reconcile.reconcile Reconcile.LastWriteWins replacePolicyInput
      lastWriteUnionsElements = case lastWriteResult of
        Right (Pattern _ [Pattern engine children]) ->
          Set.member "Last" (Subject.labels engine)
            && map (identityOf . value) children == [Subject.Symbol "first-child", Subject.Symbol "last-child"]
        _ -> False
      Right sensorRemoved = removeMember (Subject.Symbol "sensor") aircraft
      crossDeletionRejected = case replaceFrame sensorRemoved spaceWithSpan of
        Left _ -> True
        Right _ -> False
      Right reboundSpace = rebindPair (Subject.Symbol "aircraft-maintenance") (Subject.Symbol "pair-sensor")
        (Subject.Symbol "engine") (Subject.Symbol "inspect-fuel") spaceWithSpan
      crossDeletionAllowed = case replaceFrame sensorRemoved reboundSpace of
        Right _ -> True
        Left _ -> False
      Right repairBase = admit (emptyFrame (subject "repair-plan-17"))
        [ Definition (subject "work-order") []
        , Definition (labelled "engine" "ExistingRepairEngine") []
        ]
      collisionRejected = case importSubgraph aircraft [Subject.Symbol "engine"] Map.empty repairBase of
        Left _ -> True
        Right _ -> False
      renameMap = Map.fromList
        [ (Subject.Symbol "engine", Subject.Symbol "imported-engine")
        , (Subject.Symbol "fuel-system", Subject.Symbol "imported-fuel-system")
        , (Subject.Symbol "fuel-pump", Subject.Symbol "imported-fuel-pump")
        , (Subject.Symbol "diagnostic-procedure", Subject.Symbol "imported-diagnostic-procedure")
        ]
      importedRepair = importSubgraph aircraft [Subject.Symbol "engine"] renameMap repairBase
      attachedRepair = importedRepair >>= attach (Subject.Symbol "work-order") [Subject.Symbol "imported-engine"]
      invalidSpan = Span (subject "invalid-span") (frameId aircraft) (frameId maintenance)
        (Map.singleton (Subject.Symbol "pair-invalid")
          (Pair (subject "pair-invalid") (Subject.Symbol "missing") (Subject.Symbol "inspect-engine")))
      invalidSpanRejected = case addSpan invalidSpan initialSpace of
        Left _ -> True
        Right _ -> False
      checks =
        [ ("parses aircraft/maintenance Gram fixture", parsedAircraft)
        , ("parses ambiguous-reference Gram fixture", parsedAmbiguous)
        , ("parses repair-plan collision Gram fixture", parsedRepair)
        , ("flattens nested definitions into one Frame namespace", Map.size (frameMembers aircraft) == 5)
        , ("accepts indirect cycles without recursive copies", either (const False) (elem (Subject.Symbol "engine")) (closure aircraft [Subject.Symbol "engine"]))
        , ("assigns a stable local identity to an anonymous definition", Map.member (Subject.Symbol "#spike-1") (frameMembers anonymousFrame))
        , ("rejects a direct self-reference", case admit (emptyFrame (subject "self-17")) [Definition (subject "self") [Reference (Subject.Symbol "self")]] of Left _ -> True; Right _ -> False)
        , ("fails ambiguous raw Pattern compatibility import", case rawImport rawAmbiguousPattern of Left _ -> True; Right _ -> False)
        , ("runs real Pattern.Reconcile Merge policy", either (const False) (const True) reconcileResult)
        , ("observes LastWriteWins subject replacement with element union", lastWriteUnionsElements)
        , ("rejects a Span pair whose endpoint does not resolve", invalidSpanRejected)
        , ("rejects local deletion while a member is referenced", case removeMember (Subject.Symbol "fuel-system") aircraft of Left _ -> True; Right _ -> False)
        , ("rejects FrameSpace replacement that leaves a pair endpoint dangling", crossDeletionRejected)
        , ("allows replacement after explicit pair rebind", crossDeletionAllowed)
        , ("rejects cross-Frame import identity collisions by default", collisionRejected)
        , ("imports a reference closure with explicit local rebase", either (const False) (Map.member (Subject.Symbol "imported-engine") . frameMembers) importedRepair)
        , ("attaches an imported root after Frame-local validation", either (const False) (elem (Subject.Symbol "imported-engine") . memberElements . (Map.! Subject.Symbol "work-order") . frameMembers) attachedRepair)
        ]
  outcomes <- mapM (uncurry check) checks
  if and outcomes then pure () else exitFailure
