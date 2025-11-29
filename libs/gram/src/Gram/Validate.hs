{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Gram.Validate 
  ( SymbolTable
  , SymbolInfo(..)
  , SymbolType(..)
  , DefinitionStatus(..)
  , PatternSignature(..)
  , ValidationEnv(..)
  , ValidationError(..)
  , validate
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (when)

import Gram.CST (Gram(..), AnnotatedPattern(..), PatternElement(..), Path(..), PathSegment(..), Node(..), Relationship(..), SubjectPattern(..), SubjectData(..), Identifier(..), Symbol(..))

-- | The internal state used during validation.
type SymbolTable = Map Identifier SymbolInfo

data SymbolInfo = SymbolInfo
  { symType :: SymbolType
  , symStatus :: DefinitionStatus
  , symSignature :: Maybe PatternSignature
  } deriving (Show, Eq)

data SymbolType
  = TypeNode
  | TypeRelationship
  | TypePattern
  | TypeUnknown
  deriving (Show, Eq)

data DefinitionStatus
  = StatusDefined
  | StatusReferenced
  | StatusImplicit
  deriving (Show, Eq)

data PatternSignature = PatternSignature
  { sigLabels :: Set String
  , sigArity :: Int
  , sigEndpoints :: Maybe (Maybe Identifier, Maybe Identifier) -- (source, target) for relationships
  } deriving (Show, Eq)

data ValidationEnv = ValidationEnv
  { envCurrentPath :: [Identifier] -- For cycle detection
  } deriving (Show, Eq)

data ValidationError
  = DuplicateDefinition Identifier
  | UndefinedReference Identifier
  | SelfReference Identifier
  | InconsistentDefinition Identifier String
  | ImmutabilityViolation Identifier
  deriving (Show, Eq)

type ValidationState = (SymbolTable, [ValidationError])
type ValidateM a = State ValidationEnv a

-- | Initial state
emptySymbolTable :: SymbolTable
emptySymbolTable = Map.empty

emptyEnv :: ValidationEnv
emptyEnv = ValidationEnv []

-- | Validate a parsed Gram AST.
validate :: Gram -> Either [ValidationError] ()
validate (Gram _ patterns) = 
  let (_, errs) = execState (validatePatterns patterns) (emptySymbolTable, [])
  in if null errs then Right () else Left (reverse errs)

-- | Main validation loop state
-- State: (SymbolTable, [ValidationError])
validatePatterns :: [AnnotatedPattern] -> State ValidationState ()
validatePatterns pats = do
  -- Pass 1: Register all definitions
  mapM_ registerDefinition pats
  -- Pass 2: Check references and consistency
  mapM_ checkReferences pats

-- | Register definitions
registerDefinition :: AnnotatedPattern -> State ValidationState ()
registerDefinition (AnnotatedPattern _ elements) = 
  mapM_ registerElement elements

registerElement :: PatternElement -> State ValidationState ()
registerElement (PESubjectPattern sp) = registerSubjectPattern sp
registerElement (PEPath path) = registerPath path
registerElement (PEReference _) = return () -- References don't define

registerSubjectPattern :: SubjectPattern -> State ValidationState ()
registerSubjectPattern (SubjectPattern maybeSubj elements) = do
  -- Register the subject itself if identified
  let arity = length elements
  case maybeSubj of
    Just (SubjectData (Just ident) labels _) -> do
      (syms, errs) <- get
      case Map.lookup ident syms of
        Just info | symStatus info == StatusDefined -> 
          put (syms, DuplicateDefinition ident : errs)
        _ -> do
          -- We define it here with its signature (no endpoints for pattern notation)
          let sig = PatternSignature labels arity Nothing
          let info = SymbolInfo TypePattern StatusDefined (Just sig)
          put (Map.insert ident info syms, errs)
    _ -> return ()
  
  -- Recurse into elements
  mapM_ registerElement elements

registerPath :: Path -> State ValidationState ()
registerPath (Path start segments) = do
  registerNode start
  let sourceId = getNodeIdentifier start
  registerPathSegments sourceId segments

-- | Extract the identifier from a node, if present.
-- Returns Nothing for anonymous nodes.
getNodeIdentifier :: Node -> Maybe Identifier
getNodeIdentifier (Node (Just (SubjectData (Just ident) _ _))) = Just ident
getNodeIdentifier _ = Nothing

-- | Register path segments while tracking node identifiers for relationship endpoint validation.
-- The sourceId parameter is the identifier of the preceding node.
-- For each segment, we extract the target node identifier and pass both to registerRelationship,
-- allowing us to detect when a relationship identifier is reused with different endpoints.
registerPathSegments :: Maybe Identifier -> [PathSegment] -> State ValidationState ()
registerPathSegments _ [] = return ()
registerPathSegments sourceId (PathSegment rel nextNode : rest) = do
  let targetId = getNodeIdentifier nextNode
  registerRelationship rel sourceId targetId
  registerNode nextNode
  registerPathSegments targetId rest

registerNode :: Node -> State ValidationState ()
registerNode (Node (Just (SubjectData (Just ident) _ _))) = do
  (syms, errs) <- get
  case Map.lookup ident syms of
    Just info | symStatus info == StatusDefined -> return () 
    _ -> do
      let info = SymbolInfo TypeNode StatusDefined Nothing
      put (Map.insert ident info syms, errs)
registerNode _ = return () 

registerRelationship :: Relationship -> Maybe Identifier -> Maybe Identifier -> State ValidationState ()
registerRelationship (Relationship _ (Just (SubjectData (Just ident) _ _))) sourceId targetId = do
  (syms, errs) <- get
  let endpoints = (sourceId, targetId)
  case Map.lookup ident syms of
    Just info | symStatus info == StatusDefined -> 
      case symType info of
        TypeRelationship -> 
          -- Check if the endpoints match the original definition
          case symSignature info of
            Just (PatternSignature _ _ (Just existingEndpoints)) ->
              if existingEndpoints == endpoints
                then return () -- Same endpoints, this is a valid reference
                else put (syms, DuplicateDefinition ident : errs) -- Different endpoints, redefinition
            _ -> return () -- No endpoints stored, allow
        TypePattern -> 
          -- Defined via pattern notation, allow if arity is consistent with path usage
          case symSignature info of
            Just (PatternSignature _ existingArity _)
              | existingArity == 2 -> return () -- Arity matches, path usage is consistent
              | otherwise -> put (syms, InconsistentDefinition ident ("Expected arity 2 but got " ++ show existingArity) : errs)
            Nothing -> return () -- No signature to check, allow usage
        _ -> 
          -- Other types (TypeNode, TypeUnknown) - allow if arity matches
          case symSignature info of
            Just (PatternSignature _ existingArity _)
              | existingArity == 2 -> return ()
              | otherwise -> put (syms, InconsistentDefinition ident ("Expected arity 2 but got " ++ show existingArity) : errs)
            Nothing -> return ()
    _ -> do
      -- A relationship in a path (a)-[r]->(b) is implicitly arity 2 (source, target)
      let sig = PatternSignature Set.empty 2 (Just endpoints)
      let info = SymbolInfo TypeRelationship StatusDefined (Just sig)
      put (Map.insert ident info syms, errs)
registerRelationship _ _ _ = return ()

-- | Check references and consistency
checkReferences :: AnnotatedPattern -> State ValidationState ()
checkReferences (AnnotatedPattern _ elements) = 
  mapM_ checkElement elements

checkElement :: PatternElement -> State ValidationState ()
checkElement (PESubjectPattern sp) = checkSubjectPattern sp
checkElement (PEPath path) = checkPath path
checkElement (PEReference ident) = checkIdentifierRef ident Nothing

checkSubjectPattern :: SubjectPattern -> State ValidationState ()
checkSubjectPattern (SubjectPattern maybeSubj elements) = do
  case maybeSubj of
    Just (SubjectData (Just ident) _ _) -> do
      let directRefs = [id | PEReference id <- elements]
      when (ident `elem` directRefs) $ do
        (syms, errs) <- get
        put (syms, SelfReference ident : errs)
    _ -> return ()

  mapM_ checkElement elements

checkPath :: Path -> State ValidationState ()
checkPath (Path start segments) = do
  checkNode start
  mapM_ checkSegment segments

checkSegment :: PathSegment -> State ValidationState ()
checkSegment (PathSegment rel nextNode) = do
  checkRelationship rel
  checkNode nextNode

checkNode :: Node -> State ValidationState ()
checkNode (Node (Just (SubjectData (Just ident) _ _))) = checkIdentifierRef ident Nothing
checkNode _ = return ()

checkRelationship :: Relationship -> State ValidationState ()
checkRelationship (Relationship _ (Just (SubjectData (Just ident) _ _))) = 
  -- Relationships in paths imply arity 2.
  checkIdentifierRef ident (Just 2)
checkRelationship _ = return ()

checkIdentifierRef :: Identifier -> Maybe Int -> State ValidationState ()
checkIdentifierRef ident expectedArity = do
  (syms, errs) <- get
  case Map.lookup ident syms of
    Just info -> do
      -- Check consistency if we have an expected arity
      case (expectedArity, symSignature info) of
        (Just expected, Just (PatternSignature _ actual _)) 
          | expected /= actual -> 
            put (syms, InconsistentDefinition ident ("Expected arity " ++ show expected ++ " but got " ++ show actual) : errs)
        _ -> return ()
    Nothing -> put (syms, UndefinedReference ident : errs)
