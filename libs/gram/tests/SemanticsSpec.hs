{-# LANGUAGE OverloadedStrings #-}
module SemanticsSpec (spec) where

import Test.Hspec
import Gram.Validate
import Gram.Parse (parseGram)
-- import Gram.CST (Identifier(..), Symbol(..)) -- removed unused imports
import Data.Either (isLeft, isRight)
import Text.Megaparsec (parse)

-- Helper to parse and validate
validateSource :: String -> Either [ValidationError] ()
validateSource input = 
  case parse parseGram "test" input of
    Left _ -> Left [] -- Should not happen in these tests
    Right gram -> validate gram

-- Helper to extract error type
isDuplicateDefinition :: ValidationError -> Bool
isDuplicateDefinition (DuplicateDefinition _) = True
isDuplicateDefinition _ = False

isUndefinedReference :: ValidationError -> Bool
isUndefinedReference (UndefinedReference _) = True
isUndefinedReference _ = False

isSelfReference :: ValidationError -> Bool
isSelfReference (SelfReference _) = True
isSelfReference _ = False

isInconsistentDefinition :: ValidationError -> Bool
isInconsistentDefinition (InconsistentDefinition _ _) = True
isInconsistentDefinition _ = False

spec :: Spec
spec = do
  describe "Basic Pattern Validation" $ do
    it "accepts a single valid definition" $ do
      validateSource "[a]" `shouldSatisfy` isRight

    it "accepts multiple unique definitions" $ do
      validateSource "[a], [b]" `shouldSatisfy` isRight

    it "rejects duplicate definitions" $ do
      let result = validateSource "[a], [a]"
      result `shouldSatisfy` isLeft
      case result of
        Left [err] -> err `shouldSatisfy` isDuplicateDefinition
        _ -> expectationFailure "Expected single DuplicateDefinition error"

    it "accepts forward references" $ do
      validateSource "[b | a], [a]" `shouldSatisfy` isRight
    
    it "accepts backward references" $ do
      validateSource "[a], [b | a]" `shouldSatisfy` isRight

    it "rejects undefined references" $ do
      let result = validateSource "[a | b]"
      result `shouldSatisfy` isLeft
      case result of
        Left [err] -> err `shouldSatisfy` isUndefinedReference
        _ -> expectationFailure "Expected single UndefinedReference error"

    it "rejects direct self-reference" $ do
      let result = validateSource "[a | a]"
      result `shouldSatisfy` isLeft
      case result of
        Left [err] -> err `shouldSatisfy` isSelfReference
        _ -> expectationFailure "Expected single SelfReference error"

    it "accepts indirect cycles" $ do
      validateSource "[a | b], [b | a]" `shouldSatisfy` isRight

  describe "Path Notation Validation" $ do
    it "accepts a simple path" $ do
      validateSource "(a)-[r]->(b)" `shouldSatisfy` isRight

    it "defines elements in a path" $ do
      -- (a) defines 'a', so [p | a] should be valid
      validateSource "(a)-[r]->(b), [p | a]" `shouldSatisfy` isRight

    it "rejects redefinition of path elements" $ do
      -- 'r' is defined in first path, cannot be redefined in second with different structure
      -- Note: This depends on strict consistency checks. 
      -- For now, just checking duplicates if they are treated as pattern definitions.
      -- If (a)-[r]->(b) implies [r | a, b], then a second identical path is fine?
      -- No, identified relationships must be unique unless they are references.
      -- But in path notation, identifiers are often used for uniqueness.
      -- Let's assume standard redefinition rule applies: r is defined once.
      -- If we write (a)-[r]->(b) and then (c)-[r]->(d), 'r' is duplicated?
      -- Yes, if 'r' is an identifier.
      let result = validateSource "(a)-[r]->(b), (c)-[r]->(d)"
      result `shouldSatisfy` isLeft
      case result of
        Left [err] -> err `shouldSatisfy` isDuplicateDefinition
        _ -> expectationFailure "Expected DuplicateDefinition error for reused relationship identifier"

    it "accepts anonymous relationships" $ do
      validateSource "(a)-[:knows]->(b), (a)-[:knows]->(b)" `shouldSatisfy` isRight

    it "accepts relationship reuse with same endpoints" $ do
      -- Valid: (a)-[r]->(b) followed by (a)-[r]->(b)-[r2]->(c)
      -- The relationship r connects (a, b) in both cases
      validateSource "(a)-[r]->(b), (a)-[r]->(b)-[r2]->(c)" `shouldSatisfy` isRight

    it "rejects relationship reuse with different endpoints" $ do
      -- Invalid: (a)-[r]->(b) followed by (a)-[r]->(c)-[r2]->(b)
      -- The relationship r connects (a, b) first, then (a, c)
      let result = validateSource "(a)-[r]->(b), (a)-[r]->(c)-[r2]->(b)"
      result `shouldSatisfy` isLeft
      case result of
        Left [err] -> err `shouldSatisfy` isDuplicateDefinition
        _ -> expectationFailure "Expected DuplicateDefinition error for relationship connecting different nodes"

    it "accepts node reuse in cycles" $ do
      -- Valid cycle: a appears twice but with no redefinition
      validateSource "(a)-[r1]->(b)<-[r2]-(a)" `shouldSatisfy` isRight

    it "accepts node reuse across separate paths" $ do
      -- (a), (a) is valid - second is a reference to the first
      validateSource "(a), (a)" `shouldSatisfy` isRight

  describe "Mixed Notation Consistency" $ do
    it "accepts consistent definition and usage" $ do
      validateSource "[r | a, b], (a)-[r]->(b)" `shouldSatisfy` isRight

    it "rejects inconsistent arity (structure mismatch)" $ do
      -- [r | a, b, c] has 3 elements. (a)-[r]->(b) implies 2 elements.
      -- This requires Arity check.
      -- Note: We also need (c) to define 'c', otherwise it's an undefined reference.
      let result = validateSource "[r | a, b, c], (a)-[r]->(b), (c)"
      result `shouldSatisfy` isLeft 
      case result of
        Left errs -> any isInconsistentDefinition errs `shouldBe` True
        _ -> expectationFailure "Expected InconsistentDefinition error"
