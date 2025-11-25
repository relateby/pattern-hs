-- | Minimal reproduction case for Megaparsec parsing issue.
-- 
-- This test isolates the pattern: lookAhead + try + parsePropertyRecord
-- to determine if the issue is with Megaparsec or our usage.
{-# LANGUAGE OverloadedStrings #-}
module Spec.Gram.ParseMinimalRepro where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (void)

type Parser = Parsec Void String

-- Minimal whitespace parser
optionalSpace :: Parser ()
optionalSpace = void $ many (char ' ' <|> char '\t')

-- Minimal property record parser (simplified version)
parsePropertyRecord :: Parser (Map String String)
parsePropertyRecord = do
  void $ char '{'
  optionalSpace
  -- Simplified: just parse a single key-value pair
  key <- many (satisfy (\c -> c /= ' ' && c /= ':' && c /= '}'))
  optionalSpace
  void $ char ':'
  optionalSpace
  value <- many (satisfy (\c -> c /= '"' && c /= '}' && c /= ' '))
  optionalSpace
  void $ char '}'
  return $ Map.singleton key value

-- The problematic pattern: lookAhead + try + parsePropertyRecord
-- This matches the pattern in parseNode
parseNodeWithRecord :: Parser (Map String String)
parseNodeWithRecord = do
  void $ char '('
  optionalSpace
  -- This is the pattern that fails when many parsers run sequentially
  nextChar <- lookAhead (optionalSpace >> satisfy (const True))
  props <- if nextChar == '{'
    then do
      -- Record-only case - wrap in try for proper backtracking
      try parsePropertyRecord
    else do
      return Map.empty
  optionalSpace
  void $ char ')'
  return props

-- Test function
testParse :: String -> Either String (Map String String)
testParse input = case parse (parseNodeWithRecord <* eof) "" input of
  Left err -> Left (errorBundlePretty err)
  Right result -> Right result

spec :: Spec
spec = do
  describe "Minimal reproduction case for Megaparsec issue" $ do
    
    describe "parseNodeWithRecord - individual tests" $ do
      it "parses node with record (test 1)" $ do
        case testParse "({ k : v })" of
          Right props -> Map.lookup "k" props `shouldBe` Just "v"
          Left err -> expectationFailure $ "Parse failed: " ++ err
      
      it "parses node with record (test 2)" $ do
        case testParse "({ k : v })" of
          Right props -> Map.lookup "k" props `shouldBe` Just "v"
          Left err -> expectationFailure $ "Parse failed: " ++ err
      
      it "parses node with record (test 3)" $ do
        case testParse "({ k : v })" of
          Right props -> Map.lookup "k" props `shouldBe` Just "v"
          Left err -> expectationFailure $ "Parse failed: " ++ err
    
    describe "parseNodeWithRecord - sequential stress test" $ do
      -- Run the same test many times to simulate full test suite
      it "stress test: 50 sequential parses" $ do
        let testInput = "({ k : v })"
        let results = map (const (testParse testInput)) [1..50]
        let failures = filter (either (const True) (const False)) results
        case failures of
          [] -> return ()  -- All passed
          (Left err:_) -> expectationFailure $ 
            "Parse failed after multiple runs: " ++ err
          _ -> expectationFailure "Unexpected failure pattern"
    
    describe "parseNodeWithRecord - different inputs sequentially" $ do
      -- Test with different inputs to see if state accumulates
      it "parses multiple different records sequentially" $ do
        let inputs = 
              [ "({ a : 1 })"
              , "({ b : 2 })"
              , "({ c : 3 })"
              , "({ d : 4 })"
              , "({ e : 5 })"
              ]
        let results = map testParse inputs
        let failures = filter (either (const True) (const False)) results
        case failures of
          [] -> return ()  -- All passed
          (Left err:_) -> expectationFailure $ 
            "Parse failed with different inputs: " ++ err
          _ -> expectationFailure "Unexpected failure pattern"

