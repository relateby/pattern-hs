-- | Minimal reproduction case for range parsing issue.
-- 
-- This test isolates the range parsing pattern to determine if the issue
-- is with Megaparsec or our usage, similar to the parsePropertyRecord investigation.
{-# LANGUAGE OverloadedStrings #-}
module Spec.Gram.ParseRangeRepro where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Data.Char (isAlphaNum)

type Parser = Parsec Void String

-- Minimal whitespace parser
optionalSpace :: Parser ()
optionalSpace = void $ many (char ' ' <|> char '\t')

-- Minimal range parser (matches the real implementation)
parseRange :: Parser String
parseRange = do
  -- Try to parse lower bound (optional)
  -- First check if we start with dots (for ...10 case)
  startsWithDots <- lookAhead (optional (try (string "..")))
  lower <- if startsWithDots == Just ".."
    then return Nothing  -- We start with dots, so no lower bound
    else do
      -- Parse the integer part first (without decimal, to avoid consuming the first dot)
      -- Then check if dots follow to confirm this is a range
      sign <- optional (char '-')
      intPart <- some digitChar
      -- Check if dots follow - if not, this is not a range, so fail
      hasDots <- lookAhead (optional (try (string "..")))
      if hasDots == Just ".."
        then do
          -- Dots follow, so this is a range - use the integer part
          let num = read intPart :: Double
          let numWithSign = if sign == Just '-' then -num else num
          return (Just numWithSign)
        else fail "not a range"  -- No dots, fail
  -- Parse dots: ".." (closed) or "..." (open on one or both sides)
  firstDot <- char '.'
  secondDot <- char '.'
  hasThirdDot <- optional (char '.')
  if hasThirdDot == Just '.'
    then do
      -- Three dots: "..." - upper bound is optional
      upper <- optional (try parseRangeDouble)
      return $ maybe "" show lower ++ "..." ++ maybe "" show upper
    else do
      -- Two dots: ".." - upper bound is required if lower is present, optional if not
      upper <- if lower == Nothing
        then optional (try parseRangeDouble)
        else Just <$> parseRangeDouble
      return $ maybe "" show lower ++ ".." ++ maybe "" show upper
  where
    parseRangeDouble = do
      sign <- optional (char '-')
      intPart <- some digitChar
      fracPart <- optional (char '.' >> some digitChar)
      let numStr = intPart ++ maybe "" ('.' :) fracPart
      let num = read numStr :: Double
      return $ if sign == Just '-' then -num else num

-- Simulate parseValue with alternatives (like in the real code)
parseValueWithAlternatives :: Parser String
parseValueWithAlternatives = 
  try parseRange <|>
  try (do
    num <- some digitChar
    return $ "integer:" ++ num) <|>
  (do
    sym <- many (satisfy (\c -> isAlphaNum c || c == '_'))
    return $ "symbol:" ++ sym)

-- Simulate parsing a property value in a record (with whitespace)
parsePropertyValue :: Parser String
parsePropertyValue = do
  optionalSpace
  value <- parseValueWithAlternatives
  optionalSpace
  return value

-- Test function - test parseRange in isolation
testParseRange :: String -> Either String String
testParseRange input = case parse (parseRange <* eof) "" input of
  Left err -> Left (errorBundlePretty err)
  Right result -> Right result

-- Test function - test parseRange in context of alternatives (like parseValue)
testParseValue :: String -> Either String String
testParseValue input = case parse (parseValueWithAlternatives <* eof) "" input of
  Left err -> Left (errorBundlePretty err)
  Right result -> Right result

-- Test function - test parseRange in context of property value parsing
testParsePropertyValue :: String -> Either String String
testParsePropertyValue input = case parse (parsePropertyValue <* eof) "" input of
  Left err -> Left (errorBundlePretty err)
  Right result -> Right result

spec :: Spec
spec = do
  describe "Minimal reproduction case for range parsing issue" $ do
    
    describe "parseRange - individual tests (isolated)" $ do
      it "parses closed range (test 1)" $ do
        case testParseRange "1..10" of
          Right result -> result `shouldBe` "1.0..10.0"
          Left err -> expectationFailure $ "Parse failed: " ++ err
      
      it "parses closed range (test 2)" $ do
        case testParseRange "1..10" of
          Right result -> result `shouldBe` "1.0..10.0"
          Left err -> expectationFailure $ "Parse failed: " ++ err
      
      it "parses closed range (test 3)" $ do
        case testParseRange "1..10" of
          Right result -> result `shouldBe` "1.0..10.0"
          Left err -> expectationFailure $ "Parse failed: " ++ err
    
    describe "parseRange - in context of parseValue alternatives" $ do
      it "parses closed range through parseValue (test 1)" $ do
        case testParseValue "1..10" of
          Right result -> result `shouldBe` "1.0..10.0"
          Left err -> expectationFailure $ "Parse failed: " ++ err
      
      it "parses closed range through parseValue (test 2)" $ do
        case testParseValue "1..10" of
          Right result -> result `shouldBe` "1.0..10.0"
          Left err -> expectationFailure $ "Parse failed: " ++ err
      
      it "parses closed range through parseValue (test 3)" $ do
        case testParseValue "1..10" of
          Right result -> result `shouldBe` "1.0..10.0"
          Left err -> expectationFailure $ "Parse failed: " ++ err
    
    describe "parseRange - sequential stress test (isolated)" $ do
      -- Run the same test many times to simulate full test suite
      it "stress test: 50 sequential parses" $ do
        let testInput = "1..10"
        let results = map (const (testParseRange testInput)) [1..50]
        let failures = filter (either (const True) (const False)) results
        case failures of
          [] -> return ()  -- All passed
          (Left err:_) -> expectationFailure $ 
            "Parse failed after multiple runs: " ++ err
          _ -> expectationFailure "Unexpected failure pattern"
    
    describe "parseRange - sequential stress test (in context)" $ do
      -- Run the same test many times to simulate full test suite
      it "stress test: 50 sequential parses through parseValue" $ do
        let testInput = "1..10"
        let results = map (const (testParseValue testInput)) [1..50]
        let failures = filter (either (const True) (const False)) results
        case failures of
          [] -> return ()  -- All passed
          (Left err:_) -> expectationFailure $ 
            "Parse failed after multiple runs: " ++ err
          _ -> expectationFailure "Unexpected failure pattern"
    
    describe "parseRange - different inputs sequentially" $ do
      -- Test with different inputs to see if state accumulates
      it "parses multiple different ranges sequentially (isolated)" $ do
        let inputs = 
              [ "1..10"
              , "1..."
              , "...10"
              , "5..20"
              , "0..1"
              ]
        let results = map testParseRange inputs
        let failures = filter (either (const True) (const False)) results
        case failures of
          [] -> return ()  -- All passed
          (Left err:_) -> expectationFailure $ 
            "Parse failed with different inputs: " ++ err
          _ -> expectationFailure "Unexpected failure pattern"
      
      it "parses multiple different ranges sequentially (in context)" $ do
        let inputs = 
              [ "1..10"
              , "1..."
              , "...10"
              , "5..20"
              , "0..1"
              ]
        let results = map testParseValue inputs
        let failures = filter (either (const True) (const False)) results
        case failures of
          [] -> return ()  -- All passed
          (Left err:_) -> expectationFailure $ 
            "Parse failed with different inputs: " ++ err
          _ -> expectationFailure "Unexpected failure pattern"
    
    describe "parseRange - in property value context (with whitespace)" $ do
      it "parses range as property value (test 1)" $ do
        case testParsePropertyValue " 1..10 " of
          Right result -> result `shouldBe` "1.0..10.0"
          Left err -> expectationFailure $ "Parse failed: " ++ err
      
      it "parses range as property value (test 2)" $ do
        case testParsePropertyValue " 1..10 " of
          Right result -> result `shouldBe` "1.0..10.0"
          Left err -> expectationFailure $ "Parse failed: " ++ err
      
      it "stress test: 50 sequential property value parses" $ do
        let testInput = " 1..10 "
        let results = map (const (testParsePropertyValue testInput)) [1..50]
        let failures = filter (either (const True) (const False)) results
        case failures of
          [] -> return ()  -- All passed
          (Left err:_) -> expectationFailure $ 
            "Parse failed after multiple runs: " ++ err
          _ -> expectationFailure "Unexpected failure pattern"

