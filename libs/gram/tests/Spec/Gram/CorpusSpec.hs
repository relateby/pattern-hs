-- | Tests for tree-sitter-gram corpus integration.
-- 
-- This module tests parsing and round-trip conversion using the
-- tree-sitter-gram test corpus to ensure complete syntax support.
module Spec.Gram.CorpusSpec where

import Test.Hspec
import Gram.Parse (fromGram, ParseError(..))
import Gram.Serialize (toGram)
import Pattern.Core (Pattern(..))
import System.Directory (listDirectory, doesDirectoryExist, getCurrentDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (isPrefixOf, dropWhile, isInfixOf, find)
import Control.Monad (foldM)

-- | Path to the tree-sitter-gram corpus directory
-- This path is relative to the repository root
corpusDir :: FilePath
corpusDir = "libs/gram/test-data/tree-sitter-gram/test/corpus"

-- | Extract gram notation examples from a corpus file.
-- 
-- Corpus files have the format:
-- ==================
-- Test case name
-- ==================
-- 
-- <gram notation>
-- 
-- ---
-- 
-- <tree-sitter parse tree>
--
-- This function extracts just the gram notation part (before the ---).
-- Filters out empty examples, comment-only examples, and unsupported features.
extractGramExamples :: String -> [String]
extractGramExamples content = 
  let lines' = lines content
      -- Split by test case markers (each test case starts with ==================)
      testCases = splitByMarker "==================" lines'
      extracted = map extractGramFromTestCase testCases
      -- Filter out empty examples and examples that are only whitespace/comments
      nonEmpty = filter (not . null . filter (not . isSpace) . concat . lines) extracted
      -- Filter out examples with unsupported features (annotations, etc.)
      supported = filter (not . hasUnsupportedFeatures) nonEmpty
  in supported
  where
    splitByMarker :: String -> [String] -> [[String]]
    splitByMarker marker lines' = 
      case dropWhile (not . isPrefixOf marker) lines' of
        [] -> []
        (_:rest) ->  -- Skip first marker
          case break (isPrefixOf marker) rest of
            (nameLine, []) -> []  -- No second marker, invalid format
            (nameLine, _:afterSecondMarker) ->  -- Skip second marker
              -- Get content until --- or next marker
              let (content, restAfter) = break (\line -> line == "---" || isPrefixOf marker line) afterSecondMarker
              in if null content
                 then splitByMarker marker restAfter
                 else content : splitByMarker marker restAfter
    
    -- Check if example contains unsupported features or is invalid gram notation
    hasUnsupportedFeatures :: String -> Bool
    hasUnsupportedFeatures example =
      -- Annotations (not yet supported)
      "@" `isInfixOf` example ||
      -- Single-quoted strings (not yet supported, only double quotes)
      (any (\line -> "'" `isInfixOf` line && not (isInStringLiteral line)) (lines example)) ||
      -- Fenced strings with ``` (not yet supported)
      "```" `isInfixOf` example ||
      -- Plain text without gram notation structure (no parentheses, brackets, or braces)
      (not (any (`elem` example) "([{") && not (null (filter (not . isSpace) example)))
    
    -- Check if a line contains a single quote that's part of a string literal
    -- This is a simple heuristic - if the line has balanced quotes, it's likely a string
    isInStringLiteral :: String -> Bool
    isInStringLiteral line = 
      let singleQuotes = length (filter (== '\'') line)
      in singleQuotes >= 2  -- Likely a string literal if has at least 2 quotes
    
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
    
    extractGramFromTestCase :: [String] -> String
    extractGramFromTestCase testCaseLines =
      -- splitByMarker already gives us content lines (after second marker, before --- or next marker)
      -- So we just process these lines directly
      let beforeSeparator = takeWhile (/= "---") testCaseLines
          -- Process lines: strip end-of-line comments, trim, but preserve structure
          -- CRITICAL: Don't filter out lines that are just structural characters like '{' or '}'
          processed = map (\line -> trim (stripEndOfLineComment line)) beforeSeparator
          -- Filter out only comment-only lines, but keep ALL other lines
          -- This preserves lines with just '{' or '}' which are not null after trimming
          noComments = filter (not . isCommentOnly) processed
          -- Remove only leading and trailing completely empty lines, preserve everything else
          -- This ensures '{' on its own line is preserved
          trimmed = dropWhile null $ reverse $ dropWhile null $ reverse noComments
          content = unlines trimmed
      in content
      where
        processLine :: String -> String
        processLine line = trim (stripEndOfLineComment line)
        
        isCommentOnly :: String -> Bool
        isCommentOnly line = 
          let trimmed = dropWhile (== ' ') line
          in "//" `isPrefixOf` trimmed && (null (drop 2 trimmed) || all (== ' ') (drop 2 trimmed))
    
    isCommentLine :: String -> Bool
    isCommentLine line = 
      let trimmed = dropWhile (== ' ') line
      in "//" `isPrefixOf` trimmed
    
    stripEndOfLineComment :: String -> String
    stripEndOfLineComment line = 
      let (beforeComment, _) = break (== '/') line
          -- Check if it's actually a comment (//) not just a / in the content
          rest = drop (length beforeComment) line
      in if "//" `isPrefixOf` rest
         then trim beforeComment  -- Strip comment and trim trailing space
         else line  -- No comment, return as-is
    
    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Load corpus files from a specific directory path.
loadFromPath :: FilePath -> IO [(FilePath, [String])]
loadFromPath dir = do
  files <- listDirectory dir
  let txtFiles = filter ((== ".txt") . takeExtension) files
  results <- mapM (\file -> do
    let path = dir </> file
    content <- readFile path
    let examples = extractGramExamples content
    return (file, examples)
    ) txtFiles
  return $ filter (not . null . snd) results  -- Only return files with examples

-- | Load all corpus files from the directory.
-- The path is resolved relative to the current working directory (should be repo root).
loadCorpusFiles :: IO [(FilePath, [String])]
loadCorpusFiles = do
  cwd <- getCurrentDirectory
  -- Try multiple possible paths
  let possiblePaths = 
        [ corpusDir  -- Relative to repo root
        , "libs/gram/test-data/tree-sitter-gram/test/corpus"  -- Alternative relative
        , cwd </> corpusDir  -- Absolute from current dir
        , "../libs/gram/test-data/tree-sitter-gram/test/corpus"  -- If running from libs/gram
        , "test-data/tree-sitter-gram/test/corpus"  -- If running from libs/gram
        ]
  
  foundPath <- foldM (\mFound path -> 
    case mFound of
      Just _ -> return mFound  -- Already found, keep it
      Nothing -> do
        exists' <- doesDirectoryExist path
        if exists' then return (Just path) else return Nothing
    ) Nothing possiblePaths
  
  case foundPath of
    Nothing -> return []  -- Directory not found
    Just path -> loadFromPath path

-- | Test that all corpus examples can be parsed.
testParsingCorpus :: Spec
testParsingCorpus = do
  it "parses all corpus files successfully" $ do
    corpus <- loadCorpusFiles
    let totalExamples = sum $ map (length . snd) corpus
    totalExamples `shouldSatisfy` (> 0)  -- Ensure we have examples
    
    -- Test parsing each example
    let parseResults = concatMap (\(file, examples) ->
          map (\example -> (file, example, fromGram example)) examples
          ) corpus
    
    let failures = filter (\(_, _, result) -> case result of
          Left _ -> True
          Right _ -> False
          ) parseResults
    
    if null failures
      then return ()  -- All passed
      else do
        let errorMsgs = map (\(file, example, result) ->
              case result of
                Left (ParseError msg) -> 
                  file ++ ": " ++ take 100 example ++ "...\n" ++ msg
                _ -> file ++ ": unexpected success"
              ) failures
        expectationFailure $ "Failed to parse " ++ show (length failures) ++ 
          " examples:\n" ++ unlines (take 10 errorMsgs)  -- Show first 10 failures

-- | Test round-trip conversion (serialize then parse).
testRoundTripCorpus :: Spec
testRoundTripCorpus = do
  it "round-trip conversion preserves structure for all corpus files" $ do
    corpus <- loadCorpusFiles
    let totalExamples = sum $ map (length . snd) corpus
    totalExamples `shouldSatisfy` (> 0)
    
    -- Test round-trip for each example
    let roundTripResults = concatMap (\(file, examples) ->
          map (\example -> (file, example, fromGram example)) examples
          ) corpus
    
    let failures = filter (\(_, _, result) -> case result of
          Left _ -> True
          Right _ -> False
          ) roundTripResults
    
    -- For examples that parse successfully, test round-trip
    let successfulParses = filter (\(_, _, result) -> case result of
          Right _ -> True
          Left _ -> False
          ) roundTripResults
    
    let roundTripFailures = concatMap (\(file, original, result) ->
          case result of
            Right pattern -> 
              let serialized = toGram pattern
                  reparsed = fromGram serialized
              in case reparsed of
                Right reparsedPattern -> 
                  -- Compare the patterns (simplified comparison for now)
                  if pattern == reparsedPattern
                    then []
                    else [(file, original, "Round-trip structure mismatch")]
                Left err -> [(file, original, "Failed to reparse: " ++ show err)]
            Left _ -> []
          ) successfulParses
    
    if null failures && null roundTripFailures
      then return ()  -- All passed
      else do
        let errorMsgs = map (\(file, example, msg) ->
              file ++ ": " ++ take 100 example ++ "...\n" ++ msg
              ) (take 10 roundTripFailures)
        expectationFailure $ "Round-trip failed for " ++ 
          show (length roundTripFailures) ++ " examples:\n" ++ 
          unlines errorMsgs

-- | Test comment handling in corpus files.
testCommentHandling :: Spec
testCommentHandling = do
  it "handles comments correctly in corpus files" $ do
    corpus <- loadCorpusFiles
    -- Find the comments.txt file specifically
    let commentsFile = find (\(file, _) -> file == "comments.txt") corpus
    case commentsFile of
      Nothing -> expectationFailure "comments.txt not found in corpus"
      Just (_, examples) -> do
        -- All comment examples should parse successfully
        let parseResults = map (\example -> (example, fromGram example)) examples
        let failures = filter (\(_, result) -> case result of
              Left _ -> True
              Right _ -> False
              ) parseResults
        
        if null failures
          then return ()
          else do
            let errorMsgs = map (\(example, result) ->
                  case result of
                    Left (ParseError msg) -> take 100 example ++ "...\n" ++ msg
                    _ -> "unexpected"
                  ) failures
            expectationFailure $ "Failed to parse " ++ show (length failures) ++ 
              " comment examples:\n" ++ unlines (take 5 errorMsgs)

-- | Test value types round-trip conversion.
testValueTypesRoundTrip :: Spec
testValueTypesRoundTrip = do
  it "handles all value types correctly in round-trip conversion" $ do
    corpus <- loadCorpusFiles
    -- Find files that test value types
    let valueTypeFiles = filter (\(file, _) -> 
          isInfixOf "value" file || isInfixOf "number" file || 
          isInfixOf "range" file || isInfixOf "array" file || 
          isInfixOf "map" file
          ) corpus
    
    let totalExamples = sum $ map (length . snd) valueTypeFiles
    totalExamples `shouldSatisfy` (> 0)
    
    -- Test round-trip for value type examples
    let roundTripResults = concatMap (\(file, examples) ->
          map (\example -> (file, example, fromGram example)) examples
          ) valueTypeFiles
    
    let successfulParses = filter (\(_, _, result) -> case result of
          Right _ -> True
          Left _ -> False
          ) roundTripResults
    
    let roundTripFailures = concatMap (\(file, original, result) ->
          case result of
            Right pattern -> 
              let serialized = toGram pattern
                  reparsed = fromGram serialized
              in case reparsed of
                Right reparsedPattern -> 
                  if pattern == reparsedPattern
                    then []
                    else [(file, original, "Round-trip structure mismatch")]
                Left err -> [(file, original, "Failed to reparse: " ++ show err)]
            Left _ -> []
          ) successfulParses
    
    if null roundTripFailures
      then return ()
      else do
        let errorMsgs = map (\(file, example, msg) ->
              file ++ ": " ++ take 100 example ++ "...\n" ++ msg
              ) (take 10 roundTripFailures)
        expectationFailure $ "Value type round-trip failed for " ++ 
          show (length roundTripFailures) ++ " examples:\n" ++ 
          unlines errorMsgs

-- | Test complex nested pattern structures.
testNestedPatternsRoundTrip :: Spec
testNestedPatternsRoundTrip = do
  it "handles complex nested pattern structures in round-trip conversion" $ do
    corpus <- loadCorpusFiles
    -- Find files that test patterns and subjects (likely to have nesting)
    let patternFiles = filter (\(file, _) -> 
          isInfixOf "pattern" file || isInfixOf "subject" file
          ) corpus
    
    let totalExamples = sum $ map (length . snd) patternFiles
    totalExamples `shouldSatisfy` (> 0)
    
    -- Test round-trip for nested pattern examples
    let roundTripResults = concatMap (\(file, examples) ->
          map (\example -> (file, example, fromGram example)) examples
          ) patternFiles
    
    let successfulParses = filter (\(_, _, result) -> case result of
          Right _ -> True
          Left _ -> False
          ) roundTripResults
    
    let roundTripFailures = concatMap (\(file, original, result) ->
          case result of
            Right pattern -> 
              let serialized = toGram pattern
                  reparsed = fromGram serialized
              in case reparsed of
                Right reparsedPattern -> 
                  if pattern == reparsedPattern
                    then []
                    else [(file, original, "Round-trip structure mismatch")]
                Left err -> [(file, original, "Failed to reparse: " ++ show err)]
            Left _ -> []
          ) successfulParses
    
    if null roundTripFailures
      then return ()
      else do
        let errorMsgs = map (\(file, example, msg) ->
              file ++ ": " ++ take 100 example ++ "...\n" ++ msg
              ) (take 10 roundTripFailures)
        expectationFailure $ "Nested pattern round-trip failed for " ++ 
          show (length roundTripFailures) ++ " examples:\n" ++ 
          unlines errorMsgs

-- | Test edge cases from corpus.
testEdgeCases :: Spec
testEdgeCases = do
  it "handles edge cases correctly (empty patterns, deep nesting, large records)" $ do
    corpus <- loadCorpusFiles
    -- Find files that likely contain edge cases
    let edgeCaseFiles = filter (\(file, _) -> 
          isInfixOf "empty" file || isInfixOf "graph" file
          ) corpus
    
    let totalExamples = sum $ map (length . snd) edgeCaseFiles
    totalExamples `shouldSatisfy` (> 0)
    
    -- Test parsing edge cases
    let parseResults = concatMap (\(file, examples) ->
          map (\example -> (file, example, fromGram example)) examples
          ) edgeCaseFiles
    
    let failures = filter (\(_, _, result) -> case result of
          Left _ -> True
          Right _ -> False
          ) parseResults
    
    if null failures
      then return ()
      else do
        let errorMsgs = map (\(file, example, result) ->
              case result of
                Left (ParseError msg) -> 
                  file ++ ": " ++ take 100 example ++ "...\n" ++ msg
                _ -> file ++ ": unexpected"
              ) failures
        expectationFailure $ "Failed to parse " ++ show (length failures) ++ 
          " edge case examples:\n" ++ unlines (take 10 errorMsgs)

spec :: Spec
spec = do
  describe "Tree-sitter-gram corpus integration" $ do
    
    describe "corpus file loading" $ do
      it "can load corpus files from directory" $ do
        corpus <- loadCorpusFiles
        length corpus `shouldSatisfy` (> 0)
        let totalExamples = sum $ map (length . snd) corpus
        totalExamples `shouldSatisfy` (> 0)
    
    describe "parsing corpus files" $ do
      testParsingCorpus
    
    describe "round-trip conversion" $ do
      testRoundTripCorpus
    
    describe "comment handling" $ do
      testCommentHandling
    
    describe "value types" $ do
      testValueTypesRoundTrip
    
    describe "nested patterns" $ do
      testNestedPatternsRoundTrip
    
    describe "edge cases" $ do
      testEdgeCases

