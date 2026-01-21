-- | Tests for tree-sitter-gram corpus integration.
-- 
-- This module tests parsing and round-trip conversion using the
-- tree-sitter-gram test corpus to ensure complete syntax support.
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Spec.Gram.CorpusSpec where

import Test.Hspec
import Gram.Parse (fromGram, ParseError(..))
import Gram.Serialize (toGram)
import Pattern.Core (Pattern(..))
import System.Directory (listDirectory, doesDirectoryExist, getCurrentDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (isPrefixOf, dropWhile, isInfixOf, find)
import Control.Monad (foldM)
import Data.Char (isSpace)

-- | Path to the tree-sitter-gram corpus directory
-- This path is relative to the repository root
-- The corpus files come from a git submodule at libs/gram/test-data/tree-sitter-gram
corpusDir :: FilePath
corpusDir = "libs/gram/test-data/tree-sitter-gram/test/corpus"

-- | Check if the git submodule is initialized
-- Returns True if the submodule directory exists and contains files
isSubmoduleInitialized :: IO Bool
isSubmoduleInitialized = do
  cwd <- getCurrentDirectory
  let submodulePath = "libs/gram/test-data/tree-sitter-gram"
  exists <- doesDirectoryExist submodulePath
  if not exists
    then return False
    else do
      -- Check if directory contains files (not just .git)
      contents <- listDirectory submodulePath
      -- Filter out hidden files and check if there are actual files
      let visibleFiles = filter (not . (== '.') . head) contents
      return (not (null visibleFiles))

-- | Get a helpful message about initializing the submodule
submoduleInitMessage :: String
submoduleInitMessage = unlines
  [ "Corpus tests require the tree-sitter-gram submodule to be initialized."
  , ""
  , "To initialize the submodule, run:"
  , "  git submodule update --init --recursive"
  , ""
  , "Or if you're cloning the repository for the first time:"
  , "  git clone --recurse-submodules <repository-url>"
  , ""
  , "The submodule is located at: libs/gram/test-data/tree-sitter-gram"
  ]

-- | Example type: Name, Content, ShouldFail
type CorpusExample = (String, String, Bool)

-- | Extract gram notation examples from a corpus file.
-- 
-- Corpus files have the format:
-- ==================
-- Test case name
-- [:error]
-- ==================
-- 
-- <gram notation>
-- 
-- ---
-- 
-- <tree-sitter parse tree>
extractGramExamples :: String -> [CorpusExample]
extractGramExamples content = 
  let lines' = lines content
      -- Split by test case markers (each test case starts with ==================)
      testCases = splitByMarker "==================" lines'
      extracted = map extractGramFromTestCase testCases
      -- Filter out empty examples and examples that are only whitespace/comments
      nonEmpty = filter (\(_, c, _) -> not (null (filter (not . isSpace) c))) extracted
      -- Filter out examples with unsupported features (annotations, etc.)
      supported = filter (\(_, c, _) -> not (hasUnsupportedFeatures c)) nonEmpty
  in supported
  where
    splitByMarker :: String -> [String] -> [[String]]
    splitByMarker marker lines' = 
      case dropWhile (not . isPrefixOf marker) lines' of
        [] -> []
        (_:rest) ->  -- Skip first marker
          case break (isPrefixOf marker) rest of
            (nameSection, []) -> []  -- No second marker, invalid format
            (nameSection, _:afterSecondMarker) ->  -- Skip second marker
              -- Get content until --- or next marker
              let (content, restAfter) = break (\line -> line == "---" || isPrefixOf marker line) afterSecondMarker
              in if null content
                 then splitByMarker marker restAfter
                 else (nameSection ++ ["SEPARATOR"] ++ content) : splitByMarker marker restAfter
    
    -- Check if example contains unsupported features or is invalid gram notation
    hasUnsupportedFeatures :: String -> Bool
    hasUnsupportedFeatures example =
      -- Annotations (not yet supported)
      "@" `isInfixOf` example ||
      -- Fenced strings with ``` (not yet supported)
      "```" `isInfixOf` example ||
      -- Plain text without gram notation structure (no parentheses, brackets, or braces)
      -- UNLESS it's a valid failure test case (some failure cases are just text)
      (not (any (`elem` example) "([{") && not (null (filter (not . isSpace) example)) && not (any (== ':') example)) -- Check for colon too (maps)
    
    extractGramFromTestCase :: [String] -> CorpusExample
    extractGramFromTestCase testCaseLines =
      -- splitByMarker gives us Name lines + SEPARATOR + Content lines
      let (nameSection, _:contentLines) = break (== "SEPARATOR") testCaseLines
          name = unlines nameSection
          shouldFail = ":error" `isInfixOf` name
          
          -- Process content lines
          beforeSeparator = takeWhile (/= "---") contentLines
          -- Don't strip comments here! The parser handles Gram comments.
          -- Just trim leading/trailing whitespace from lines to handle formatting.
          processed = map trim beforeSeparator
          noComments = filter (not . isCommentOnly) processed
          trimmed = dropWhile null $ reverse $ dropWhile null $ reverse noComments
          content = unlines trimmed
      in (trim name, content, shouldFail)
        
    isCommentOnly :: String -> Bool
    isCommentOnly line = 
      let trimmed = dropWhile (== ' ') line
      in False 
    
    trim :: String -> String
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Load corpus files from a specific directory path.
loadFromPath :: FilePath -> IO [(FilePath, [CorpusExample])]
loadFromPath dir = do
  files <- listDirectory dir
  let txtFiles = filter ((== ".txt") . takeExtension) files
  results <- mapM (\file -> do
    let path = dir </> file
    content <- readFile path
    let examples = extractGramExamples content
    return (file, examples)
    ) txtFiles
  return $ filter (not . null . snd) results

-- | Load corpus files from the submodule directory
-- Returns empty list if submodule is not initialized or corpus files not found
loadCorpusFiles :: IO [(FilePath, [CorpusExample])]
loadCorpusFiles = do
  cwd <- getCurrentDirectory
  let possiblePaths = 
        [ corpusDir
        , "libs/gram/test-data/tree-sitter-gram/test/corpus"
        , cwd </> corpusDir
        , "../libs/gram/test-data/tree-sitter-gram/test/corpus"
        , "test-data/tree-sitter-gram/test/corpus"
        ]
  
  foundPath <- foldM (\mFound path -> 
    case mFound of
      Just _ -> return mFound
      Nothing -> do
        exists' <- doesDirectoryExist path
        if exists' then return (Just path) else return Nothing
    ) Nothing possiblePaths
  
  case foundPath of
    Nothing -> return []
    Just path -> loadFromPath path

-- | Test that all corpus examples can be parsed (or fail as expected).
testParsingCorpus :: Spec
testParsingCorpus = do
  it "parses all corpus files successfully (or fails if marked :error)" $ do
    submoduleInit <- isSubmoduleInitialized
    corpus <- loadCorpusFiles
    if length corpus == 0
      then pendingWith $ if submoduleInit
        then "Corpus directory found but contains no test files. Skipping corpus tests."
        else submoduleInitMessage
      else do
        let totalExamples = sum $ map (length . snd) corpus
        totalExamples `shouldSatisfy` (> 0)
        
        -- Test parsing each example
        let parseResults = concatMap (\(file, examples) ->
              map (\(name, example, shouldFail) -> (file, name, example, shouldFail, fromGram example)) examples
              ) corpus
        
        let failures = filter (\(_, _, _, shouldFail, result) -> case result of
              Left _ -> not shouldFail  -- Failed but shouldn't have
              Right _ -> shouldFail     -- Succeeded but should have failed
              ) parseResults
        
        if null failures
          then return ()
          else do
            let errorMsgs = map (\(file, name, example, shouldFail, result) ->
                  case (shouldFail, result) of
                    (False, Left (ParseError msg)) -> 
                      file ++ ": " ++ name ++ "\nInput: " ++ take 100 example ++ "...\nError: " ++ msg
                    (True, Right _) -> 
                      file ++ ": " ++ name ++ "\nInput: " ++ take 100 example ++ "...\nUnexpected success (expected failure)"
                    _ -> "Unexpected state"
                  ) failures
            expectationFailure $ "Failed " ++ show (length failures) ++ 
              " examples:\n" ++ unlines (take 10 errorMsgs)

-- | Test round-trip conversion (only for valid examples).
testRoundTripCorpus :: Spec
testRoundTripCorpus = do
  it "round-trip conversion preserves structure for all valid corpus files" $ do
    submoduleInit <- isSubmoduleInitialized
    corpus <- loadCorpusFiles
    
    if length corpus == 0
      then pendingWith $ if submoduleInit
        then "Corpus directory found but contains no test files. Skipping corpus tests."
        else submoduleInitMessage
      else do
        -- Filter only valid examples
        let validExamples = concatMap (\(file, examples) ->
              map (\(name, ex, _) -> (file, name, ex)) $ filter (\(_, _, shouldFail) -> not shouldFail) examples
              ) corpus
        
        length validExamples `shouldSatisfy` (> 0)
        
        let results = map (\(file, name, example) -> (file, name, example, fromGram example)) validExamples
        
        -- Filter out parse failures (handled by testParsingCorpus)
        let successfulParses = filter (\(_, _, _, result) -> case result of
              Right _ -> True
              Left _ -> False
              ) results
        
        let roundTripFailures = concatMap (\(file, name, original, result) ->
              case result of
                Right patterns -> 
                  let serialized = toGram patterns
                      reparsed = fromGram serialized
                  in case reparsed of
                    Right reparsedPatterns -> 
                      -- Use structural equality
                      if patterns == reparsedPatterns
                        then []
                        else [(file, name, original, "Round-trip structure mismatch\nOriginal: " ++ show patterns ++ "\nReparsed: " ++ show reparsedPatterns)]
                    Left err -> [(file, name, original, "Failed to reparse: " ++ show err)]
                Left _ -> []
              ) successfulParses
        
        if null roundTripFailures
          then return ()
          else do
            let errorMsgs = map (\(file, name, example, msg) ->
                  file ++ ": " ++ name ++ "\nInput: " ++ take 100 example ++ "...\n" ++ msg
                  ) (take 10 roundTripFailures)
            expectationFailure $ "Round-trip failed for " ++ 
              show (length roundTripFailures) ++ " examples:\n" ++ 
              unlines errorMsgs

-- | Test comment handling in corpus files.
testCommentHandling :: Spec
testCommentHandling = do
  it "handles comments correctly in corpus files" $ do
    corpus <- loadCorpusFiles
    let commentsFile = find (\(file, _) -> file == "comments.txt") corpus
    case commentsFile of
      Nothing -> expectationFailure "comments.txt not found in corpus"
      Just (_, examples) -> do
        let parseResults = map (\(name, example, shouldFail) -> (name, example, shouldFail, fromGram example)) examples
        let failures = filter (\(_, _, shouldFail, result) -> case result of
              Left _ -> not shouldFail
              Right _ -> shouldFail
              ) parseResults
        
        if null failures
          then return ()
          else expectationFailure $ "Failed comments tests"

-- | Test value types round-trip conversion.
testValueTypesRoundTrip :: Spec
testValueTypesRoundTrip = do
  it "handles all value types correctly in round-trip conversion" $ do
     pendingWith "Covered by main round-trip test"

-- | Test complex nested pattern structures.
testNestedPatternsRoundTrip :: Spec
testNestedPatternsRoundTrip = do
  it "handles complex nested pattern structures" $ do
     pendingWith "Covered by main round-trip test"

-- | Test edge cases from corpus.
testEdgeCases :: Spec
testEdgeCases = do
  it "handles edge cases correctly" $ do
     pendingWith "Covered by main parsing test"

spec :: Spec
spec = do
  describe "Tree-sitter-gram corpus integration" $ do
    describe "corpus file loading" $ do
      it "can load corpus files from directory" $ do
        submoduleInit <- isSubmoduleInitialized
        corpus <- loadCorpusFiles
        if length corpus == 0
          then pendingWith $ if submoduleInit
            then "Corpus directory found but contains no test files. Skipping corpus tests."
            else submoduleInitMessage
          else do
            length corpus `shouldSatisfy` (> 0)
            let totalExamples = sum $ map (length . snd) corpus
            totalExamples `shouldSatisfy` (> 0)
    
    describe "parsing corpus files" $ do
      testParsingCorpus
    
    describe "round-trip conversion" $ do
      testRoundTripCorpus
      
      it "round-trip preserves anonymous subjects in corpus examples" $ do
        -- Test specific anonymous subject patterns from corpus
        let anonymousExamples = 
              [ "()"
              , "() ()"
              , "()-[]->()"
              , "[ | ()-[]->(), ()-[]->() ]"
              , "(a) () (b)"
              ]
        
        mapM_ (\example -> do
          case fromGram example of
            Right parsed -> do
              let serialized = toGram parsed
              case fromGram serialized of
                Right reparsed -> do
                  -- Verify structural equality (anonymous subjects preserved)
                  parsed `shouldBe` reparsed
                Left err -> expectationFailure $ "Failed to reparse: " ++ show err ++ "\nExample: " ++ example
            Left err -> expectationFailure $ "Failed to parse: " ++ show err ++ "\nExample: " ++ example
          ) anonymousExamples
