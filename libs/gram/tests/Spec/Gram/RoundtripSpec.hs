{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Spec.Gram.RoundtripSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BSL
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Subject.Value as SubjectValue
import qualified Gram.Parse as Gram
import qualified Gram.Serialize as Gram
import qualified Gram.JSON ()  -- Import ToJSON/FromJSON instances
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Helper function to test roundtrip: Gram -> JSON -> Gram
roundtripGramJSON :: String -> Either String String
roundtripGramJSON gramInput = do
  -- Parse gram to [Pattern]
  patterns <- case Gram.fromGram gramInput of
    Left err -> Left (show err)
    Right ps -> Right ps
  
  -- Convert to JSON
  let jsonBytes = encode patterns
  
  -- Parse JSON back to [Pattern]
  patterns' <- case (decode jsonBytes :: Maybe [Pattern.Pattern Subject.Subject]) of
    Just ps -> Right ps
    Nothing -> Left "Failed to decode JSON back to Pattern list"
  
  -- Convert back to Gram
  let gramOutput = Gram.toGram patterns'
  
  return gramOutput

-- | Find all .gram test files in a directory recursively
findCorpusFiles :: FilePath -> IO [FilePath]
findCorpusFiles dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then return []
    else do
      contents <- listDirectory dir
      let fullPaths = map (dir </>) contents
      
      files <- forM fullPaths $ \path -> do
        isFile <- doesFileExist path
        isDir <- doesDirectoryExist path
        if isFile && takeExtension path == ".gram"
          then return [path]
          else if isDir
            then findCorpusFiles path  -- Recurse into subdirectories
            else return []
      
      return $ concat files

-- | Test roundtrip for a single corpus file
testRoundtrip :: FilePath -> Spec
testRoundtrip filePath = it ("roundtrips " ++ filePath) $ do
  gramInput <- readFile filePath
  case roundtripGramJSON gramInput of
    Left err -> expectationFailure $ "Roundtrip failed: " ++ err
    Right gramOutput -> do
      -- Re-parse both to compare patterns structurally
      case (Gram.fromGram gramInput, Gram.fromGram gramOutput) of
        (Right p1, Right p2) -> p1 `shouldBe` p2
        (Left e1, _) -> expectationFailure $ "Original parse failed: " ++ show e1
        (_, Left e2) -> expectationFailure $ "Output parse failed: " ++ show e2

spec :: Spec
spec = do
  describe "Root record roundtrip (fromGram / toGram)" $ do
    it "roundtrips {h:1}\\n(a) via fromGram . toGram . fromGram" $ do
      let input = "{h:1}\n(a)"
      case Gram.fromGram input of
        Right ps -> do
          let out = Gram.toGram ps
          out `shouldBe` "{h:1}\n(a)"
          case Gram.fromGram out of
            Right ps' -> ps `shouldBe` ps'
            Left e -> expectationFailure $ "Reparse failed: " ++ show e
        Left e -> expectationFailure $ "Parse failed: " ++ show e

    it "roundtrips {}" $ do
      case Gram.fromGram "{}" of
        Right [p] -> do
          Gram.toGram [p] `shouldBe` "{}"
          Gram.fromGram "{}" `shouldBe` Right [p]
        _ -> expectationFailure "fromGram {} should return [p]"

  describe "JSON Roundtrip Tests" $ do
    
    describe "Custom Edge Case Roundtrip Tests" $ do
      customFiles <- runIO $ findCorpusFiles "libs/gram/test-data/roundtrip/custom"
      
      if null customFiles
        then it "warns about missing custom test files" $
          pendingWith "No .gram custom test files found in libs/gram/test-data/roundtrip/custom/"
        else do
          it ("found " ++ show (length customFiles) ++ " custom test files") $ do
            length customFiles `shouldSatisfy` (> 0)
          
          mapM_ testRoundtrip customFiles
    
    describe "Full Corpus Roundtrip Tests" $ do
      corpusFiles <- runIO $ findCorpusFiles "libs/gram/test-data/tree-sitter-gram/test/corpus"
      
      if null corpusFiles
        then it "skips corpus tests (submodule not initialized)" $
          pendingWith "tree-sitter-gram submodule not initialized"
        else do
          it ("found " ++ show (length corpusFiles) ++ " corpus files") $ do
            length corpusFiles `shouldSatisfy` (> 0)
          
          -- Only test a subset to keep tests fast
          let testFiles = take 10 corpusFiles
          mapM_ testRoundtrip testFiles
    
    describe "Property-based Roundtrip Tests" $ do
      it "roundtrips preserve structure for arbitrary patterns" $ 
        property prop_roundtripPreservesStructure
      
      it "roundtrips preserve all value types" $
        property prop_roundtripPreservesAllValueTypes

-- | QuickCheck property: roundtrip should preserve pattern structure
-- Note: JSON doesn't distinguish between integers and decimals that are whole numbers
-- (e.g., 2.0 becomes 2), so we use semantic equivalence rather than strict equality
prop_roundtripPreservesStructure :: Pattern.Pattern Subject.Subject -> Bool
prop_roundtripPreservesStructure pat =
  case decode (encode pat) of
    Just pat' -> patternEquivalent pat pat'
    Nothing -> False

-- | QuickCheck property: roundtrip should preserve all value types
-- Note: JSON doesn't distinguish between integers and decimals that are whole numbers
prop_roundtripPreservesAllValueTypes :: SubjectValue.Value -> Bool
prop_roundtripPreservesAllValueTypes val =
  let pat = Pattern.Pattern (Subject.Subject (Subject.Symbol "test") Set.empty (Map.singleton "val" val)) []
  in case decode (encode pat) of
    Just pat' -> patternEquivalent pat pat'
    Nothing -> False

-- | Check if two patterns are semantically equivalent
-- Handles the case where JSON doesn't distinguish VDecimal and VInteger for whole numbers
patternEquivalent :: Pattern.Pattern Subject.Subject -> Pattern.Pattern Subject.Subject -> Bool
patternEquivalent (Pattern.Pattern v1 es1) (Pattern.Pattern v2 es2) =
  subjectEquivalent v1 v2 && length es1 == length es2 && and (zipWith patternEquivalent es1 es2)

-- | Check if two subjects are semantically equivalent
subjectEquivalent :: Subject.Subject -> Subject.Subject -> Bool
subjectEquivalent (Subject.Subject sym1 labels1 props1) (Subject.Subject sym2 labels2 props2) =
  sym1 == sym2 && labels1 == labels2 && Map.keys props1 == Map.keys props2 &&
  and (Map.elems $ Map.intersectionWith valueEquivalent props1 props2)

-- | Check if two values are semantically equivalent
-- Treats VInteger n and VDecimal (fromIntegral n) as equivalent
valueEquivalent :: SubjectValue.Value -> SubjectValue.Value -> Bool
valueEquivalent (SubjectValue.VInteger i1) (SubjectValue.VInteger i2) = i1 == i2
valueEquivalent (SubjectValue.VDecimal d1) (SubjectValue.VDecimal d2) = d1 == d2
valueEquivalent (SubjectValue.VInteger i) (SubjectValue.VDecimal d) = fromIntegral i == d
valueEquivalent (SubjectValue.VDecimal d) (SubjectValue.VInteger i) = d == fromIntegral i
valueEquivalent (SubjectValue.VBoolean b1) (SubjectValue.VBoolean b2) = b1 == b2
valueEquivalent (SubjectValue.VString s1) (SubjectValue.VString s2) = s1 == s2
valueEquivalent (SubjectValue.VSymbol s1) (SubjectValue.VSymbol s2) = s1 == s2
valueEquivalent (SubjectValue.VTaggedString t1 c1) (SubjectValue.VTaggedString t2 c2) = t1 == t2 && c1 == c2
valueEquivalent (SubjectValue.VArray vs1) (SubjectValue.VArray vs2) = 
  length vs1 == length vs2 && and (zipWith valueEquivalent vs1 vs2)
valueEquivalent (SubjectValue.VMap m1) (SubjectValue.VMap m2) = 
  Map.keys m1 == Map.keys m2 && and (Map.elems $ Map.intersectionWith valueEquivalent m1 m2)
valueEquivalent (SubjectValue.VRange r1) (SubjectValue.VRange r2) = r1 == r2
valueEquivalent (SubjectValue.VMeasurement u1 v1) (SubjectValue.VMeasurement u2 v2) = u1 == u2 && v1 == v2
valueEquivalent _ _ = False

-- QuickCheck Arbitrary instances for generating test data
instance Arbitrary Subject.Symbol where
  arbitrary = Subject.Symbol <$> arbitrary

instance Arbitrary Subject.Subject where
  arbitrary = Subject.Subject
    <$> arbitrary
    <*> (Set.fromList <$> listOf (elements ["Label1", "Label2", "Label3"]))
    <*> (Map.fromList <$> listOf ((,) <$> elements ["key1", "key2", "key3"] <*> arbitrary))

instance Arbitrary SubjectValue.Value where
  arbitrary = oneof
    [ SubjectValue.VInteger <$> arbitrary
    , SubjectValue.VDecimal <$> arbitrary
    , SubjectValue.VBoolean <$> arbitrary
    , SubjectValue.VString <$> arbitrary
    , SubjectValue.VSymbol <$> arbitrary
    , SubjectValue.VTaggedString <$> elements ["json", "url", "code"] <*> arbitrary
    , SubjectValue.VArray <$> resize 3 (listOf arbitrary)
    , SubjectValue.VMap <$> (Map.fromList <$> resize 3 (listOf ((,) <$> arbitrary <*> arbitrary)))
    , SubjectValue.VRange <$> arbitrary
    , SubjectValue.VMeasurement <$> elements ["kg", "m", "°C"] <*> arbitrary
    ]

instance Arbitrary SubjectValue.RangeValue where
  arbitrary = SubjectValue.RangeValue <$> arbitrary <*> arbitrary

instance Arbitrary (Pattern.Pattern Subject.Subject) where
  arbitrary = sized $ \n ->
    if n <= 0
      then Pattern.Pattern <$> arbitrary <*> pure []
      else Pattern.Pattern <$> arbitrary <*> resize (n `div` 2) (listOf arbitrary)
