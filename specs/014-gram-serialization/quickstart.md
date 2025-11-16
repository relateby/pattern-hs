# Quickstart: Gram Serialization Library

**Feature**: 014-gram-serialization  
**Date**: 2025-01-27

## Overview

The Gram library provides serialization and deserialization of Pattern Subject data structures to and from gram notation. This guide demonstrates basic usage patterns and common operations.

## Installation

The gram library is part of the multi-library mono-repo. To use it in your project:

```haskell
-- In your .cabal file
build-depends:
    base >=4.17.0.0 && <5,
    gram,
    pattern,
    subject,
    containers ^>=0.6,
    text >=2.0
```

## Basic Usage

### Importing the Library

```haskell
import Gram                    -- Main module (re-exports everything)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (VString, VInteger, VBoolean)
import Data.Map (fromList, empty)
import Data.Set (Set)
import qualified Data.Set as Set
```

### Serializing Pattern Subject to Gram Notation

#### Simple Subject

```haskell
-- Create a simple subject
let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
let p = Pattern { value = s, elements = [] }

-- Serialize to gram notation
toGram p
-- Result: "(n:Person)"
```

#### Subject with Properties

```haskell
-- Create a subject with properties
let props = fromList [("name", VString "Alice"), ("age", VInteger 30)]
let s = Subject (Symbol "n") (Set.fromList ["Person"]) props
let p = Pattern { value = s, elements = [] }

-- Serialize to gram notation
toGram p
-- Result: "(n:Person {name:\"Alice\",age:30})"
```

#### Anonymous Subject

```haskell
-- Create an anonymous subject (empty Symbol)
let s = Subject (Symbol "") (Set.fromList ["Person"]) empty
let p = Pattern { value = s, elements = [] }

-- Serialize to gram notation
toGram p
-- Result: "(:Person)" or "()" depending on implementation
```

### Parsing Gram Notation to Pattern Subject

#### Simple Subject

```haskell
-- Parse a simple subject
case fromGram "(n:Person)" of
  Right p -> do
    putStrLn "Parsed successfully"
    print (value p)  -- Subject (Symbol "n") (Set.fromList ["Person"]) empty
  Left err -> putStrLn $ "Parse error: " ++ show err
```

#### Subject with Properties

```haskell
-- Parse a subject with properties
case fromGram "(n:Person {name:\"Alice\",age:30})" of
  Right p -> do
    putStrLn "Parsed successfully"
    print (value p)  -- Subject with properties
  Left err -> putStrLn $ "Parse error: " ++ show err
```

#### Error Handling

```haskell
-- Handle parsing errors
case fromGram "(invalid" of
  Right p -> putStrLn "Parsed successfully"
  Left (ParseError msg) -> putStrLn $ "Parse error: " ++ msg
  -- Result: "Parse error: Unexpected end of input: expected ')' at position 8"
```

### Round-Trip Conversion

```haskell
-- Create a Pattern Subject
let s = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")])
let p = Pattern { value = s, elements = [] }

-- Serialize then parse (round-trip)
case fromGram (toGram p) of
  Right p' -> 
    if p == p' then
      putStrLn "Round-trip conversion successful: structure and values preserved"
    else
      putStrLn "Round-trip conversion failed: structure or values changed"
  Left err -> putStrLn $ "Round-trip conversion failed: " ++ show err
```

## Advanced Usage

### Nested Patterns

```haskell
-- Create nested patterns
let s1 = Subject (Symbol "a") (Set.fromList ["Person"]) empty
let s2 = Subject (Symbol "b") (Set.fromList ["Person"]) empty
let p1 = Pattern { value = s1, elements = [] }
let p2 = Pattern { value = s2, elements = [] }
let parent = Pattern { value = Subject (Symbol "g") Set.empty empty, elements = [p1, p2] }

-- Serialize nested pattern
toGram parent
-- Result: "(g (a:Person) (b:Person))"
```

### Relationship Patterns

```haskell
-- Create a relationship pattern (a)-[r:KNOWS]->(b)
let source = Pattern { value = Subject (Symbol "a") (Set.fromList ["Person"]) empty, elements = [] }
let rel = Pattern { value = Subject (Symbol "r") (Set.fromList ["KNOWS"]) empty, elements = [] }
let target = Pattern { value = Subject (Symbol "b") (Set.fromList ["Person"]) empty, elements = [] }
let relationship = Pattern { value = Subject (Symbol "g") Set.empty empty, elements = [source, rel, target] }

-- Serialize relationship pattern
toGram relationship
-- Result: "(g (a:Person) (r:KNOWS) (b:Person))"
-- Note: Actual relationship syntax depends on gram notation specification
```

### All Value Types

#### Standard Value Types

```haskell
-- Integer
let props = fromList [("count", VInteger 42)]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {count:42})"

-- Decimal
let props = fromList [("pi", VDecimal 3.14)]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {pi:3.14})"

-- Boolean
let props = fromList [("active", VBoolean True)]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {active:true})"

-- String
let props = fromList [("name", VString "Alice")]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {name:\"Alice\"})"

-- Symbol
let props = fromList [("type", VSymbol "Person")]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {type:Person})"
```

#### Extended Value Types

```haskell
-- Tagged String
let props = fromList [("url", VTaggedString "url" "https://example.com")]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {url:url`https://example.com`})"

-- Array
let props = fromList [("tags", VArray [VString "tag1", VString "tag2"])]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {tags:[\"tag1\",\"tag2\"]})"

-- Map
let props = fromList [("metadata", VMap (fromList [("key1", VString "value1"), ("key2", VInteger 42)]))]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {metadata:{key1:\"value1\",key2:42}})"

-- Range
let props = fromList [("age", VRange (RangeValue (Just 18) (Just 65)))]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {age:18..65})"

-- Measurement
let props = fromList [("weight", VMeasurement "kg" 70.5)]
let s = Subject (Symbol "n") Set.empty props
toGram (Pattern { value = s, elements = [] })
-- Result: "(n {weight:70.5kg})"
```

## Testing

### Unit Tests

```haskell
import Test.Hspec
import Gram
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))

spec :: Spec
spec = do
  describe "toGram" $ do
    it "serializes simple subject" $ do
      let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
      let p = Pattern { value = s, elements = [] }
      toGram p `shouldBe` "(n:Person)"
    
    it "serializes subject with properties" $ do
      let props = fromList [("name", VString "Alice")]
      let s = Subject (Symbol "n") (Set.fromList ["Person"]) props
      let p = Pattern { value = s, elements = [] }
      toGram p `shouldBe` "(n:Person {name:\"Alice\"})"
  
  describe "fromGram" $ do
    it "parses simple subject" $ do
      case fromGram "(n:Person)" of
        Right p -> value p `shouldBe` Subject (Symbol "n") (Set.fromList ["Person"]) empty
        Left err -> expectationFailure $ "Parse failed: " ++ show err
    
    it "handles parse errors" $ do
      case fromGram "(invalid" of
        Right _ -> expectationFailure "Should have failed"
        Left (ParseError msg) -> msg `shouldContain` "expected"
```

### Property-Based Tests

```haskell
import Test.QuickCheck
import Gram
import Pattern.Core (Pattern(..))

-- Round-trip conversion property
prop_roundTrip :: Pattern Subject -> Property
prop_roundTrip p = 
  case fromGram (toGram p) of
    Right p' -> p === p'  -- Structure and values preserved
    Left err -> property False  -- Should never happen for valid input

-- Run property test
main = quickCheck prop_roundTrip
```

### Integration Tests with Test Corpus

```haskell
import Test.Hspec
import Gram
import System.Directory (listDirectory)
import System.FilePath ((</>))

-- Load test corpus files
loadTestCorpus :: IO [String]
loadTestCorpus = do
  files <- listDirectory "test-data/tree-sitter-gram-corpus"
  mapM (readFile . ("test-data/tree-sitter-gram-corpus" </>)) files

-- Test corpus integration
spec :: Spec
spec = do
  describe "tree-sitter-gram test corpus" $ do
    it "parses all test corpus files" $ do
      corpus <- loadTestCorpus
      forM_ corpus $ \content -> do
        case fromGram content of
          Right _ -> return ()  -- Success
          Left err -> expectationFailure $ "Failed to parse corpus file: " ++ show err
    
    it "round-trip conversion for all test corpus files" $ do
      corpus <- loadTestCorpus
      forM_ corpus $ \content -> do
        case fromGram content of
          Right p -> do
            case fromGram (toGram p) of
              Right p' -> p `shouldBe` p'  -- Round-trip preserves structure
              Left err -> expectationFailure $ "Round-trip failed: " ++ show err
          Left err -> expectationFailure $ "Initial parse failed: " ++ show err
```

## Common Patterns

### Serializing Multiple Patterns

```haskell
-- Serialize a list of patterns
serializePatterns :: [Pattern Subject] -> [String]
serializePatterns = map toGram

-- Usage
let patterns = [p1, p2, p3]
let gramStrings = serializePatterns patterns
```

### Parsing with Error Collection

```haskell
-- Parse multiple gram notation strings, collecting errors
parseMany :: [String] -> ([Pattern Subject], [ParseError])
parseMany = foldr f ([], [])
  where
    f s (ps, errs) = case fromGram s of
      Right p -> (p : ps, errs)
      Left err -> (ps, err : errs)
```

### Validating Gram Notation

```haskell
-- Check if a string is valid gram notation
isValidGram :: String -> Bool
isValidGram s = case fromGram s of
  Right _ -> True
  Left _ -> False
```

## Error Handling Best Practices

### Pattern Matching on Either

```haskell
-- Recommended: Pattern match on Either
case fromGram input of
  Right p -> do
    -- Use parsed pattern
    processPattern p
  Left (ParseError msg) -> do
    -- Handle error
    putStrLn $ "Parse error: " ++ msg
```

### Using Either Monad

```haskell
-- Chain parsing operations
parseAndProcess :: String -> Either ParseError Result
parseAndProcess input = do
  p <- fromGram input
  processPattern p
```

## Performance Considerations

- Serialization: O(n) where n is the number of nodes in the pattern
- Parsing: O(n) where n is the length of the gram notation string
- Memory: O(n) for both operations

For large patterns (10,000+ nodes) or large files (1MB+), operations should complete in under 1 second.

## Next Steps

- See `data-model.md` for detailed data model information
- See `contracts/type-signatures.md` for complete API documentation
- See `research.md` for technical decisions and rationale
- See `plan.md` for implementation plan and project structure

