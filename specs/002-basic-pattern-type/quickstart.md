# Quickstart: Basic Pattern Type

**Feature**: 002-basic-pattern-type  
**Date**: 2025-01-27

## Overview

This quickstart guide shows how to create and use the basic Pattern type. The Pattern type is a decorated sequence that stores a value and contains zero or more Pattern elements.

## Prerequisites

- Haskell project with Pattern library installed
- GHC 9.10.3 or compatible version
- Basic understanding of Haskell data types

## Getting Started

### Import the Module

```haskell
import Pattern.Core (Pattern(..))
```

The `Pattern(..)` import brings in the data type, constructor, and field accessors.

### Creating Atomic Patterns

An atomic pattern is a pattern with no elements. Atomic patterns are the fundamental building blocks from which all other patterns are constructed:

```haskell
-- String atomic pattern
node1 :: Pattern String
node1 = Pattern { value = "node1", elements = [] }

-- Integer atomic pattern
node2 :: Pattern Int
node2 = Pattern { value = 42, elements = [] }

-- Custom type atomic pattern
data Person = Person { name :: String, age :: Int }
  deriving Show

alice :: Pattern Person
alice = Pattern { value = Person "Alice" 30, elements = [] }
```

### Creating Patterns with Elements

A pattern can contain pattern elements:

```haskell
-- Pattern with two elements
parent :: Pattern String
parent = Pattern 
  { value = "parent"
  , elements = [ Pattern { value = "elem1", elements = [] }
               , Pattern { value = "elem2", elements = [] }
               ]
  }

-- Nested patterns (patterns containing patterns)
nested :: Pattern String
nested = Pattern 
  { value = "root"
  , elements = [ Pattern { value = "level1"
                         , elements = [ Pattern { value = "level2", elements = [] } ]
                         }
               ]
  }
```

### Inspecting Pattern Structure

Use field accessors to inspect patterns:

```haskell
-- Extract the value
getValue :: Pattern String -> String
getValue p = value p

-- Extract the elements
getElements :: Pattern String -> [Pattern String]
getElements p = elements p

-- Check if a pattern is empty (no elements)
isLeaf :: Pattern v -> Bool
isLeaf p = null (elements p)

-- Example usage
example :: IO ()
example = do
  let leaf = Pattern { value = "test", elements = [] }
  let parent = Pattern { value = "parent", elements = [leaf] }
  
  putStrLn $ "Atomic pattern value: " ++ value leaf
  putStrLn $ "Atomic pattern has elements: " ++ show (not $ null $ elements leaf)
  putStrLn $ "Pattern has " ++ show (length $ elements parent) ++ " elements"
```

## Common Patterns

### Building a Tree Structure

```haskell
-- Build a simple tree
tree :: Pattern String
tree = Pattern 
  { value = "root"
  , elements = [ Pattern { value = "left", elements = [] }
               , Pattern { value = "right", elements = [] }
               ]
  }
```

### Singular Pattern

```haskell
-- A singular pattern has exactly one element
singularPattern :: Pattern String
singularPattern = Pattern 
  { value = "parent"
  , elements = [ Pattern { value = "only element", elements = [] } ]
  }
```

### Empty Elements List

A pattern with an empty elements list is an atomic pattern:

```haskell
-- These are equivalent (both are atomic patterns)
leaf1 :: Pattern String
leaf1 = Pattern { value = "leaf", elements = [] }

leaf2 :: Pattern String
leaf2 = Pattern { value = "leaf", elements = [] }
```

## Type Safety

The type system ensures type consistency:

```haskell
-- ✅ All patterns in a structure share the same type
valid :: Pattern String
valid = Pattern 
  { value = "root"
  , elements = [ Pattern { value = "element", elements = [] } ]
  }

-- ❌ This won't compile - type mismatch
-- invalid :: Pattern String
-- invalid = Pattern 
--   { value = "root"
--   , elements = [ Pattern { value = 42, elements = [] } ]  -- Error!
--   }
```

## Next Steps

After understanding the basic Pattern type, you can:

1. **Phase 2**: Add typeclass instances (Show, Eq) for pattern comparison and display
2. **Phase 3**: Use constructor functions (`pattern`, `patternWith`) for easier creation
3. **Phase 4-6**: Use typeclass instances (Functor, Foldable, Traversable) to transform patterns
4. **Phase 8+**: Classify patterns as graph elements (nodes, relationships, subgraphs, paths)

## Examples

### Complete Working Example

```haskell
module Main where

import Pattern.Core (Pattern(..))

main :: IO ()
main = do
  -- Create atomic patterns
  let node1 = Pattern { value = "Alice", elements = [] }
  let node2 = Pattern { value = "Bob", elements = [] }
  
  -- Create pattern with elements
  let relationship = Pattern 
        { value = "knows"
        , elements = [node1, node2]
        }
  
  -- Inspect structure
  putStrLn $ "Relationship value: " ++ value relationship
  putStrLn $ "Number of elements: " ++ show (length $ elements relationship)
  
  -- Access element values
  let elems = elements relationship
  case elems of
    [p1, p2] -> do
      putStrLn $ "First element: " ++ value p1
      putStrLn $ "Second element: " ++ value p2
    _ -> putStrLn "Unexpected structure"
```

## Troubleshooting

### Common Issues

1. **Type mismatch errors**: Ensure all patterns in a structure use the same value type
2. **Empty list**: An empty `elements` list creates an atomic pattern - this is correct
3. **Deep nesting**: Patterns can be arbitrarily nested - the type system allows this

### Getting Help

- See `data-model.md` for detailed structure documentation
- See `contracts/type-signatures.md` for complete API reference
- Check test files in `tests/Spec/Pattern/CoreSpec.hs` for usage examples

