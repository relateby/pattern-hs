# Quickstart: Basic Pattern Type

**Feature**: 002-basic-pattern-type  
**Date**: 2025-01-27

## Overview

This quickstart guide shows how to create and use the basic Pattern type. The Pattern type is a recursive tree structure that stores a value and contains zero or more child Pattern instances.

## Prerequisites

- Haskell project with Pattern library installed
- GHC 9.8.4 or compatible version
- Basic understanding of Haskell data types

## Getting Started

### Import the Module

```haskell
import Pattern.Core (Pattern(..))
```

The `Pattern(..)` import brings in the data type, constructor, and field accessors.

### Creating Leaf Patterns

A leaf pattern is a pattern with no child elements:

```haskell
-- String leaf pattern
node1 :: Pattern String
node1 = Pattern { value = "node1", elements = [] }

-- Integer leaf pattern
node2 :: Pattern Int
node2 = Pattern { value = 42, elements = [] }

-- Custom type leaf pattern
data Person = Person { name :: String, age :: Int }
  deriving Show

alice :: Pattern Person
alice = Pattern { value = Person "Alice" 30, elements = [] }
```

### Creating Patterns with Children

A pattern can contain child patterns:

```haskell
-- Pattern with two children
parent :: Pattern String
parent = Pattern 
  { value = "parent"
  , elements = [ Pattern { value = "child1", elements = [] }
               , Pattern { value = "child2", elements = [] }
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

-- Extract the children
getChildren :: Pattern String -> [Pattern String]
getChildren p = elements p

-- Check if a pattern is a leaf (no children)
isLeaf :: Pattern v -> Bool
isLeaf p = null (elements p)

-- Example usage
example :: IO ()
example = do
  let leaf = Pattern { value = "test", elements = [] }
  let parent = Pattern { value = "parent", elements = [leaf] }
  
  putStrLn $ "Leaf value: " ++ value leaf
  putStrLn $ "Leaf has children: " ++ show (not $ null $ elements leaf)
  putStrLn $ "Parent has " ++ show (length $ elements parent) ++ " children"
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

### Pattern with Single Child

```haskell
-- Pattern with exactly one child
singleChild :: Pattern String
singleChild = Pattern 
  { value = "parent"
  , elements = [ Pattern { value = "only child", elements = [] } ]
  }
```

### Empty Children List

A pattern with an empty children list is a leaf pattern:

```haskell
-- These are equivalent (both are leaf patterns)
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
  , elements = [ Pattern { value = "child", elements = [] } ]
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
  -- Create leaf patterns
  let node1 = Pattern { value = "Alice", elements = [] }
  let node2 = Pattern { value = "Bob", elements = [] }
  
  -- Create pattern with children
  let relationship = Pattern 
        { value = "knows"
        , elements = [node1, node2]
        }
  
  -- Inspect structure
  putStrLn $ "Relationship value: " ++ value relationship
  putStrLn $ "Number of children: " ++ show (length $ elements relationship)
  
  -- Access child values
  let children = elements relationship
  case children of
    [p1, p2] -> do
      putStrLn $ "First child: " ++ value p1
      putStrLn $ "Second child: " ++ value p2
    _ -> putStrLn "Unexpected structure"
```

## Troubleshooting

### Common Issues

1. **Type mismatch errors**: Ensure all patterns in a structure use the same value type
2. **Empty list vs leaf**: An empty `elements` list creates a leaf pattern - this is correct
3. **Deep nesting**: Patterns can be arbitrarily nested - the type system allows this

### Getting Help

- See `data-model.md` for detailed structure documentation
- See `contracts/type-signatures.md` for complete API reference
- Check test files in `tests/Spec/Pattern/CoreSpec.hs` for usage examples

