# Quickstart: Separation of Container and Content Parsing

This guide shows how to use the new container-aware API in `gram-hs`.

## Parsing Multiple Patterns

Previously, parsing multiple patterns would return a single wrapped pattern. Now you can get them as a list:

```haskell
import Gram

let input = "(a) (b) (c)"
case fromGramList input of
    Right patterns -> print (length patterns) -- Outputs: 3
    Left err -> print err
```

## Handling Document Headers

If your Gram file starts with a metadata record, you can separate it easily:

```haskell
import Gram

let input = "{version: 1.0} (a:Process)"
case fromGramWithHeader input of
    Right (Just header, patterns) -> do
        print header    -- Outputs: Map containing version: 1.0
        print patterns  -- Outputs: List containing (a:Process)
    Right (Nothing, patterns) -> 
        print "No header found"
    Left err -> print err
```

## Serialization

You can serialize lists of patterns directly:

```haskell
import Gram
import Pattern.Core

let patterns = [node "a", node "b"]
putStrLn $ toGramList patterns
-- Outputs:
-- (a)
-- (b)
```

Or with a header:

```haskell
import Gram
import Subject.Core (emptyPropertyRecord)

let header = Map.fromList [("app", VString "GramApp")]
let patterns = [node "a"]
putStrLn $ toGramWithHeader header patterns
-- Outputs:
-- {app: "GramApp"}
-- (a)
```
