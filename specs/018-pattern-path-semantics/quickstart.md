# Quickstart: Gram Validator

## Usage

```haskell
import Gram.Parse (parseGram)
import Gram.Validate (validate)
import Text.Megaparsec (parse, errorBundlePretty)

main :: IO ()
main = do
  let source = "[a], [b | a]"
  case parse parseGram "" source of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right cst -> case validate cst of
      Left errs -> mapM_ print errs
      Right () -> putStrLn "Validation successful!"
```

## Common Errors

### Duplicate Definition
```
[a]
[a] -- Error: DuplicateDefinition 'a'
```

### Undefined Reference
```
[a | b] -- Error: UndefinedReference 'b'
```

### Inconsistent Path
```
[r | a, b]
(a)-[r]->(c) -- Error: InconsistentDefinition 'r' (arity mismatch)
```

