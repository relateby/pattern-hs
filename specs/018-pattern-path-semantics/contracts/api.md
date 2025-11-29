# Gram Validator API

## Module: `Gram.Validate`

### Types

```haskell
data ValidationError
  = DuplicateDefinition Identifier
  | UndefinedReference Identifier
  | SelfReference Identifier
  | InconsistentDefinition Identifier String
  | ImmutabilityViolation Identifier
  deriving (Show, Eq)

type ValidationResult = Either [ValidationError] ()
```

### Functions

```haskell
-- | Validate a parsed Gram AST.
-- Returns a list of errors if validation fails, or Unit if successful.
validate :: Gram.CST.Gram -> ValidationResult

-- | Validate a single pattern (useful for incremental checks or tests).
validatePattern :: Gram.CST.AnnotatedPattern -> ValidationResult
```

