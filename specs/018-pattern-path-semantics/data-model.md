# Data Model

## Symbol Table

The internal state used during validation.

```haskell
type SymbolTable = Map Identifier SymbolInfo

data SymbolInfo = SymbolInfo
  { symType :: SymbolType      -- Node, Relationship, or Pattern
  , symStatus :: DefinitionStatus
  , symSignature :: Maybe PatternSignature -- For consistency checks
  }

data SymbolType
  = TypeNode
  | TypeRelationship
  | TypePattern
  | TypeUnknown -- Inferred but not yet specific

data DefinitionStatus
  = StatusDefined        -- Fully defined (e.g., [a])
  | StatusReferenced     -- Referenced but not yet defined
  | StatusImplicit       -- Implicitly defined (e.g., in a path)

data PatternSignature = PatternSignature
  { sigLabels :: Set String
  , sigArity :: Int -- Number of elements
  }
```

## Validation Context

The environment for validation.

```haskell
data ValidationEnv = ValidationEnv
  { envCurrentPath :: [Identifier] -- For cycle detection
  }

data ValidationError
  = ErrDuplicateDefinition Identifier
  | ErrUndefinedReference Identifier
  | ErrSelfReference Identifier
  | ErrInconsistentDefinition Identifier String -- String describes mismatch
  | ErrImmutabilityViolation Identifier
```

## Core Structures (Existing)

Refer to `Gram.CST` for the AST structure being validated.

