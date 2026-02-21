# Quickstart: Verifying Polish

This guide explains how to verify the integration and polish of the `pattern` library.

## Prerequisites
- GHC 9.12.2
- Cabal 3.10+

## 1. Generating Documentation
To verify Haddock coverage and example rendering:

```bash
cabal haddock pattern --haddock-all
```

Check the output for "Missing documentation" warnings. Open the generated HTML to verify formatting.

## 2. Running Tests
To verify "rigorous semantics" via property tests:

```bash
cabal test pattern-test
```

Ensure all tests pass (0 failures).

## 3. Verifying Exports
Launch the REPL to check the API surface:

```bash
cabal repl pattern
```

In the REPL:

```haskell
:browse Pattern
```

Verify that only the intended public API is visible.

