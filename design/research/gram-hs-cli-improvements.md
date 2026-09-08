# gramref CLI Tool Improvements for Testing gram-rs

**Purpose**: Document requested improvements to the `gramref` CLI tool to make it more suitable for automated testing and equivalence checking with `gram-rs`.

**Target**: `gramref` reference implementation CLI tool  
**Location**: `/Users/akollegger/.cabal/bin/gramref`  
**Manpage**: `/Users/akollegger/.cabal/share/man/man1/gramref.1`

## Overview

The `gramref` CLI tool is the reference implementation for the gram/pattern ecosystem. To effectively use it for testing `gram-rs`, several improvements would enhance automation, comparison, and test data generation capabilities.

## Priority Classifications

- **High Priority**: Blocks or significantly impedes testing infrastructure implementation
- **Medium Priority**: Improves efficiency and developer experience
- **Low Priority**: Nice-to-have enhancements

---

## High Priority Improvements

### 1. Metadata Exclusion for Equivalence Checking

**Problem**: The current JSON output includes `Meta.Hash` and `Meta.Timestamp` fields that change on every execution, making direct JSON comparison difficult for equivalence checking.

**Current Output Structure**:
```json
{
  "Meta": {
    "Command": "parse",
    "Hash": "...",  // Changes every run
    "Timestamp": "2025-12-27T20:46:36.551812+0000",  // Changes every run
    "Version": "0.1.0"
  },
  "Result": { ... }
}
```

**Requested Solution**: Add flags to exclude metadata:
- `--no-meta` or `--value-only`: Output only `Result.Value` (or `Error`) without metadata wrapper
- `--deterministic`: Ensure deterministic output (sorted keys, no timestamps, fixed hash)

**Example Usage**:
```bash
# Output only the pattern value for comparison
gramref parse input.gram --format json --value-only

# Deterministic output suitable for comparison
gramref parse input.gram --format json --deterministic
```

**Use Case**: Direct comparison of `Result.Value` between gramref and gram-rs outputs in equivalence checking utilities.

**Implementation Notes**:
- When `--value-only` is used, output should be just the `Result.Value` JSON (or `Error` object if error occurred)
- When `--deterministic` is used, all metadata should use fixed values or be excluded
- Both flags can be combined

**Status**: [ ] Not Implemented

---

### 2. Test Suite Generation (`--type suite`)

**Problem**: The `generate --type suite` command currently returns "Generator type not yet implemented" error, preventing automated test case extraction.

**Current Behavior**:
```bash
$ gramref generate --type suite --count 2 --seed 42
{
  "Error": {
    "Type": "ParseError",
    "Message": "Generator type not yet implemented"
  }
}
```

**Requested Solution**: Implement `--type suite` to output test cases in the format defined by `specs/002-workspace-setup/contracts/test-sync-format.md`:

**Expected Output Format**:
```json
{
  "version": "1.0",
  "test_cases": [
    {
      "name": "test_case_identifier",
      "description": "Human-readable description",
      "input": {
        "type": "gram_notation",
        "value": "(node)-[edge]->(target)"
      },
      "expected": {
        "type": "pattern",
        "value": { ... }
      },
      "operations": [
        {
          "op": "match",
          "against": "(pattern)",
          "expected_bindings": [ ... ]
        }
      ]
    }
  ]
}
```

**Example Usage**:
```bash
# Generate test suite with 10 test cases
gramref generate --type suite --count 10 --seed 42 --format json > test_cases.json

# Generate test suite with specific complexity
gramref generate --type suite --count 5 --complexity standard --seed 42
```

**Use Case**: Automated test case extraction for gram-rs testing infrastructure (User Story 4: Test Data Extraction).

**Implementation Notes**:
- Should generate test cases covering various pattern structures and operations
- Test cases should be deterministic when using `--seed`
- Should support `--complexity` levels: minimal, basic, standard, complex, adversarial
- Output must conform to test-sync-format.md schema

**Status**: [ ] Not Implemented

---

### 3. Canonical JSON Output Mode

**Problem**: JSON output may have non-deterministic key ordering or formatting, making comparison unreliable.

**Requested Solution**: Add `--canonical` flag that ensures:
- Sorted keys at all levels
- Consistent formatting (no optional whitespace)
- No optional fields (only required fields)
- Deterministic across runs

**Example Usage**:
```bash
# Canonical output for reliable comparison
gramref parse input.gram --format json --canonical
```

**Use Case**: Reliable comparison and snapshot testing where exact JSON matching is required.

**Implementation Notes**:
- Should use consistent JSON serialization (sorted keys, no trailing commas, etc.)
- Can be combined with `--value-only` for clean comparison output
- Should be the default when `--deterministic` is used

**Status**: [ ] Not Implemented

---

## Medium Priority Improvements

### 4. Batch Processing Mode

**Problem**: Processing multiple test inputs requires multiple invocations, which is inefficient for large test suites.

**Requested Solution**: Add `--batch` mode that accepts multiple inputs:

**Example Usage**:
```bash
# Process multiple files
gramref parse --batch input1.gram input2.gram input3.gram --format json

# Process from stdin (newline-delimited)
cat test_files.txt | gramref parse --batch --format json
```

**Use Case**:**
- Efficient processing of multiple test cases
- Integration with test runners that need to process many inputs
- Reduced overhead when comparing multiple patterns

**Implementation Notes**:
- Output should be either:
  - Array of results: `[{...}, {...}, {...}]`
  - Newline-delimited JSON (NDJSON): one JSON object per line
- Should support `--batch-format` to choose output format
- Errors for individual inputs should not stop batch processing

**Status**: [ ] Not Implemented

---

### 5. Property Test Data Generation

**Problem**: `generate --type pattern` works, but there's no mode specifically optimized for property-based testing with size/complexity control.

**Requested Solution**: Add `--type property` that generates patterns suitable for property-based testing:

**Example Usage**:
```bash
# Generate 100 patterns for property testing
gramref generate --type property --count 100 --seed 42 --complexity standard

# Generate patterns with size constraints
gramref generate --type property --count 50 --complexity basic --max-size 10
```

**Use Case**: Generate test data for proptest generators in gram-rs (User Story 1: Property-Based Testing).

**Implementation Notes**:
- Should generate patterns with controlled size/complexity
- Should support `--max-size` or `--max-depth` constraints
- Patterns should be valid and suitable for property testing
- Should work with `--seed` for reproducibility

**Status**: [ ] Not Implemented

---

### 6. Output Filtering/Selection

**Problem**: Need to extract specific parts of output for comparison (e.g., just `Result.Value` without parsing JSON externally).

**Requested Solution**: Add JSONPath-like filtering or output path selection:

**Example Usage**:
```bash
# Extract just the pattern value
gramref parse input.gram --format json --select '.Result.Value'

# Extract specific field
gramref parse input.gram --format json --select '.Result.Value.symbol'
```

**Alternative**: Use `--output-path` to specify which part to output:
```bash
gramref parse input.gram --format json --output-path Result.Value
```

**Use Case**: Extract just the pattern value for comparison, ignoring metadata, without external JSON parsing tools.

**Implementation Notes**:
- Could use simple dot-notation path (e.g., `Result.Value`)
- Or support JSONPath subset
- Should handle errors gracefully (invalid path, missing field, etc.)

**Status**: [ ] Not Implemented

---

## Low Priority Improvements

### 7. Direct Comparison Command

**Problem**: No built-in way to compare two outputs from gramref or between gramref and gram-rs.

**Requested Solution**: Add `compare` command:

**Example Usage**:
```bash
# Compare two JSON outputs
gramref compare output1.json output2.json --format diff

# Compare with structured output
gramref compare output1.json output2.json --format json
```

**Expected Output**:
```json
{
  "equivalent": false,
  "differences": [
    {
      "path": ["Result", "Value", "symbol"],
      "expected": "node1",
      "actual": "node2"
    }
  ]
}
```

**Use Case**: Quick validation of gram-rs outputs against gramref without writing custom comparison code.

**Implementation Notes**:
- Should support various output formats (diff, json, human-readable)
- Should report field-level differences
- Could integrate with equivalence checking utilities

**Status**: [ ] Not Implemented

---

### 8. Test Case Extraction from Existing Tests

**Problem**: Manual extraction of test cases from pattern-hs test files is time-consuming.

**Requested Solution**: Add `extract-tests` command:

**Example Usage**:
```bash
# Extract test cases from pattern-hs test directory
gramref extract-tests ../pattern-hs/tests/ --output test_cases.json

# Extract with filtering
gramref extract-tests ../pattern-hs/tests/ --pattern "*.hs" --output test_cases.json
```

**Use Case**: Automated test case extraction from gramref reference implementation test files.

**Implementation Notes**:
- Should parse pattern-hs test files and extract test cases
- Output should conform to test-sync-format.md schema
- Should handle various test file formats
- Could be enhanced to extract from specific test frameworks

**Status**: [ ] Not Implemented

---

### 9. Enhanced Validation Mode

**Problem**: `validate` command exists but may need enhancements for better CI/CD integration.

**Requested Solution**: Enhance `validate` command with:
- Better integration with `--runner` for gram-rs
- Structured output format for CI/CD
- Exit codes that indicate specific failure types
- Progress reporting for large test suites

**Example Usage**:
```bash
# Validate with gram-rs runner
gramref validate test_suite.json --runner "cargo run --bin gram-rs" --format json

# CI/CD friendly output
gramref validate test_suite.json --runner "..." --format ci
```

**Use Case**: CI/CD integration and automated validation of gram-rs against gramref.

**Implementation Notes**:
- Should provide structured output suitable for CI/CD systems
- Exit codes should be specific (e.g., 0=success, 1=test failures, 2=runner errors, etc.)
- Progress reporting should be optional (e.g., `--progress` flag)

**Status**: [ ] Not Implemented

---

## Implementation Priority Summary

### High Priority (Blocks Testing Infrastructure)
1. ✅ Metadata exclusion (`--no-meta` or `--value-only`)
2. ✅ Test suite generation (`--type suite`)
3. ✅ Canonical JSON mode (`--canonical`)

### Medium Priority (Improves Efficiency)
4. ⚠️ Batch processing
5. ⚠️ Property test data generation
6. ⚠️ Output filtering

### Low Priority (Nice to Have)
7. ℹ️ Direct comparison command
8. ℹ️ Test case extraction
9. ℹ️ Enhanced validation mode

---

## Related Documentation

- **Test Sync Format**: `specs/002-workspace-setup/contracts/test-sync-format.md`
- **Test Utilities API**: `specs/003-test-infrastructure/contracts/test-utilities-api.md`
- **Testing Infrastructure**: `specs/003-test-infrastructure/`

---

## Notes

- All improvements should maintain backward compatibility with existing gramref CLI usage
- Flags can be combined where it makes sense (e.g., `--canonical --value-only`)
- Error handling should be consistent with current gramref behavior
- Output formats should be well-documented in the manpage

---

**Last Updated**: 2025-12-27  
**Related Feature**: 003-test-infrastructure
