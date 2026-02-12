#!/bin/bash
# CI Check Script for pattern-hs
# Runs the same checks that CI would run locally

set -e  # Exit on error
set -u  # Exit on undefined variable

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

echo "=========================================="
echo "Running CI Checks for pattern-hs"
echo "=========================================="
echo ""

# Track overall success
FAILED=0

# Function to run a check and track failures
run_check() {
    local name="$1"
    shift
    echo -e "${YELLOW}Running: $name${NC}"
    if "$@"; then
        echo -e "${GREEN}✓ $name passed${NC}"
        echo ""
        return 0
    else
        echo -e "${RED}✗ $name failed${NC}"
        echo ""
        FAILED=1
        return 1
    fi
}

# 1. Build all packages
run_check "Build all packages" cabal build all

# 2. Run all tests
run_check "Run all tests" cabal test all --test-show-details=direct

# 3. Build documentation
run_check "Build Haddock documentation" cabal haddock all

# Summary
echo "=========================================="
if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All CI checks passed!${NC}"
    exit 0
else
    echo -e "${RED}Some CI checks failed${NC}"
    exit 1
fi
