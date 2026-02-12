#!/bin/bash
# Initialize git submodules for pattern-hs
# This script initializes the tree-sitter-gram submodule required for corpus tests

set -e  # Exit on error
set -u  # Exit on undefined variable

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

echo "=========================================="
echo "Initializing Git Submodules for pattern-hs"
echo "=========================================="
echo ""

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo -e "${RED}Error: Not a git repository${NC}"
    exit 1
fi

# Check if .gitmodules exists
if [ ! -f ".gitmodules" ]; then
    echo -e "${YELLOW}No .gitmodules file found. No submodules to initialize.${NC}"
    exit 0
fi

echo -e "${BLUE}Submodules configured:${NC}"
cat .gitmodules | grep "\[submodule" | sed 's/\[submodule "\(.*\)"\]/\1/'
echo ""

# Initialize submodules
echo -e "${YELLOW}Initializing submodules...${NC}"
if git submodule update --init --recursive; then
    echo ""
    echo -e "${GREEN}✓ All submodules initialized successfully${NC}"
    echo ""
    echo -e "${BLUE}Submodule locations:${NC}"
    git submodule status
    echo ""
    echo -e "${GREEN}You can now run corpus tests:${NC}"
    echo "  cabal test gram-test"
    exit 0
else
    echo ""
    echo -e "${RED}✗ Failed to initialize submodules${NC}"
    echo ""
    echo -e "${YELLOW}Troubleshooting:${NC}"
    echo "  - Ensure you have network access"
    echo "  - Check that submodule URLs are accessible"
    echo "  - Verify git credentials if using private repositories"
    exit 1
fi
