#!/bin/bash

# Setup script for Trade War Model

echo "Setting up Trade War Model..."

# Check if cabal is installed
if ! command -v cabal &> /dev/null; then
    echo "Error: cabal is not installed. Please install the Haskell Platform or GHCup."
    exit 1
fi

# Check if GHC is installed
if ! command -v ghc &> /dev/null; then
    echo "Error: GHC is not installed. Please install the Haskell Platform or GHCup."
    exit 1
fi

# Update cabal package list
echo "Updating cabal package list..."
cabal update

# Initialize the project
echo "Initializing the project..."
cabal build

echo "Setup complete! You can now run the trade war model with:"
echo "cabal run trade-war-sim"
echo ""
echo "To run tests, use:"
echo "cabal test" 